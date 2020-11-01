library(dplyr)
library(lubridate)

hurdat <- read.csv("data/hurdat2/hurdat2_since1980_cleaned.csv", stringsAsFactors = FALSE)

# add a radius to each hurdat measurement (in nautical miles)
wind_34_cols <- c("wind_34_ne", "wind_34_nw", "wind_34_se", "wind_34_sw")

# max of the 4 quadrant wind speed radii
hurdat$radius <- apply(hurdat, 1, function(x) as.numeric(max(x[wind_34_cols])))

# missing values are -999, so replace with 0
hurdat$radius <- ifelse(hurdat$radius > 0 , hurdat$radius, 0)

# lat/lon in format "12.3W" <- convert this to degrees west/n whatever.
hurdat$degrees_north <- sapply(hurdat$lat, function(l) {
  l <- stringr::str_trim(l) 
  l_num <- as.numeric(stringr::str_extract(l, "^[0-9.]*"))
  l_dir <- stringr::str_extract(l, "[NS]")
  if (l_dir == "S") l_num <- -1 * l_num
  l_num
}) 

hurdat$degrees_west <- sapply(hurdat$lon, function(l) {
  l <- stringr::str_trim(l) 
  l_num <- as.numeric(stringr::str_extract(l, "^[0-9.]*"))
  l_dir <- stringr::str_extract(l, "[WE]")
  if (l_dir == "E") l_num <- -1 * l_num
  l_num
}) 

# also let's make the date/times a little nicer:
hurdat$datetime <- lubridate::ymd(hurdat$date) + lubridate::hours(as.integer(hurdat$hours/100))

## now get data ready to query for cmorph:
#    we want max/min time and max/min lat and lon for each storm.

cmorph_summ <- hurdat %>% 
  group_by(h_id, h_name) %>% 
  summarize(lat_min = min(degrees_north - (0.1) * (1/5) * radius),
            lat_max = max(degrees_north + (0.1) * (1/5) * radius),
            lon_min = max(degrees_west + (0.1) * (1/5) * radius),    ## switch "max"/"min" longitudes, since we want them read west to east
            lon_max = min(degrees_west - (0.1) * (1/5) * radius),
            time_min = min(datetime),
            time_max = max(datetime)) %>% 
  arrange(time_min)

# convert lat lon back to the format the hurdat query wants:
cmorph_summ$lat_min_q <- sprintf("%s%s", round(cmorph_data$lat_min, 1), "N")
cmorph_summ$lat_max_q <- sprintf("%s%s", round(cmorph_data$lat_max, 1), "N")
cmorph_summ$lon_min_q <- sprintf("%s%s", round(cmorph_data$lon_min, 1), "W")
cmorph_summ$lon_max_q <- sprintf("%s%s", round(cmorph_data$lon_max, 1), "W")

cmorph_query <- filter(cmorph_summ, year(time_min) > 2002) %>% 
  mutate( 
       query = sprintf("http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CMORPH/.3-hourly/.mean/.morphed/.cmorph/X/(%s)/(%s)/RANGEEDGES/Y/(%s)/(%s)/RANGEEDGES/T/(0000-0300\ %s\ %s\ %s)/(2100-2400\ %s\ %s\ %s)/RANGEEDGES/gridtable.tsv", 
       lon_max_q, lon_min_q, 
       lat_min_q, lat_max_q,
       day(time_min), month(time_min, label=TRUE), year(time_min), 
       day(time_max), month(time_max, label=TRUE), year(time_max)))
cmorph_query$query <- stringr::str_replace_all(cmorph_query$query, " ", "%20")
cmorph_query$query <- stringr::str_replace_all(cmorph_query$query, "[(]", "%28")
cmorph_query$query <- stringr::str_replace_all(cmorph_query$query, "[)]", "%29")
      
queries <- cmorph_query %>% '$'(query)
q <- queries[1]
q


for (i in 1:length(queries)) {
  file_name <- sprintf("data/cmorph/cmorph_%s_%s.txt", 
                       cmorph_query$h_name[i],
                       cmorph_query$h_id[i])
  
  if (file.exists(file_name)) {
    next
  }
  
  curl::curl_download(url=cmorph_query$query[i], file_name)
  
  if ( i %% 20 == 0) {
    print(sprintf("done with %s queries out of %s \n", i, nrow(cmorph_query)))
    print(sprintf("%s%s", 
                  paste0(rep("o", floor(i/4)),collapse = ""), 
                  paste0(rep("-", floor(nrow(cmorph_data)/4) - floor(i/4)), collapse="")))
  }
  
  
  sleepy_time <- runif(1, 4, 9)
  print(sprintf("sleeping for %s seconds, zzzz", sleepy_time))
  Sys.sleep(sleepy_time)
}


## now loop through the downloads and join the rainfall data to hurdat
for (cmorph_file in list.files("data/cmorph/")) {
  cmorph_data <- read.delim(sprintf("data/cmorph/%s", cmorph_file), 
                          header = TRUE, skip=1, stringsAsFactors = FALSE)
  
  h_name <- stringr::str_split_fixed(cmorph_file, "[_.]", n = 4)[2]
  h_id <- stringr::str_split_fixed(cmorph_file, "[_.]", n = 4)[3]
  
  if (file.exists(sprintf("data/cmorph/rainfall_%s", h_id))) {
    next
  }
  
  cmorph_data$degree_east_c <- round(cmorph_data$degree_east, 1)
  cmorph_data$degree_north_c <- round(cmorph_data$degree_north, 1)
 
  # time is recorded in hours since inception of cmorph data, so convert to a nice datetime
  cmorph_data$datetime <- as_datetime("2002.12.07.00.00.00") + minutes(round(cmorph_data$days.since.2002.12.07.00.00.00 * 24 * 60, 0))
  minute(cmorph_data$datetime) <- 0
  
  # now bucket into same 6hr chunks hurdat uses:
  hour(cmorph_data$datetime) <- case_when(
    hour(cmorph_data$datetime) < 6 ~ 0,
    hour(cmorph_data$datetime)  < 12 ~ 6,
    hour(cmorph_data$datetime)  < 18 ~ 12,
    hour(cmorph_data$datetime) < 24 ~ 18)
  
  cmorph_data <- cmorph_data %>% 
    group_by(degree_east_c, degree_north_c, datetime) %>% 
    summarise(precip_mm = sum(mm.hr * 3)) %>% 
    ungroup() %>% 
    mutate(degree_west_c = -degree_east_c) %>% 
    select(degree_west_c,degree_north_c,precip_mm,datetime)
 
  hurdat_locs <- select(hurdat, c("h_name", "h_id", "datetime", "degrees_north", "degrees_west", "radius")) %>% 
    filter(h_name == h_name, h_id == h_id)
  
  joined <- left_join(cmorph_data, hurdat_locs, on=c(h_name, h_id, datetime)) 
  joined$radius <- ifelse(joined$radius <= 10, 10, joined$radius)
  joined$in_storm  = with(joined, (degree_west_c >= degrees_west - (0.1) * (1/5) * radius) &
                      (degree_west_c <= degrees_west + (0.1) * (1/5) * radius) &
                      (degree_north_c >= degrees_north - (0.1) * (1/5) *radius) &
                      (degree_north_c <= degrees_north + (0.1) * (1/5)*radius))
  
  h_with_cmorph_rain <- joined %>% 
    filter(in_storm) %>% 
    group_by(h_name, h_id, datetime) %>% 
    summarise(rainfall = sum(precip_mm))

  write.csv(h_with_cmorph_rain, sprintf("data/cmorph/rainfall_%s", h_id))
}


hurdat_with_rainfall <- lapply(unique(hurdat$h_id), function(idx) {
  filename <- sprintf("data/cmorph/rainfall_%s", idx)
  
  if (!file.exists(filename)) return(NULL)
  rainfall <- read.csv(filename, header=TRUE, stringsAsFactors = FALSE)
  rainfall$datetime <- as.Date(rainfall$datetime)
    
  hurdat_joined_rainfall <- hurdat %>%
    filter(h_id == idx) %>% 
    left_join(select(rainfall, c(h_id, rainfall, datetime)), on=c(h_id, datetime)) %>% 
    select(h_name, h_id, datetime, lat, lon, radius, status, id, windspeed_max, 
           pressure_min, wind_34_ne, wind_34_se, wind_34_sw, wind_34_nw, 
           wind_50_ne, wind_50_se, wind_50_sw, wind_50_nw,
           wind_64_ne, wind_64_se,wind_64_sw, wind_64_nw, rainfall)
  return(hurdat_joined_rainfall)
  
}) %>% bind_rows()

ids_no_rainfall <- unique(hurdat$h_id)[!unique(hurdat$h_id) %in% unique(hurdat_with_rainfall$h_id)]
hurdat_all <- hurdat %>% 
  filter(h_id %in% ids_no_rainfall) %>% 
  select(h_name, h_id, datetime, lat, lon, radius, status, id, windspeed_max, 
         pressure_min, wind_34_ne, wind_34_se, wind_34_sw, wind_34_nw, 
         wind_50_ne, wind_50_se, wind_50_sw, wind_50_nw,
         wind_64_ne, wind_64_se,wind_64_sw, wind_64_nw) %>% 
  mutate(rainfall = -1) %>% 
  bind_rows(hurdat_with_rainfall) %>% 
  arrange(desc(h_id))
hurdat_all$rainfall <- round(hurdat_all$rainfall, 3)
write.csv(hurdat_all, "data/hurdat2/hurdat_with_precip.csv")

hurdat_daily <- hurdat_all
hurdat_daily$datetime <- as_date(hurdat_daily$datetime)

hurdat_daily <- hurdat_daily %>% 
  group_by(h_name, h_id, datetime) %>% 
  summarise(lat = min(lat), lon = min(lon), radius = max(radius), windspeed_max = max(windspeed_max),
            pressure_min = min(pressure_min), 
            wind_34_ne = max(wind_34_ne), wind_34_se = max(wind_34_se),
            wind_34_sw = max(wind_34_sw), wind_34_sw = max(wind_34_nw), 
            wind_50_ne = max(wind_50_ne), wind_50_se = max(wind_50_ne), 
            wind_50_sw = max(wind_50_sw), wind_50_nw = max(wind_50_nw),
            wind_64_ne = max(wind_64_ne), wind_64_se = max(wind_64_ne),
            wind_64_sw = max(wind_64_ne), wind_64_nw = max(wind_64_nw),
            rainfall = sum(rainfall))
write.csv(hurdat_daily, "data/hurdat2/hurdat_with_precip_daily.csv")

#http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CMORPH/.3-hourly/.mean/.morphed/.cmorph/X/(32.3W)/(48.5W)/RANGEEDGES/T/(0000-0300 18 Jul 2006)/(2100-2400 22 Jul 2006)/RANGEEDGES/Y/(-73.7N)/(-56.5N)/RANGEEDGES/gridtable.tsv
#http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CMORPH/.3-hourly/.mean/.morphed/.cmorph/X/(18)/(Jul)/RANGEEDGES/T/(0000-0300 :g:g2006 32.3Wv 48.5W)/(2100-2400 22 Jul 2006)/RANGEEDGES/Y/(-73.7N)/(-56.5N)/RANGEEDGES/gridtable.tsv
#http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CMORPH/.3-hourly/.mean/.morphed/.cmorph/X/(18)/(Jul)/RANGEEDGES/T/(0000-0300 2006 32.3W 48.5W)/(2100-2400 22 Jul 2006)/RANGEEDGES/Y/(-73.7N)/(-56.5N)/RANGEEDGES/gridtable.tsv