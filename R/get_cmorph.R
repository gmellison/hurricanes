library(dplyr)
library(lubridate)

hurdat <- read.csv("data/hurdat2/hurdat2_since1980_cleaned.csv")

# add a radius to each hurdat measurement (in nautical miles)
wind_34_cols <- c("wind_34_ne", "wind_34_se", "wind_34_se", "wind_34_sw")

# max of the 4 quadrant wind speed radii
hurdat$radius <- apply(hurdat, 1, function(x) max(x[wind_34_cols]))

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

cmorph_data <- hurdat %>% 
  group_by(h_id, h_name) %>% 
  summarize(lat_min = min(degrees_north - (1/8)*radius),
            lat_max = max(degrees_north + (1/8)*radius),
            lon_min = max(degrees_west + (1/8)*radius),    ## switch "max"/"min" longitudes, since we want them read west to east
            lon_max = min(degrees_west - (1/8)*radius),
            time_min = min(datetime),
            time_max = max(datetime)) %>% 
  arrange(time_min)

# convert lat lon back to the format the hurdat query wants:
cmorph_data$lat_min_q <- sprintf("%s%s", round(cmorph_data$lat_min, 1), "N")
cmorph_data$lat_max_q <- sprintf("%s%s", round(cmorph_data$lat_max, 1), "N")
cmorph_data$lon_min_q <- sprintf("%s%s", round(cmorph_data$lon_min, 1), "W")
cmorph_data$lon_max_q <- sprintf("%s%s", round(cmorph_data$lon_max, 1), "W")

cmorph_query <- filter(cmorph_data, year(time_min) > 2002) %>% 
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


for (i in 1:3) {
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
                  paste0(rep("-", floor(nrow(cmorph_data)/4)), collapse="")))
  }
  
  
  sleepy_time <- runif(1, 20, 40)
  print(sprintf("sleeping for %s seconds, zzzz", sleepy_time))
  Sys.sleep(sleepy_time)
}


#http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CMORPH/.3-hourly/.mean/.morphed/.cmorph/X/(32.3W)/(48.5W)/RANGEEDGES/T/(0000-0300 18 Jul 2006)/(2100-2400 22 Jul 2006)/RANGEEDGES/Y/(-73.7N)/(-56.5N)/RANGEEDGES/gridtable.tsv
#http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CMORPH/.3-hourly/.mean/.morphed/.cmorph/X/(18)/(Jul)/RANGEEDGES/T/(0000-0300 :g:g2006 32.3Wv 48.5W)/(2100-2400 22 Jul 2006)/RANGEEDGES/Y/(-73.7N)/(-56.5N)/RANGEEDGES/gridtable.tsv
#http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CMORPH/.3-hourly/.mean/.morphed/.cmorph/X/(18)/(Jul)/RANGEEDGES/T/(0000-0300 2006 32.3W 48.5W)/(2100-2400 22 Jul 2006)/RANGEEDGES/Y/(-73.7N)/(-56.5N)/RANGEEDGES/gridtable.tsv