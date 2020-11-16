library(dplyr)
library(lubridate)
library(stringr)

hurdat <- read.csv("data/hurdat2/hurdat2_since1980_cleaned.csv", stringsAsFactors = FALSE)

## first, filter to just landfalls since that's really what we care about:
hurdat <- hurdat %>% filter(id == "L")

# add a radius to each hurdat measurement (in nautical miles)
wind_34_cols_n <- c("wind_34_ne", "wind_34_nw")
wind_34_cols_e <- c("wind_34_ne", "wind_34_se")
wind_34_cols_s <- c("wind_34_se", "wind_34_sw")
wind_34_cols_w <- c("wind_34_nw", "wind_34_sw")

# max of the 4 quadrant wind speed radii
hurdat$radius_n <- apply(hurdat, 1, function(x) as.numeric(max(x[wind_34_cols_n])))
hurdat$radius_e <- apply(hurdat, 1, function(x) as.numeric(max(x[wind_34_cols_e])))
hurdat$radius_s <- apply(hurdat, 1, function(x) as.numeric(max(x[wind_34_cols_s])))
hurdat$radius_w <- apply(hurdat, 1, function(x) as.numeric(max(x[wind_34_cols_w])))

# missing values are -999, so replace with 0
hurdat$radius_n <- ifelse(hurdat$radius_n > 0 , hurdat$radius_n, 0)
hurdat$radius_e <- ifelse(hurdat$radius_e > 0 , hurdat$radius_e, 0)
hurdat$radius_s <- ifelse(hurdat$radius_s > 0 , hurdat$radius_s, 0)
hurdat$radius_w <- ifelse(hurdat$radius_w > 0 , hurdat$radius_w, 0)

# lat/lon in format "12.3W" <- convert this to degree east/degree north.
hurdat$degree_north <- sapply(hurdat$lat, function(l) {
  l <- stringr::str_trim(l) 
  l_num <- as.numeric(stringr::str_extract(l, "^[0-9.]*"))
  l_dir <- stringr::str_extract(l, "[NS]")
  if (l_dir == "S") l_num <- -1 * l_num
  l_num
}) 

hurdat$degree_east <- sapply(hurdat$lon, function(l) {
  l <- stringr::str_trim(l) 
  l_num <- as.numeric(stringr::str_extract(l, "^[0-9.]*"))
  l_dir <- stringr::str_extract(l, "[WE]")
  if (l_dir == "W") l_num <- -1 * l_num
  l_num
}) 

# make a nice datetime column:
hurdat$datetime <- lubridate::ymd(hurdat$date) + lubridate::hours(as.integer(hurdat$hours/100))
write.csv(hurdat, "data/hurdat2/hurdat2_join.csv")

## now get data ready to query for cmorph:
# the lat/lon mins and maxes are based on using a constant approx 20 nautical mile radius
query_prep <- hurdat %>% 
  group_by(h_name, h_id, datetime) %>% 
  summarise(lat_min = degree_north - (0.1) * (1/5) * 5 , # min(radius_s, 5),
            lat_max = degree_north + (0.1) * (1/5) * 5, # min(radius_n, 5),
            lon_min = degree_east - (0.1) * (1/5) * 5, # min(radius_w, 5),    ## switch "max"/"min" longitudes, since we want them read west to east
            lon_max = degree_east + (0.1) * (1/5) * 5, # min(radius_e, 5),
            time_q_start = datetime - hours((hour(datetime) %% 3)) - hours(12),
            time_q_end = datetime - hours((hour(datetime) %% 3)) + hours(12)) %>% 
  arrange(time_q_start)

# convert lat lon back to the format the cmorph query wants:
query_prep$lat_min_q <- sprintf("%s%s", 
                                ifelse(query_prep$lat_min == query_prep$lat_max, query_prep$lat_min - 0.125, query_prep$lat_min), 
                                       "N")
query_prep$lat_max_q <- sprintf("%s%s", 
                                ifelse(query_prep$lat_min == query_prep$lat_max, query_prep$lat_max + 0.125, query_prep$lat_max), 
                                "N")
query_prep$lon_min_q <- sprintf("%s%s", 
                                ifelse(query_prep$lon_min == query_prep$lon_max, query_prep$lon_min - 0.125, query_prep$lon_min),
                                "E")
query_prep$lon_max_q <- sprintf("%s%s",
                               ifelse(query_prep$lon_min == query_prep$lon_max, query_prep$lon_max + 0.125, query_prep$lon_max),
                                "E")


##
## "http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CMORPH/.3-hourly/.mean/.morphed/.cmorph/X/(%s)/(%s)/RANGEEDGES/Y/(%s)/(%s)/RANGEEDGES/T/(0000-0300\ %s\ %s\ %s)/(2100-2400\ %s\ %s\ %s)/RANGEEDGES/gridtable.tsv"
##
# cmorph only exists since Dec. 2002
query_df <- filter(query_prep, year(time_q_start) == 2002 & month(time_q_start) == 12 | year(time_q_start) >= 2003) %>% 
  mutate( 
       query = sprintf("http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CMORPH/.3-hourly/.mean/.morphed/.cmorph/X/(%s)/(%s)/RANGEEDGES/Y/(%s)/(%s)/RANGEEDGES/T/(%02d00-%02d00\ %s\ %s\ %s)/(%02d00-%02d00\ %s\ %s\ %s)/RANGEEDGES/gridtable.tsv", 
       lon_min_q, lon_max_q, ### all these are just the arguments for the string replacement above
       lat_min_q, lat_max_q,
       hour(time_q_start), hour(time_q_start)+3, day(time_q_start), month(time_q_start, label=TRUE), year(time_q_start),  
       hour(time_q_end), hour(time_q_end)+3, day(time_q_end), month(time_q_end, label=TRUE), year(time_q_end)))

# need to fiddle with some special characters (space, parentheses)):
query_df$query <- str_replace_all(query_df$query, " ", "%20")
query_df$query <- str_replace_all(query_df$query, "[(]", "%28")
query_df$query <- str_replace_all(query_df$query, "[)]", "%29")

# vector of the queries to loop through:
queries <- query_df$query
for (i in 1:length(queries)) {
  # one query/file per landfall row.
  # filename will contain storm name, id, and landfall datetime.
  file_name <- sprintf("data/cmorph_landfalls2/cmorph_%s_%s_%s.txt", 
                       query_df$h_name[i],
                       query_df$h_id[i], 
                       str_replace_all(query_df$datetime[i], "[-_: ]", ""))
 
  # skip if file exists already 
  if (file.exists(file_name)) {
    next
  }
 
  # finally query the actual data, curl_download will save to disk
  curl::curl_download(url=queries[i], file_name)
  
  if ( i %% 5 == 0) { # every 20 queries print a little progress bar
    print(sprintf("done with %s queries out of %s \n", i, nrow(query_df)))
    print(sprintf("%s%s", 
                  paste0(rep("o", floor(i/4)),collapse = ""), 
                  paste0(rep("-", floor(nrow(query_df)/4) - floor(i/4)), collapse="")))
  }
  
  # be nice to their server
  sleepy_time <- runif(1, 2, 4)
  print(sprintf("sleeping for %s seconds, zzzz", sleepy_time))
  Sys.sleep(sleepy_time)
}

#http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CMORPH/.3-hourly/.mean/.morphed/.cmorph/X/(32.3W)/(48.5W)/RANGEEDGES/T/(0000-0300 18 Jul 2006)/(2100-2400 22 Jul 2006)/RANGEEDGES/Y/(-73.7N)/(-56.5N)/RANGEEDGES/gridtable.tsv
#http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CMORPH/.3-hourly/.mean/.morphed/.cmorph/X/(18)/(Jul)/RANGEEDGES/T/(0000-0300 :g:g2006 32.3Wv 48.5W)/(2100-2400 22 Jul 2006)/RANGEEDGES/Y/(-73.7N)/(-56.5N)/RANGEEDGES/gridtable.tsv
#http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.CMORPH/.3-hourly/.mean/.morphed/.cmorph/X/(18)/(Jul)/RANGEEDGES/T/(0000-0300 2006 32.3W 48.5W)/(2100-2400 22 Jul 2006)/RANGEEDGES/Y/(-73.7N)/(-56.5N)/RANGEEDGES/gridtable.tsv