library(dplyr)
library(lubridate)
library(stringr)

data <- read.csv("data/AllData.csv")
data$datetime <- ymd(data$Time)

query_prep <- data %>% 
  filter(year(datetime) >= 2003) %>% 
  group_by(Name,datetime) %>% 
  summarise(lat_min = Latitude - (0.175) , 
            lat_max = Latitude + (0.175) , 
            lon_min = Longitude - (0.175) ,
            lon_max = Longitude + (0.175) ,
            time_q_start = datetime,
            time_q_end = datetime + days(5)) %>% 
  arrange(time_q_start)

query_prep$lat_min_q <- sprintf("%s%s", 
                                ifelse(query_prep$lat_min == query_prep$lat_max, query_prep$lat_min - 0.25, query_prep$lat_min), 
                                       "N")
query_prep$lat_max_q <- sprintf("%s%s", 
                                ifelse(query_prep$lat_min == query_prep$lat_max, query_prep$lat_max + 0.25, query_prep$lat_max), 
                                "N")
query_prep$lon_min_q <- sprintf("%s%s", 
                                ifelse(query_prep$lon_min == query_prep$lon_max, query_prep$lon_min - 0.25, query_prep$lon_min),
                                "E")
query_prep$lon_max_q <- sprintf("%s%s",
                               ifelse(query_prep$lon_min == query_prep$lon_max, query_prep$lon_max + 0.25, query_prep$lon_max),
                                "E")

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

query_df <- query_df %>% 
  arrange(Name, datetime)

# vector of the queries to loop through:
queries <- query_df$query
for (i in 1:length(queries)) {
  # one query/file per landfall row.
  # filename will contain storm name, id, and landfall datetime.
  file_name <- sprintf("data/cmorph_data_all/cmorph_%s_%s_%s_%s.txt", 
                       query_df$Name[i],
                       str_replace_all(query_df$datetime[i], "[-_: ]", ""),
                       str_replace_all(query_df$lat_min[i] + 0.175, "[.-]", ""),
                       str_replace_all(query_df$lon_min[i] + 0.175, "[.-]", ""))
 
  # skip if file exists already 
  if (file.exists(file_name)) {
    next
  }
  
  # finally query the actual data, curl_download will save to disk
  curl::curl_download(url=queries[i], file_name)
  
  if (file.size(file_name) == 296) {
    file.remove(file_name)
  }
  
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


### now join back to all data, add 5 days cumulative rainfall:
## now loop through the downloads and join the rainfall data to hurdat
precip_df <- bind_rows(lapply(list.files("data/cmorph_data_all/"), function(cmorph_file) {
  cmorph_data <- read.delim(sprintf("data/cmorph_data_all/%s", cmorph_file), 
                          header = TRUE, skip=1, stringsAsFactors = FALSE)
  
  Name <- str_split_fixed(cmorph_file, "[_]", n = 4)[2]
  landfall_date_string <- str_split_fixed(cmorph_file, "[_.]", n = 4)[3] 
  lat <- as.numeric(str_split_fixed(cmorph_file, "[_.]", n = 6)[4])/10
  lon <- as.numeric(str_split_fixed(cmorph_file, "[_.]", n = 6)[5])/10
  
  landfall_date <- ymd_hms(
    ifelse(str_count(landfall_date_string) == 14, 
           landfall_date_string, 
           sprintf("%s000000",landfall_date_string)))

  cmorph_data$rainfall_day <- floor(cmorph_data$days.since.2002.12.07.00.00.00 - 
    min(cmorph_data$days.since.2002.12.07.00.00.00))
  
  rainfall_df <- cmorph_data %>% 
    group_by(rainfall_day) %>% 
    summarise(Rainfall = sum(mm.hr) * 3) %>% 
    mutate(RainfallCumulative = cumsum(Rainfall)) %>% 
    tidyr::pivot_wider(c(rainfall_day, RainfallCumulative), 
                       names_from = rainfall_day,
                       names_prefix = "RainfallCumulativeCMORPHDay",
                       values_from = RainfallCumulative)
  
  rainfall_df$Name <- Name
  rainfall_df$Time <- landfall_date
  rainfall_df$lat_abs <- lat
  rainfall_df$lon_abs <- lon
  return(rainfall_df)
  }
))

precip_df$Time <- as.character(precip_df$Time)
data_all <- read.csv("data/AllData2.csv")
data_all$lat_abs <- abs(data_all$Latitude)
data_all$lon_abs <- abs(data_all$Longitude)

data <- data_all %>% 
  left_join(precip_df, by = c("Name", "Time", "lat_abs", "lon_abs")) %>% 
  group_by(Name) %>% 
  mutate(n_years = length(unique(year(ymd(Time))))) %>% 
  ungroup() %>% 
  mutate(Name = ifelse(n_years == 1, Name, sprintf("%s%s", Name, year(ymd(Time))))) %>% 
  select(Name, Time, Latitude, Longitude, MaxWindSpeed, CentPressure, Rainfall, Surge,
         RainfallCumulativeCMORPHDay0,RainfallCumulativeCMORPHDay1,RainfallCumulativeCMORPHDay2,
         RainfallCumulativeCMORPHDay3,RainfallCumulativeCMORPHDay4)
head(arrange(data, Name))
write.csv(data, "data/AllData.csv")
            