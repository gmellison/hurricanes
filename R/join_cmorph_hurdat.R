library(dplyr)
library(lubridate)
library(stringr)

## now loop through the downloads and join the rainfall data to hurdat
precip_df <- bind_rows(lapply(list.files("data/cmorph_landfalls/"), function(cmorph_file) {
  cmorph_data <- read.delim(sprintf("data/cmorph_landfalls/%s", cmorph_file), 
                          header = TRUE, skip=1, stringsAsFactors = FALSE)
  
  h_name <- str_split_fixed(cmorph_file, "[_]", n = 4)[2]
  h_id <- str_split_fixed(cmorph_file, "[_]", n = 4)[3]
  landfall_date_string <- str_split_fixed(cmorph_file, "[_.]", n = 5)[4] 
  landfall_date <- ymd_hms(
    ifelse(str_count(landfall_date_string) == 14, 
           landfall_date_string, 
           sprintf("%s000000",landfall_date_string)))

  rainfall <- sum(cmorph_data$mm.hr) * 3
  
  df <- data.frame(h_name=h_name,h_id=h_id,datetime=landfall_date,precip=rainfall)
  return(df)
  }
))

hurdat <- read.csv("data/hurdat2/hurdat2_join.csv",stringsAsFactors = FALSE)
hurdat$datetime <- ymd_hms(hurdat$datetime)
hurdat_with_rainfall <- left_join(hurdat, precip_df)

hurdat_output <- select(hurdat_with_rainfall,
                        h_name, h_id, datetime, hours, id, status, lat, lon, 
                        windspeed_max, pressure_min, precip)

write.csv(hurdat_output, "data/hurdat2/hurdat_with_precip.csv")
