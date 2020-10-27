### data schema
schema <- c(
            "date",
            "hours",
            "id",           ## id indicates intensity peak, landfall, etc
            "status",       ## status indicates hurricane, depression, cyclone, etc
            "lat",
            "lon",
            "windspeed_max",
            "pressure_min",
            "wind_34_ne",
            "wind_34_se",
            "wind_34_sw",
            "wind_34_nw",
            "wind_50_ne",
            "wind_50_se",
            "wind_50_sw",
            "wind_50_nw",
            "wind_64_ne",
            "wind_64_se",
            "wind_64_sw",
            "wind_64_nw")
data <- read.table("data/hurdat2/hurdat2_1980_2020.txt", 
                   col.names=schema, 
                   fill=TRUE)
# get rid of commas
data <- data.frame(apply(data, 2, function(x) stringr::str_remove(x, ",")), stringsAsFactors = FALSE)
head(data)

### pull out all the rows that contain "id", "name", "nrow" 
#       they start the blocks of data for each storm, 
#       so use them to prep the data.
#
info_rows <- data[stringr::str_detect(data$date, "^A"),1:3]
names(info_rows) <- c("h_id", "h_name", "n_rows")
info_rows$n_rows <- as.numeric(info_rows$n_rows)
info_rows$h_name <- as.character(info_rows$h_name)
info_rows$h_id <- as.character(info_rows$h_id)

# init a list to store all the hurricane data in
h_list <- vector(nrow(info_rows), mode="list")
names(h_list) <- info_rows$h_id 

# loop through all the info rows
for (i in 1:nrow(info_rows)) {
  # get the indices for each storm:
  #   from start = info row + 1 to end = start + (nrow-1)
  i_start <- which(data$date == info_rows$h_id[i]) + 1
  i_end <- i_start + info_rows$n_rows[i]-1 
  
  # grab the rows, and add cols for name and id
  h_df <- data[i_start:i_end, ]
  h_df$h_name <- info_rows$h_name[i]
  h_df$h_id <- info_rows$h_id[i] 
 
  # store it in the proper place in the list (indexed on hurricane id -- name not unique) 
  h_list[[info_rows$h_id[i]]] <- h_df
}

# squish all the list dfs into one big df
hurricane_df <- dplyr::bind_rows(h_list)
# save the data
write.csv(hurricane_df, file="data/hurdat2/hurdat2_since1980_cleaned.csv")

