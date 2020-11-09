library(rvest)
library(dplyr)
library(stringr)
library(lubridate)

rainfall_max <- xml2::read_html("https://www.wpc.ncep.noaa.gov/tropical/rain/tcmaxima.html")
rainfall_table <- rvest::html_table(rainfall_max)

df <- rainfall_table[[1]]
names(df) <- df[1, ]
df <- df[-1, ]

# need to clean the datafrome a little
df_out <- lapply(1:nrow(df), function(i) {
  row <- df[i, ]
  name <- row$Name
  # Name contains both storm name and dates, extract them both:
  h_name <- stringr::str_split_fixed(name, " ", 3)[1]
  h_date <- stringr::str_split_fixed(name, "[()]", 4)[2]
  h_month <- stringr::str_split_fixed(h_date, " ", 2)[1]
  h_year <- stringr::str_split_fixed(h_date, " ", 2)[2]
  
  # multiple ovservations are split by '\n'
  amounts <- unlist(stringr::str_split(row$Amount, "[\n]"))
  n_amounts <- length(amounts)
  locations <- stringr::str_split_fixed(row$Location, "[\n]", n_amounts)[1,]
  
  print(i)
  row_df <- data.frame(name = h_name,
                       month = h_month,
                       year = h_year,
                       amount = as.numeric(amounts),
                       location = locations)
  row_df
}) %>% bind_rows

df_out$name <- str_to_lower(df_out$name)
write.csv(joined, "data/rainfall_point_maxima.csv")

hurdat_landfall <- read.csv("data/hurdat2/hurdat_landfalls.csv", stringsAsFactors = FALSE)

hurdat_landfall$year <- as.character(year(hurdat_landfall$datetime))
hurdat_landfall$name_lower <- str_to_lower(hurdat_landfall$h_name)

joined <- left_join(hurdat_landfall, select(df_out, name, year, amount, location),
                    by = c("name_lower" = "name", "year" = "year"))
joined$name_lower <- NULL                    
names(joined)[names(joined) == "amount"] <- "point_max"
head(joined)

write.csv(joined, "data/hurdat2/hurdat2_landfall_with_point_maxima.csv")
