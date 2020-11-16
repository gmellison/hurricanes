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
point_maxima <- lapply(1:nrow(df), function(i) {
  row <- df[i, ]
  name <- row$Name
  # Name contains both storm name and dates, extract them both:
  h_name <- str_to_lower(str_split_fixed(name, " ", 3)[1])
  
  h_date <- str_split_fixed(name, "[()]", 4)[2]
  h_month <- str_split_fixed(h_date, " ", 2)[1]
  h_year <- as.numeric(str_split_fixed(h_date, " ", 2)[2])
  
  # multiple ovservations are split by '\n'
  amounts <- unlist(stringr::str_split(row$Amount, "[\n]"))
  n_amounts <- length(amounts)
  locations <- stringr::str_split_fixed(row$Location, "[\n]", n_amounts)[1,]
  
  row_df <- data.frame(name = h_name,
                       month = h_month,
                       year = h_year,
                       amount = as.numeric(amounts),
                       location = locations)
  row_df
}) %>% bind_rows

write.csv(joined, "data/rainfall_point_maxima.csv")

# get hurdat, and add an index for sequential landfalls
hurdat <- read.csv("data/hurdat2/hurdat_with_precip.csv", stringsAsFactors = FALSE)
hurdat <- hurdat %>% 
  arrange(h_name, datetime) %>% 
  group_by(h_name, h_id) %>% 
  mutate(lf_idx = row_number(),
         year = year(datetime),
         name_lower = str_to_lower(h_name))

# add the same index to the point maxima data:
point_maxima <- point_maxima %>% 
  group_by(name, year) %>% 
  mutate(lf_idx = row_number())

joined <- left_join(hurdat, select(point_maxima, name, year, lf_idx, amount, location),
                    by = c("name_lower" = "name", "year" = "year", "lf_idx" = "lf_idx")) %>% 
  arrange(datetime,lf_idx)

joined$year <- NULL
joined$X <- NULL
joined$name_lower <- NULL

names(joined)[names(joined) == "amount"] <- "point_max"
head(joined)
joined$point_max_mml <- joined$point_max * 25.4

write.csv(joined, "data/hurdat2/hurdat2_landfall_with_point_maxima.csv")
