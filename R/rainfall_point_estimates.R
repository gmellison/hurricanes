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
  h_name <- str_replace_all(h_name, "\n", "")
  
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

#### Want to add the point maxima for storms between 1980 and 2002ish

point_maxima <- point_maxima[as.numeric(point_maxima$year) < 2002, ]
point_maxima <- filter(point_maxima, name != "unnamed")
point_maxima <- na.omit(point_maxima)
head(point_maxima, 10)

# add the same index to the point maxima data:
point_maxima <- point_maxima %>% 
  group_by(name, year) %>% 
  mutate(lf_idx = row_number())


# get all data, and add an index to join on
h_data <- read.csv("data/AllData.csv", stringsAsFactors = FALSE)
h_data$X <- NULL
cols_out <- names(h_data)

h_data <- h_data %>% 
  mutate(datetime = ymd(Time),
         name_lower = str_to_lower(Name)) %>% 
  mutate(year = year(datetime)) %>% 
  arrange(name_lower) %>% 
  group_by(name_lower) %>% 
  mutate(lf_idx = row_number()) %>%
  ungroup()

joined <- left_join(h_data, select(point_maxima, name, year, lf_idx, amount, location),
                    by = c("name_lower" = "name", "year" = "year", "lf_idx" = "lf_idx")) %>% 
  arrange(datetime, lf_idx)

joined$point_max_mm <- joined$amount * 25.4
joined$Rainfall <- ifelse(is.na(joined$Rainfall) & !is.na(joined$point_max_mm), 
                          joined$point_max_mm,
                          joined$Rainfall)

joined <- select(joined, cols_out)
write.csv(joined, "data/AllData2.csv")
