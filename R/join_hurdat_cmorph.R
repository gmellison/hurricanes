library(stringr)
library(dplyr)

h <- read.csv("data/hurdat2/test.txt", skip=1, header = FALSE, stringsAsFactors = FALSE)
head(h)
table(h$V5)
table(h$V6)
p <- read.delim("data/rainfall/cmorph_grid.tsv", header = TRUE, skip=1, stringsAsFactors = FALSE)

names(h)[c(1,2,5,6)] <- c("date", "time",  "lat", "lon")

head(h)
head(p)

## make the hurdat lat/lons nicer
h$lat_nice <- sapply(h$lat, function(l) {
  l <- stringr::str_trim(l) 
  l_num <- as.numeric(stringr::str_extract(l, "^[0-9.]*"))
  l_dir <- stringr::str_extract(l, "[NS]")
  if (l_dir == "S") l_num <- -1 * l_num
  l_num
}) 

h$lon_nice <- sapply(h$lon, function(l) {
  l <- stringr::str_trim(l) 
  l_num <- as.numeric(stringr::str_extract(l, "^[0-9.]*"))
  l_dir <- stringr::str_extract(l, "[WE]")
  if (l_dir == "W") l_num <- -1 * l_num
  l_num
}) 

# put the cmorph lat/lon on the same scale
p$lat_nice <- round(p$degree_north, 1)
p$lon_nice <- round(p$degree_east, 1)

head(p)
head(h)


# bucket the cmorph times into the 6hr hurdat ranges
p$days.since.2002.12.07.00.00.00 %>% table # currently in 3 hr chunks
p$time_chunk <- case_when(
  p$days.since.2002.12.07.00.00.00 %in% c(6191.062, 6191.188) ~ 0,
  p$days.since.2002.12.07.00.00.00 %in% c(6191.312, 6191.438) ~ 600,
  p$days.since.2002.12.07.00.00.00 %in% c(6191.562, 6191.688) ~ 1200,
  p$days.since.2002.12.07.00.00.00 %in% c(6191.812, 6191.938) ~ 1800
)

# get radius (in nm) at each time chunk of storm
h$r1 <- apply(h[, c(9,10,11,12)], 1, max)
h$r2 <- apply(h[, c(13,14,15,16)], 1, max)
h$r3 <- apply(h[, c(17,18,19,20)], 1, max)

# aggregate rainfall to same scale as hurdat data 
p_agg <- p %>% 
  group_by(lat_nice, lon_nice, time_chunk) %>% 
  summarise(precip = sum(mm.hr*3))

joined <- h %>% 
  left_join(p, by = c("time"="time_chunk"), suffix = c("", "_pr"))

joined <- joined %>% 
  mutate(p_in_storm = sqrt((lat_nice - lat_nice_pr)^2 + (lon_nice - lon_nice_pr)^2) * 8 < r1) %>% 
  filter(p_in_storm)

joined %>% 
  group_by(date, time, lat, lon) %>% 
  summarize(pr = sum(mm.hr))
