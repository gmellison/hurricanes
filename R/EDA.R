library(dplyr)
library(ggplot2)
library(lubridate)
library(maps)
library(maptools)

storm_data <- read.csv("data/AllData.csv", stringsAsFactors = FALSE)
summary(storm_data)
head(storm_data)

# plot them on a map
wm <- borders("world", colour="gray50", fill="white")
sm <- ggplot() + wm
sm <- sm +  geom_point(data=storm_data, aes(x=Longitude, y=Latitude), color="red", alpha=0.5)
ggsave("plots/eda/storm_map.png")


####
#### couple time series
####

# storms per year:
df_plot <- group_by(storm_data, year = year(mdy(Time))) %>% 
  summarise(count=n())

year_series <- ggplot(df_plot) + 
  geom_point(aes(x=year, y=count)) +
  geom_line(aes(x=year, y=count))
year_series
ggsave("plots/eda/storms_per_year.png")

# pressure, wind, rainfall, surge by year:
ggplot(storm_data) + 
  geom_point(aes(x=mdy(Time), y=Rainfall))
  
ggplot(storm_data) + 
  geom_point(aes(x=mdy(Time), y=MaxWindSpeed))

ggplot(storm_data) + 
  geom_point(aes(x=mdy(Time), y=CentPressure)) 

ggplot(storm_data) + 
  geom_point(aes(x=mdy(Time), y=Surge))
 
####
#### couple scatter plots
####
# windspeeed vs. pressure
pres_vs_windspeed <- ggplot(storm_data,
       aes(x=CentPressure,
           y = MaxWindSpeed)) + 
  geom_point()
ggsave("plots/eda/wind_vs_pressure.png")

# rain vs. pressure
ggplot(storm_data,
       aes(x=CentPressure,
           y = Rainfall)) + 
  geom_point()
ggsave("plots/eda/rain_vs_pressure.png")

# surge vs. rainfall
ggplot(storm_data,
       aes(x = Rainfall,
           y = Surge)) + 
  geom_point()
ggsave("plots/eda/rain_vs_surge.png")

# surge vs. pressure
ggplot(storm_data,
       aes(x = CentPressure,
           y = Surge)) + 
  geom_point()
ggsave("plots/eda/pressure_vs_surge.png")

