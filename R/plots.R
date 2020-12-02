library(ggplot2)
library(dplyr)

data <- read.csv("data/AllData2.csv")

data$MaxWindSpeed_Z <- scale(data$MaxWindSpeed)
data$Rainfall_Z <- scale(data$Rainfall)
data$Surge_Z <- scale(data$Surge)

data <- select(data, c(Name, Surge_Z, Rainfall_Z, MaxWindSpeed_Z))

dplot1 <- ggplot(data) +
  geom_density(aes(x=MaxWindSpeed_Z)) +
  theme_minimal()

dplot2 <- ggplot(data) +
  geom_density(aes(x=Rainfall_Z)) + 
  theme_minimal()

dplot3 <- ggplot(data) +
  geom_density(aes(x=Surge_Z)) + 
  theme_minimal()

dp1 <- ggplot_build(dplot1)
dp2 <- ggplot_build(dplot2)
dp3 <- ggplot_build(dplot3)


make_density_plot <- function(h) {
  ws_i <- which(dp1$data[[1]]$x >= as.numeric(h$MaxWindSpeed_Z))
  rf_i <- which(dp2$data[[1]]$x >= as.numeric(h$Rainfall_Z))
  s_i <- which(dp3$data[[1]]$x >= as.numeric(h$Surge_Z))
    
  dplot1_fill <- dplot1 +
    geom_area(data = data.frame(x=dp1$data[[1]]$x[ws_i],
                                y=dp1$data[[1]]$y[ws_i]),
              aes(x,y, fill = min(x))) +
    scale_fill_distiller(palette="Spectral", limits = range(data$MaxWindSpeed_Z, na.rm=TRUE))
  
  dplot2_fill <- dplot2 +
    geom_area(data = data.frame(x=dp2$data[[1]]$x[rf_i],
                                y=dp2$data[[1]]$y[rf_i]),
              aes(x,y, fill=min(x))) +
     scale_fill_distiller(palette="Spectral", limits = range(data$Rainfall_Z, na.rm=TRUE))
  
  dplot3_fill <- dplot3 +
    geom_area(data = data.frame(x=dp3$data[[1]]$x[s_i],
                                y=dp3$data[[1]]$y[s_i]),
              aes(x,y, fill=min(x))) +
       scale_fill_distiller(palette="Spectral", limits = range(data$Surge_Z, na.rm=TRUE))
  
  gridExtra::grid.arrange(dplot1_fill, dplot2_fill, dplot3_fill)
}

## circle plots
data_c <- data
data_c$Surge_Z[is.na(data$Surge_Z)] <- 0
data_c$Rainfall_Z[is.na(data$Rainfall_Z)] <- 0
data_c$MaxWindSpeed_Z[is.na(data$MaxWindSpeed_Z)] <- 0

r_rain <- range(data_c$Rainfall_Z)
r_surge <- range(data_c$Surge_Z)
r_wind <- range(data_c$MaxWindSpeed_Z)


h <- data[data$Name == "DORIAN", ][5, ]

norm <- function(x, range, new_range = c(0, 2 * pi)) {
  (x - min(range)) / (max(range) - min(range)) * max(new_range)
}

make_rect <- function(normed_x, y, y_height = 1) {
  data.frame(x = c(0, normed_x, normed_x, 0), y = c(y, y, y+y_height, y+y_height))
}

make_circle_plot <- function(h) {
  h_rain <- as.numeric(h$Rainfall_Z)
  h_surge <- as.numeric(h$Surge_Z)
  h_wind <- as.numeric(h$MaxWindSpeed_Z)
  
  surge_df <- make_rect(norm(h_surge, r_surge), 0.5)
  rain_df <- make_rect(norm(h_rain, r_rain), 1.5)
  wind_df <- make_rect(norm(h_wind, r_wind), 2.5)
  ggplot() + 
    geom_polygon(data=make_rect(norm(2*pi, c(0,2*pi)), y = 0, y_height=0.5),
                 aes(x,y),
                 fill = "black", 
                 color = "black") +  # this ploygon is the center
    geom_polygon(data=surge_df,
                 aes(x,y,fill = max(x)/(2*pi)),
                 color = "black") + #,
                 # alpha = max(surge_df$x)/(2*pi)) +
     geom_polygon(data=rain_df,
                 aes(x,y, fill = max(x)/(2*pi)),
                 color = "black") + #,
                 # alpha = max(rain_df$x)/(2*pi)) + 
     geom_polygon(data=wind_df,
                 aes(x,y,fill = max(x)/(2*pi)),
                 color = "black") + #,
                 #alpha = max(wind_df$x)/(2*pi)) +
    coord_polar() +
    scale_fill_distiller(palette="Spectral", limits=c(0, 1)) + 
    theme_minimal() + 
    theme(axis.text=element_blank(),
          axis.title = element_blank())
  
}

   
h <- data_c[data_c$Name == "KATRINA", ][3, ]
h <- data_c[data_c$Name == "MITCH", ][1, ]
h <- data_c[data_c$Name == "ISABEL", ][1, ]
h <- data_c[data_c$Name == "DORIAN"][3, ]
make_density_plot(h)
make_circle_plot(h)

