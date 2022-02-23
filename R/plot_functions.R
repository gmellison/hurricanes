
# function to generate the bar plot, based on a single row of the h dataframe.
make_bar_plot <- function(h, cuts, new_cuts=TRUE) {
  
  # rename things nicely
  h <- h %>% 
    select(c("Name", "year", 
             "Rainfall"="RainfallPointMax_Z",  
             "Pressure" = "Pressure_Z",
             "Windspeed" = "MaxWindSpeed_Z",
             "Surge" = "Surge_Z",
             "Tornado" = "Tornado_Z",
             "Size" = "Radius_Z"))
  
  # format data as key/value for easy bar plotting   
  plt_h <- pivot_longer(h, names_to = "Hazard", values_to = "ZScore", 
                        cols = c("Surge", "Windspeed", "Rainfall", "Tornado", "Pressure", "Size"), 
  )
  names(plt_h)[4] <- "ZScore"
  
  ws_cuts    <- cuts$ws_cuts
  surge_cuts <- cuts$surge_cuts
  pres_cuts  <- cuts$pres_cuts
  avg_cuts   <- cuts$avg_cuts
 
  if (new_cuts) {
    plt_h$Category <- .bincode(plt_h$ZScore, avg_cuts)-1
    plt_h$Category[plt_h$Hazard == "Windspeed"] <- .bincode(plt_h$ZScore[plt_h$Hazard == "Windspeed"], ws_cuts)-1
    plt_h$Category[plt_h$Hazard == "Surge"]     <- .bincode(plt_h$ZScore[plt_h$Hazard == "Surge"], surge_cuts)-1
    plt_h$Category[plt_h$Hazard == "Pressure"]  <- .bincode(plt_h$ZScore[plt_h$Hazard == "Pressure"], pres_cuts)-1
  } else {
    plt_h$Category <- .bincode(plt_h$ZScore, ws_cuts)-1
  }
  
  plt_h$Category[is.na(plt_h$Category)] <- 0
  
  ggplot(plt_h) + 
    geom_bar(aes(y = 2^(Category+1), 
                 fill = factor(Category),
                 x = factor(Hazard),
                 col = factor(Hazard)),
             stat= "identity") +
    scale_fill_manual(values=c("0"="yellow", "1"="orange", 
                               "2"="tomato1", "3" = "tomato3", "4"="red2", "5"="black"),
                      guide="none") +
    scale_color_manual(values=c(rep("black",6)), guide="none") + 
    coord_cartesian(ylim = c(0,2^6)) +
    labs(y = "Hazard Level", x = "") + #, title = sprintf("%s - %s", plt_h$Name[1], plt_h$year[1])) +
    ggthemes::theme_clean() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.65, size = 10.5),
          panel.background = element_blank(),
          legend.background = element_blank(),
          plot.background = element_blank()) +
    labs(title= sprintf("%s - %s", h$Name, h$year)) + 
    scale_y_continuous(breaks = 2^c(1,2,3,4,5,6), labels = c("TS", "1", "2", "3","4", "5"))
}
