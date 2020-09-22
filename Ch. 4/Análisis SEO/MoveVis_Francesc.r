install.packages("moveVis")
install.packages('move', 'raster', 'ggplot2')
install.packages("Rtools")
 library(moveVis)
library(move)
library(raster)
library(ggplot2)
library(dplyr)
library(readr)
 TD_move <-
 read_delim("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/Congreso SEO/Plots/TD_move_SP_UK_FR_DE.csv",";", escape_double = FALSE, 
            col_types = cols(Date = col_date(format = "%d/%m/%Y"), Time = col_time(format = "%H:%M:%S")), 
            trim_ws = TRUE)
 View(TD_move)
 #Converteixo data i hora a un Objecte de classe POSIXct

 time_stack <- as.POSIXct(paste(TD_move$Date, TD_move$Time), format = 
                           "%Y-%m-%d %H:%M:%S",tz="UTC")


 #Afegeixo aquest objecte al data frame 
 TD_move <- cbind(TD_move, time_stack)
#Converteixo dataframe a move object http://movevis.org/reference/df2move.html
td_cat_move<-df2move(TD_move, "+proj=longlat +ellps=WGS84 +datum=WGS84
                    +no_defs", "Longitude", "Latitude", time="time_stack", track_id = "ID")
 #©això son descriptius de les dades
 unique(timestamps(td_cat_move))
timeLag(td_cat_move, unit = "days")

 #We can conclude that each track has a sampling rate of roughly 1 day, however sampling rates 
#differ over time. Due to this, tracks do not share unique timestamps. For animation,
#unique frame times are needed, regardless if we want to animate a single track or multiple at once.
#Thus, we need to align move_data in order to * make all tracks share unique timestamps that can be 
#assigned to frames * make all tracks share unique, steady sampling rates without gaps

 td_cat_move <- align_move(td_cat_move, res = 24, digit = 0, unit = "hours") 
 #Tot i que és un punt diari, ho he posat així perquè el video no vagi tan rapid

 #frames_spatial creates a list of ggplot2 maps displaying movement. Each object represents a single frame.
#Each frame can be viewed or modified individually. The returned list of frames can be animated using animate_frames.
 frames <- frames_spatial(td_cat_move, map_service = "osm", map_type = 
                            "topographic", alpha = 0.5,  trace_show = TRUE)


 frames.a <- add_timestamps(frames, td_cat_move, type = "text") 
 #frames.a

 length(frames.a) # number of frames
 frames.a[[1385]] # display one of the frames

   animate_frames(frames.a, out_file =
                      "C:/Users/francesc.sarda/FEINA/Tortora/2018/Videos/TD_move_SP_tot.mp4"
                    )

   
   # EXAMPLE ALIGN #
   
   library(moveVis)
   library(move)
   data("move_data")
   
   # the tracks in move_data have irregular timestamps and sampling rates.
   # print unique timestamps and timeLag
   unique(timestamps(move_data))
   unique(unlist(timeLag(move_data, units = "secs")))
   
   # use align_move to correct move_data to a uniform time scale and lag using interpolation.
   # resolution of 4 minutes (240 seconds) at digit 0 (:00 seconds) per timestamp:
   m <- align_move(move_data, res = 240, digit = 0, unit = "secs")
   unique(unlist(timeLag(m, units = "secs")))
   
   # resolution of 1 hour (3600 seconds) at digit 0 (:00 seconds) per timestamp:
   m <- align_move(move_data, res = 3600, digit = 0, unit = "secs")
   unique(unlist(timeLag(m, units = "secs")))
   
   # resolution of 1 hour (15 seconds) at digit 0 (:00 seconds) per timestamp:
   m <- align_move(move_data, res = 15, digit = 0, unit = "secs")
   unique(unlist(timeLag(m, units = "secs")))
   
   # resolution of 1 hour:
   m <- align_move(move_data, res = 60, unit = "mins")
   unique(unlist(timeLag(m, units = "secs")))
   
   #### Gangas ####
   
   
   
   rm(list = ls())
   
  
   
   library(moveVis)
   library(move)
   library(raster)
   library(ggplot2)
   library(dplyr)
   library(readr)
   
   PTEALC <-
     read_delim("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/Congreso SEO/Plots/Gangues_2019.csv",
                ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y"), Time = col_time(format = "%H:%M")),trim_ws = TRUE)
   View(PTEALC)
   
   PTEALC <- subset(PTEALC, ID != 'GUE04')
   #PTEALC <- subset(PTEALC, ID = 'GUE04')
   unique(PTEALC$ID)
   
   #Converteixo data i hora a un Objecte de classe POSIXct
   
   time_stack <- as.POSIXct(paste(PTEALC$Date, PTEALC$Time), format = "%Y-%m-%d %H:%M:%S",tz="UTC")
   
   
   #Afegeixo aquest objecte al data frame
   PTEALC <- cbind(PTEALC, time_stack)
   
   #Converteixo dataframe a move object 
   #http://movevis.org/reference/df2move.html
   
   PTEALC_move<-df2move(PTEALC, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", "Longitude", "Latitude", time="time_stack", track_id = "ID")
   
   #©això son descriptius de les dades
   unique(timestamps(PTEALC_move))
   timeLag(PTEALC_move, unit = "hours")
   
   
   #We can conclude that each track has a sampling rate of roughly 1 day, however sampling rates
   #differ over time. Due to this, tracks do not share unique timestamps. For animation,
   #unique frame times are needed, regardless if we want to animate a single track or multiple at once.
   #Thus, we need to align move_data in order to * make all tracks share unique timestamps that can be
   #assigned to frames * make all tracks share unique, steady sampling rates without gaps
   
   PTEALC_move <- align_move(PTEALC_move, res = 1, digit = 0, unit = "hours")
   
   
   #frames_spatial creates a list of ggplot2 maps displaying movement. Each object represents a single frame.
   #Each frame can be viewed or modified individually. The returned list of frames can be animated using animate_frames.
   
   frames <- frames_spatial(PTEALC_move, path_colours = c("red", "green", 
                                                          "blue", "yellow", "pink"),
                            map_service = "osm", map_type = "watercolor", 
                            alpha = 0.5)
   
   frames.a <- add_timestamps(frames, PTEALC_move, type = "text")
   #frames.a
   
   length(frames.a) # number of frames
   frames.a[[2000]] # display one of the frames
   
   animate_frames(frames.a, out_file = 
                    "C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/Congreso SEO/Plots/PTEALC_2019_v2.mp4")