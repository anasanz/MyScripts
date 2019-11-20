
rm(list = ls())


library(moveVis)
library(move)
library(raster)
library(ggplot2)
library(dplyr)
library(readr)

# Load used positions and create date-time

gps <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "gps_datetime")

gps@data$date <- as.Date(with(gps@data, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
gps@data$time <- paste(gps@data$Hour,":",gps@data$Minute,":",gps@data$Second, sep="")

gps_data <- gps@data

#gps@data$datetime <- with(gps@data, as.POSIXlt(paste(date, time), format="%Y-%m-%d %H:%M", tz="UTC"))

time_stack <- as.POSIXlt(paste(gps_data$date, gps_data$time), format="%Y-%m-%d %H:%M:%S", tz="UTC")

gps_data <- gps_data[ ,c(1,8,9)]

proj4string(gps)


# Join datetime to data frame
PTER_move <- cbind(gps_data, time_stack)

# Convert to move object

pter_move <- df2move(PTER_move, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", 
                   "Longitude", "Latitude", 
                   time="time_stack", track_id = "Logger_ID")

unique(timestamps(pter_move))
timeLag(pter_move, unit = "days")
timeLag(pter_move, unit = "hours") # This means that is basically hourly positions


# Align
pter_move_aligned <- align_move(pter_move, res = 2, digit = 0, unit = "hours") # Adjust to hourly positions
unique(unlist(timeLag(pter_move_aligned, units = "hours")))

# Create frames
#frames_spatial creates a list of ggplot2 maps displaying movement. Each object represents a single frame.
#Each frame can be viewed or modified individually. The returned list of frames can be animated using animate_frames.

frames <- frames_spatial(pter_move_aligned, map_service = "mapbox", map_type = "satellite", 
                         map_token = "pk.eyJ1IjoiYW5hc2FuejIyIiwiYSI6ImNrMmtrdzh5MjBnbG8zbXFwenp2cTR6c3UifQ.mn-oDdu4sKeOS5L2UzQYbg",
                         alpha = 0.5,
                         path_colours = c("red", "green", "blue", "yellow", "pink"))
frames.a <- add_timestamps(frames, pter_move_aligned, type = "text")
frames.a2 <- add_scalebar(frames.a)
frames.a3 <-  add_progress(frames.a2)


length(frames.a) # number of frames
frames.a[[1193]] # display one of the frames

animate_frames(frames.a3, out_file = 
                 "C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/Congreso SEO/Plots/Pter_res3.mp4")




#####################################################
# Take only PIC 17 and PIC 15

# Load used positions and create date-time

gps <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "gps_datetime")
gps <- gps[which(gps@data$Logger_ID %in% c("PIC15", "PIC17")), ]

gps@data$date <- as.Date(with(gps@data, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
gps@data$time <- paste(gps@data$Hour,":",gps@data$Minute,":",gps@data$Second, sep="")

gps_data <- gps@data

#gps@data$datetime <- with(gps@data, as.POSIXlt(paste(date, time), format="%Y-%m-%d %H:%M", tz="UTC"))

time_stack <- as.POSIXlt(paste(gps_data$date, gps_data$time), format="%Y-%m-%d %H:%M:%S", tz="UTC")

gps_data <- gps_data[ ,c(1,8,9)]

proj4string(gps)


# Join datetime to data frame
PTER_move <- cbind(gps_data, time_stack)

# Convert to move object

pter_move <- df2move(PTER_move, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", 
                     "Longitude", "Latitude", 
                     time="time_stack", track_id = "Logger_ID")

unique(timestamps(pter_move))
timeLag(pter_move, unit = "days")
timeLag(pter_move, unit = "hours") # This means that is basically hourly positions


# Align
pter_move_aligned <- align_move(pter_move, res = 1, digit = 0, unit = "hours") # Adjust to hourly positions
unique(unlist(timeLag(pter_move_aligned, units = "hours")))

# Create frames
#frames_spatial creates a list of ggplot2 maps displaying movement. Each object represents a single frame.
#Each frame can be viewed or modified individually. The returned list of frames can be animated using animate_frames.

frames <- frames_spatial(pter_move_aligned, map_service = "mapbox", map_type = "satellite", 
                         map_token = "pk.eyJ1IjoiYW5hc2FuejIyIiwiYSI6ImNrMmtrdzh5MjBnbG8zbXFwenp2cTR6c3UifQ.mn-oDdu4sKeOS5L2UzQYbg",
                         alpha = 0.5,
                         path_colours = c("red", "blue"))
frames.a <- add_timestamps(frames, pter_move_aligned, type = "text")
frames.a2 <- add_scalebar(frames.a)
frames.a3 <-  add_progress(frames.a2)


length(frames.a) # number of frames
frames.a[[1193]] # display one of the frames

animate_frames(frames.a3, out_file = 
                 "C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/Congreso SEO/Plots/Pter_p15_p17_res1.mp4")


#####################################################
# Take only 2 days of PIC 17 and PIC 15

# Load used positions and create date-time

gps <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "gps_datetime")
gps <- gps[which(gps@data$Logger_ID %in% c("PIC15", "PIC17")), ]

gps@data$date <- as.Date(with(gps@data, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
gps@data$time <- paste(gps@data$Hour,":",gps@data$Minute,":",gps@data$Second, sep="")

gps_data <- gps@data

#gps@data$datetime <- with(gps@data, as.POSIXlt(paste(date, time), format="%Y-%m-%d %H:%M", tz="UTC"))

time_stack <- as.POSIXlt(paste(gps_data$date, gps_data$time), format="%Y-%m-%d %H:%M:%S", tz="UTC")

gps_data <- gps_data[ ,c(1,8,9)]

proj4string(gps)


# Join datetime to data frame
PTER_move <- cbind(gps_data, time_stack)
PTER_move_PIC15_2days <- PTER_move[c(which(row.names(PTER_move) == 6028): which(row.names(PTER_move) == 6083)), ]
PTER_move_PIC17_2days <- PTER_move[c(which(row.names(PTER_move) == 7580): which(row.names(PTER_move) == 7634)), ]
PTER_move_2days <- rbind(PTER_move_PIC15_2days, PTER_move_PIC17_2days)

# Convert to move object

pter_move <- df2move(PTER_move_2days, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", 
                     "Longitude", "Latitude", 
                     time="time_stack", track_id = "Logger_ID")

unique(timestamps(pter_move))
timeLag(pter_move, unit = "days")
timeLag(pter_move, unit = "hours") # This means that is basically hourly positions


# Align
pter_move_aligned <- align_move(pter_move, res = 1, digit = 0, unit = "hours") # Adjust to hourly positions
unique(unlist(timeLag(pter_move_aligned, units = "hours")))

# Create frames
#frames_spatial creates a list of ggplot2 maps displaying movement. Each object represents a single frame.
#Each frame can be viewed or modified individually. The returned list of frames can be animated using animate_frames.

frames <- frames_spatial(pter_move_aligned, map_service = "mapbox", map_type = "satellite", 
                         map_token = "pk.eyJ1IjoiYW5hc2FuejIyIiwiYSI6ImNrMmtrdzh5MjBnbG8zbXFwenp2cTR6c3UifQ.mn-oDdu4sKeOS5L2UzQYbg",
                         alpha = 0.5,
                         path_colours = c("red", "blue"))
frames.a <- add_timestamps(frames, pter_move_aligned, type = "text")
frames.a2 <- add_scalebar(frames.a)
frames.a3 <-  add_progress(frames.a2)


length(frames.a) # number of frames
frames.a[[1193]] # display one of the frames

animate_frames(frames.a3, out_file = 
                 "C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/Congreso SEO/Plots/Pter_p15_p17_res1_2days.mp4")
