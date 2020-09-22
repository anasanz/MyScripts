

library(raster)
library(rgdal)


# Load GPS positions 

cip03_p1 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "cip03_p1")
cip03_p2 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "cip03_p2")
cip03_p3 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "cip03_p3")

cip04_p1 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "cip04_p1")
cip04_p2 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "cip04_p2")
cip04_p3 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "cip04_p3")

pic02_p2 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "pic02_p2")
pic02_p3 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "pic02_p3")

pic15_p2 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "pic15_p2")
pic15_p3 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "pic15_p3")

pic17_p2 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "pic17_p2")
pic17_p3 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "pic17_p3")
proj4string(pic17_p3)

# Create column to identify each period

cip03_p1$ID_p <- "cip03_p1"
cip03_p2$ID_p <- "cip03_p2"
cip03_p3$ID_p <- "cip03_p3"

cip04_p1$ID_p <- "cip04_p1"
cip04_p2$ID_p <- "cip04_p2"
cip04_p3$ID_p <- "cip04_p3"

pic02_p2$ID_p <- "pic02_p2"
pic02_p3$ID_p <- "pic02_p3"

pic15_p2$ID_p <- "pic15_p2"
pic15_p3$ID_p <- "pic15_p3"

pic17_p2$ID_p <- "pic17_p2"
pic17_p3$ID_p <- "pic17_p3"

# Select same columns

cip03_p1@data <- cip03_p1@data[ ,which(colnames(cip03_p1@data) %in% c("Logger_ID", "ID_p", "Year", "Month", "Day", "Hour", "Minute", "Second", "Latitude", "Longitude"))]
cip03_p2@data <- cip03_p2@data[ ,which(colnames(cip03_p2@data) %in% c("Logger_ID", "ID_p", "Year", "Month", "Day", "Hour", "Minute", "Second", "Latitude", "Longitude"))]
cip03_p3@data <- cip03_p3@data[ ,which(colnames(cip03_p3@data) %in% c("Logger_ID", "ID_p", "Year", "Month", "Day", "Hour", "Minute", "Second", "Latitude", "Longitude"))]

cip04_p1@data <- cip04_p1@data[ ,which(colnames(cip04_p1@data) %in% c("Logger_ID", "ID_p", "Year", "Month", "Day", "Hour", "Minute", "Second", "Latitude", "Longitude"))]
cip04_p2@data <- cip04_p2@data[ ,which(colnames(cip04_p2@data) %in% c("Logger_ID", "ID_p", "Year", "Month", "Day", "Hour", "Minute", "Second", "Latitude", "Longitude"))]
cip04_p3@data <- cip04_p3@data[ ,which(colnames(cip04_p3@data) %in% c("Logger_ID", "ID_p", "Year", "Month", "Day", "Hour", "Minute", "Second", "Latitude", "Longitude"))]

pic02_p2@data <- pic02_p2@data[ ,which(colnames(pic02_p2@data) %in% c("Logger_ID", "ID_p", "Year", "Month", "Day", "Hour", "Minute", "Second", "Latitude", "Longitude"))]
pic02_p3@data <- pic02_p3@data[ ,which(colnames(pic02_p3@data) %in% c("Logger_ID", "ID_p", "Year", "Month", "Day", "Hour", "Minute", "Second", "Latitude", "Longitude"))]

pic15_p2@data <- pic15_p2@data[ ,which(colnames(pic15_p2@data) %in% c("Logger_ID", "ID_p", "Year", "Month", "Day", "Hour", "Minute", "Second", "Latitude", "Longitude"))]
pic15_p3@data <- pic15_p3@data[ ,which(colnames(pic15_p3@data) %in% c("Logger_ID", "ID_p", "Year", "Month", "Day", "Hour", "Minute", "Second", "Latitude", "Longitude"))]

pic17_p2@data <- pic17_p2@data[ ,which(colnames(pic17_p2@data) %in% c("Logger_ID", "ID_p", "Year", "Month", "Day", "Hour", "Minute", "Second", "Latitude", "Longitude"))]
pic17_p3@data <- pic17_p3@data[ ,which(colnames(pic17_p3@data) %in% c("Logger_ID", "ID_p", "Year", "Month", "Day", "Hour", "Minute", "Second", "Latitude", "Longitude"))]

# Join all datasets

gps <- rbind(cip03_p1, cip03_p2, cip03_p3, cip04_p1, cip04_p2, cip04_p3,
             pic02_p2, pic02_p3, pic15_p2, pic15_p3, pic17_p2, pic17_p3)

writeOGR(gps, 'C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying', "gps_datetime", driver="ESRI Shapefile")
