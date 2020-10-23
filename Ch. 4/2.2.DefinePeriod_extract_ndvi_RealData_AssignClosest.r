
rm(list=ls())

library(raster)
library(lubridate)

# Load data and set it into format for function
setwd("D:/PhD/Fourth chapter/GIS/Capas_Rocío/GPS")
pos <- read.delim("D_gangas_no_regadio_ETRS89_tot.txt", dec = ",")

pos <- readOGR("XYD_gangas_no_regadio_ETRS89_tot.shp")

pos <- pos[which(pos$Year == 2017), ] # try with ndvi 2017
pos <- pos[which(pos$Month == 2 | pos$Month == 3 | pos$Month == 4), ] # 

pos$Date <- as.Date(paste(pos$Year, pos$Month, pos$Day, sep = "-")) # Generate date column


# Load all rasters 
rastlist <- list.files(path = "D:/PhD/Fourth chapter/GIS/NDVI/Marc/NDVI_real/Todas", pattern='.tif$', 
                       all.files=TRUE, full.names=TRUE)
allrasters <- lapply(rastlist, raster)


# Generate dates attribute (NECESSARY TO HAVE FILES IN THE FOLLOWING FORMAT: "2017-2-25.tif" (year-mes-day.tif)
allnames <- NULL # Create vector with all names
for (i in 1:length(allrasters)){
  allnames <- c(allnames, names(allrasters[[i]]))
}

alldates <- NULL 
for (i in 1:length(allnames)){
  allnames[[i]] <- gsub('\\.', '-', allnames[[i]])
  alldates[[i]] <- as.Date(substr(allnames[[i]], 2, 10))}

# 1. ---- ASSIGN CLOSEST DATE ---- 

# Format for function:

pos2 <- as.data.frame(pos@data[,colnames(pos@data) %in% c("Longitude","Latitude", "Date")]) # GPS ositions

mat <- matrix(c("2017-02-1", "2017-02-15",      # Matrix with all intervals
                "2017-02-15", "2017-02-25",
                "2017-02-25", "2017-03-07",
                "2017-03-07", "2017-03-17",
                "2017-03-17", "2017-04-06",
                "2017-04-06", "2017-04-16",
                "2017-04-16", "2017-04-30"), 
              nrow = 7, ncol = 2, byrow = TRUE)

setwd("D:/PhD/MyScripts/Ch. 4")
source("AssignClosest.r")

positions_extract <- assign.closest(data_positions = pos2, int = mat)


# 2. ---- EXTRACT NDVI ----

pos@data <- cbind(pos@data, positions_extract$closest_date)

pos@data <- pos@data[,c(1,8,9,20,22,23)]
colnames(pos@data)[6] <- "closest_date"

# Change CRS

check <- allrasters[[i]]
crs(check) # WGS84
crs(pos)
pos3 <- spTransform(pos, CRS = crs(check))# Change CRS
crs(pos3)

# Plot to check

plot(check)
points(pos3)


pos3$ndvi <- NA

for (i in 1:length(allrasters)){
  
  pos_tmp <- pos3[which(pos3$closest_date == alldates[[i]]), ]
  
  coord <- pos_tmp@coords
  cells <- cellFromXY(allrasters[[i]], coord) # Tells the number of the cells where the coord. fall
  positions_extract_ndvi <- allrasters[[i]][cells] # Extract
  
  pos3$ndvi[which(pos3$closest_date == alldates[[i]])] <- positions_extract_ndvi
}

pos3@data

setwd("D:/PhD/Fourth chapter/GIS/Capas_Rocío/GPS")

write.csv(pos3@data, "positions_ndvi_2017.csv")
