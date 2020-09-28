
rm(list=ls())

library(raster)

# Load data
setwd("D:/PhD/Fourth chapter/GIS/Capas_Rocío/GPS")
pos <- read.delim("D_gangas_no_regadio_ETRS89_tot.txt", dec = ",")
pos <- pos[-which(pos$Year == 2016), ] #♠?? preguntar rocío
pos$ndvi <- NA

# Load all rasters
rastlist <- list.files(path = "D:/PhD/Fourth chapter/GIS/NDVI/Todas", pattern='.tiff$', 
                       all.files=TRUE, full.names=TRUE)
allrasters <- lapply(rastlist, raster)


# Generate dates attribute (NECESSARY TO HAVE FILES IN THE FOLLOWING FORMAT: "01_07_4_2017.tiff")

allnames <- NULL # Create vector with all names
for (i in 1:length(allrasters)){
  allnames <- c(allnames, names(allrasters[[i]]))
}

alldates <- list()
dates <- list()
for (i in 1:length(allrasters)){
  dates$Year <- as.numeric(substr(allnames[[i]], 10, 13))
  dates$Month <- as.numeric(substr(allnames[[i]], 8,8))
  dates$end <- as.numeric(substr(allnames[[i]], 5,6))
  dates$start <- as.numeric(substr(allnames[[i]], 2,3))
  alldates[[i]] <- dates
  }


for (i in 1:length(allrasters)){

pos_tmp <- pos[which(pos$Year == alldates[[i]]$Year &      # Select points from dates raster file
                       pos$Month == alldates[[i]]$Month &
                       pos$Day >= alldates[[i]]$start &
                       pos$Day <= alldates[[i]]$end), ]

coord <- pos_tmp[ ,c("Longitude","Latitude")] 
cells <- cellFromXY(allrasters[[i]], coord) # Tells the number of the cells where the coord. fall
pos_ndvi <- allrasters[[i]][cells] # Extract

pos$ndvi[which(pos$Year == alldates[[i]]$Year & 
            pos$Month == alldates[[i]]$Month &
            pos$Day >= alldates[[i]]$start &
            pos$Day <= alldates[[i]]$end)] <- pos_ndvi
}

setwd("D:/PhD/Fourth chapter/GIS/Capas_Rocío/GPS")
write.csv(pos, "positions_feb_ab_periods1_2.csv")
