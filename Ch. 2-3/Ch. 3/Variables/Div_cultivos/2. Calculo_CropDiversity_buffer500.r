
rm(list=ls())

library(rgdal)
library(dplyr)
library(raster)
library(rgeos)


# CALCULATE VARIABLE CROP RICHNESS #

# Load DUN layers cropped with buffers

# There are errors in intersect loop below (self intersections in the layers. Modified manually in Arcgis and re-load them here(dun15, dun19))

setwd("C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Buffers_cropdiver_classification")

dun15 <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Buffers_cropdiver_classification2", layer = "dun15_crop2") 
dun16 <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Buffers_cropdiver_classification2", layer = "dun16_crop2") 
dun17 <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Buffers_cropdiver_classification2", layer = "dun17_crop2") 
dun18 <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Buffers_cropdiver_classification2", layer = "dun18_crop2") 
dun19 <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Buffers_cropdiver_classification2", layer = "dun19_crop2")

# Load classification 

setwd("C:/Users/ana.sanz/Documents/PhD/Third chapter/Data")
crops <- read.csv("clasificacion_cultivos_div_greening.csv", header = TRUE, sep = ";")
#crops_years <- crops[c(1:111),c(5:9)]
crops <- crops[c(1:111),c(1,4)]
crops <- crops[-which(duplicated(crops$Cultiu)), ] # Otherwise NOTHING works


# For each transect (buffer) and year, calculate number of crops

tr <- readOGR("C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS", "Trans_2018_EPSG23031") # Contains transects sampled each year (1/0)
buf <- gBuffer(tr, byid = TRUE, width = 500)
buf_id <- unique(buf@data$Codi)

layer_names <- c("dun15", "dun16", "dun17", "dun18", "dun19")
layers <- c(dun15, dun16, dun17, dun18, dun19)

crop_diver <- as.data.frame(matrix(NA, nrow = nrow(buf), ncol = length(layers)+1))
colnames(crop_diver) <- c("Codi", layer_names)
crop_diver$Codi <- buf@data$Codi


for (i in 1:length(layers)){
  
  poli <- raster::intersect(buf, layers[[i]]) # Intersect buffers with management fields polygons
  poli@data <- left_join(poli@data, crops, by = "Cultiu") # Join in attribute table of the layers the new classification
  poli <- poli[-which(poli$diversitat.greening == "no"), ] # Delete the fields that are not considered for crop diversification in greening
  
  for (j in 1:length(buf)) {
    
    poli_transect <- poli[which(poli$Codi == buf_id[j]), ]  # SUbset of each buffer independently
    n_crops <- length(unique(poli_transect@data$diversitat.greening)) # Calculate number of different crops (unique)
    crop_diver[j,i+1] <- n_crops # Store
  }
}

setwd("C:/Users/ana.sanz/Documents/PhD/Third chapter/Data")
read.csv("clasificacion_cultivos_div_greening.csv", header = TRUE, sep = ";")

write.csv(crop_diver, "crop_richness_500_2.csv")
