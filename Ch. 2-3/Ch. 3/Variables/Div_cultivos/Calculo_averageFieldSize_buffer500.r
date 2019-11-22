
rm(list=ls())

library(rgdal)
library(dplyr)
library(raster)
library(rgeos)


# CALCULATE VARIABLE AVERAGE FIELD SIZE #

# Load SIGPAC layers cropped (manually in arcgis from full sigpac layers - SIGPAC15_full_EPSG23031.shp) with buffers

setwd("C:/Users/ana.sanz/Documents/PhD_12_Nov/Third chapter/GIS/Buffers_average_field_size")

sigpac15 <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD_12_Nov/Third chapter/GIS/Buffers_average_field_size", layer = "sigpac15_crop") 
sigpac16 <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD_12_Nov/Third chapter/GIS/Buffers_average_field_size", layer = "sigpac16_crop") 
sigpac17 <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD_12_Nov/Third chapter/GIS/Buffers_average_field_size", layer = "sigpac17_crop") 
sigpac18 <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD_12_Nov/Third chapter/GIS/Buffers_average_field_size", layer = "sigpac18_crop") 
sigpac19 <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD_12_Nov/Third chapter/GIS/Buffers_average_field_size", layer = "sigpac19_crop")

# Load transects and make buffers
tr <- readOGR("C:/Users/ana.sanz/Documents/PhD_12_Nov/Third chapter/GIS", "Trans_2010_2018_ch3_EPSG23031") # Contains transects sampled each year (1/0)
buf <- gBuffer(tr, byid = TRUE, width = 500) 
buf_id <- unique(buf@data$Codi)

# Data ready for loop

layer_names <- c("sigpac15", "sigpac16", "sigpac17", "sigpac18", "sigpac19")
layers <- c(sigpac15, sigpac16, sigpac17, sigpac18, sigpac19)

av_field_size <- as.data.frame(matrix(NA, nrow = nrow(buf), ncol = length(layers)+1))
colnames(av_field_size) <- c("Codi", layer_names)
av_field_size$Codi <- buf@data$Codi

crops_select <- c("FL","FV", "VO", "FS", "OV", "TA", "VI") # En documento usos_sigpac.xls


for (i in 1:length(layers)){
  
  poli <- raster::intersect(buf, layers[[i]]) # Intersect buffers with sigpac fields polygons
  poli_agri <- poli[which(poli@data$US %in% crops_select), ]   # 1. Select relevant agriculture crops
  poli_agri2 <- poli_agri[-which(duplicated(poli_agri$ID_REC)), ]   # 2. Remove duplicates (ID) <- because you can cut the same polygon in 2 polygons and then you count it twice making the average
  poli_agri3 <- poli_agri2[-which(poli_agri2@data$HA < 0.03), ]   # 3. Remove field size < 0.03ha (noise, bad digitalization)


  # 4. Calculate average field size


  poli@data <- left_join(poli@data, crops, by = "Cultiu") # Join in attribute table of the layers the new classification
  poli <- poli[-which(poli$diversitat.greening == "no"), ] # Delete the fields that are not considered for crop diversification in greening
  
  for (j in 1:length(buf)) {
    
    poli_transect <- poli[which(poli$Codi == buf_id[j]), ]  # SUbset of each buffer independently
    n_crops <- length(unique(poli_transect@data$diversitat.greening)) # Calculate number of different crops (unique)
    crop_diver[j,i+1] <- n_crops # Store
  }
}


p <- poli_agri@data[which(poli_agri@data$ID_REC == "25049:0:0:5:1:40"), ]
class(poli_agri@data$ID_REC)
