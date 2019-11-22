
rm(list=ls())

library(rgdal)
library(dplyr)
library(raster)
library(rgeos)


# CALCULATE VARIABLE AVERAGE FIELD SIZE #

# Load SIGPAC layers cropped (manually in arcgis from full sigpac layers - SIGPAC15_full_EPSG23031.shp) with buffers

setwd("C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Buffers_average_field_size")

sigpac15 <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Buffers_average_field_size", layer = "sigpac15_crop") 
sigpac16 <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Buffers_average_field_size", layer = "sigpac16_crop") 
sigpac17 <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Buffers_average_field_size", layer = "sigpac17_crop") 
sigpac18 <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Buffers_average_field_size", layer = "sigpac18_crop") 
sigpac19 <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Buffers_average_field_size", layer = "sigpac19_crop")

# Change names sigpac 17 and sigpac 18 to make it fit with loop
names(sigpac15)
names(sigpac17)[5] <- "HA"
names(sigpac17)[14] <- "US"
names(sigpac17)[20] <- "ID_REC"

names(sigpac18)
names(sigpac18)[12] <- "ID_REC"
names(sigpac18)[13] <- "US"
names(sigpac18)[17] <- "HA"

# Load transects and make buffers
tr <- readOGR("C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS", "Trans_2010_2018_ch3_EPSG23031") # Contains transects sampled each year (1/0)
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

  for (j in 1:length(buf)) { # 4. Calculate average field size (I take the VALUE of size in ha of the fields that fall in the buffer, not the area that falls like with the fallows variable)
    
    poli_transect <- poli_agri3[which(poli_agri3$Codi == buf_id[j]), ]  # SUbset of each buffer independently
    av_field <- base::mean(poli_transect$HA)
    av_field_size[j,i+1] <- av_field # Store
  }
}

#W CHECK NA VALUES Y PROJECTION (capa transectos NO ESTA en el coordinate system adecuado. tambien para crop diversity)
setwd("C:/Users/ana.sanz/Documents/PhD/Third chapter/Data")
write.csv(av_field_size, "av_fieldsize_500.csv")

