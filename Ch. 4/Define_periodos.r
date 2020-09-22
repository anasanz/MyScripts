
#################################################################################################
################################### DEFINICIÓN DE PERIODOS    ####################################
#################################################################################################

library(rgdal)
library(raster)
library(dplyr)

# Posiciones GPS

setwd("D:/PhD/Fourth chapter/GIS/Capas_Rocío")
pos <- read.delim("D_gangas_no_regadio_ETRS89_tot.txt", dec = ",")
pos <- pos[-which(pos$Year == 2016), ] #♠?? preguntar rocío

# Capas DUN

dun17 <- readOGR("D:/PhD/Fourth chapter/GIS/Capas_Rocío/usos", "clip_dun17_usos_4326")
dun18 <- readOGR("D:/PhD/Fourth chapter/GIS/Capas_Rocío/usos", "clip_dun18_usos_4326")
dun19 <- readOGR("D:/PhD/Fourth chapter/GIS/Capas_Rocío/usos", "clip_dun19_usos_4326")

# Converted to raster in arcgis because it works better
r17 <- raster("D:/PhD/Fourth chapter/GIS/Capas_Rocío/usos/clip_dun17_usos_4326.tif") # Cereal is value 5
r18 <- raster("D:/PhD/Fourth chapter/GIS/Capas_Rocío/usos/clip_dun18_usos_4326.tif") # Cereal is value 1
r19 <- raster("D:/PhD/Fourth chapter/GIS/Capas_Rocío/usos/clip_dun19_usos_4326.tif") # Cereal is value 5

# Create cereal layers

d17 <- layerize(r17, classes = NULL, bylayer = TRUE, suffix = 'numbers')
d18 <- layerize(r18, classes = NULL, bylayer = TRUE, suffix = 'numbers')
d19 <- layerize(r19, classes = NULL, bylayer = TRUE, suffix = 'numbers')

setwd("D:/PhD/Fourth chapter/GIS/Capas_Rocío/usos")

cereal17 <- raster(d17, layer = 5)
cereal17[is.na(cereal17)] <- 0 # Change NA to 0
writeRaster(cereal17, filename = 'cereal_17', format = 'GTiff')

cereal18 <- raster(d18, layer = 1)
cereal18[is.na(cereal18)] <- 0 # Change NA to 0
writeRaster(cereal18, filename = 'cereal_18', format = 'GTiff')

cereal19 <- raster(d19, layer = 5)
cereal19[is.na(cereal19)] <- 0 # Change NA to 0
writeRaster(cereal19, filename = 'cereal_19', format = 'GTiff')

# Extract positions in cereal

cereal <- stack(cereal17, cereal18, cereal19)
coord <- pos[ ,c("Longitude","Latitude")]

cells <- cellFromXY(cereal, coord) # 1. Tells the number of the cells where the coord. fall
poscereal <- cereal[cells] 
colnames(poscereal) <- c("2017", "2018", "2019")

pos <- cbind(pos, poscereal)
pos$cereal <- NA

for (i in 1:nrow(pos)){
  pos[i, 25] <- pos[i, which(colnames(pos) == pos$Year[i])]
}

# Check: clean columns
pos <- pos[,c(1:9, 20:25)]
positions_cereal <- pos[ ,c(1:11,15)]

# Create date column
positions_cereal$date <- paste(positions_cereal$Month,"-",positions_cereal$Day, sep = "")

positions_cereal <- arrange(positions_cereal, date)
# All
plot(xtabs(~ date, positions_cereal)) 
plot(xtabs(~ date, positions_cereal[])) 

