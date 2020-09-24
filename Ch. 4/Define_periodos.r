
#################################################################################################
##################  DEFINICIÓN DE PERIODOS: POSICIONES GPS EN CEREAL   #########################
#################################################################################################

rm(list=ls())

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
#writeRaster(cereal17, filename = 'cereal_17', format = 'GTiff')

cereal18 <- raster(d18, layer = 1)
cereal18[is.na(cereal18)] <- 0 # Change NA to 0
#writeRaster(cereal18, filename = 'cereal_18', format = 'GTiff')

cereal19 <- raster(d19, layer = 5)
cereal19[is.na(cereal19)] <- 0 # Change NA to 0
#writeRaster(cereal19, filename = 'cereal_19', format = 'GTiff')

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
positions_cereal$date <- paste(positions_cereal$Month,"/",positions_cereal$Day, sep = "")
positions_cereal <- positions_cereal[order(as.Date(positions_cereal$date, format= "%m/%d")),]

# Arrange cutre by date so that it is in order (paste manually the ones from december up):
pos_cereal_dic <- positions_cereal[grep("12/", as.character(positions_cereal$date)), ]
positions_cereal <- positions_cereal[-grep("12/", as.character(positions_cereal$date)), ]
positions_cereal <- rbind(pos_cereal_dic, positions_cereal)

# Plot

positions_cereal$date <- factor(positions_cereal$date, levels = unique(positions_cereal$date)) # change factor to sort factor levels

# Create vector for names x axes
dates <- c("1-Dic", "18-Ene", "20-Mar", "28-Ab", "6-Jun", "18-Jul", "29-Agos")
res <- barplot(table(positions_cereal$date), xaxs="i",xaxt="n") # To get midpoints
coordx <- res[c(1, 45, 91, 130, 169, 211, 253)]

setwd("D:/PhD/Fourth chapter/Results/Preliminar results")
pdf("posiciones_gps.pdf",9,5)
barplot(table(positions_cereal$date), main = "Nº posiciones GPS", xlab = "", ylab = "", xaxt = "n", las = 2)
abline(v = 108.7, lwd = 2)
abline(v = 202.3, lwd = 2)
barplot(table(positions_cereal$date[which(positions_cereal$cereal == 1)]), col = adjustcolor("red", alpha.f = 0.8), xlab = "", xaxt = "n", yaxt = "n", add = TRUE)

legend("topleft", legend = c("Cereal"), col = c("red"), fill = c("red"), bty = "n")
mtext(dates, side = 1, line = 0.5, at = coordx, las = 2)

dev.off()
 


# Percentage of positions in cereal per day

positions_cereal$cereal <- as.factor(positions_cereal$cereal) # Need to be factor to not drop levels with empty observations (keep the 0 where there is no positions in cereal)
data_cereal <- positions_cereal %>%
  group_by(date, cereal, .drop = FALSE) %>%
  summarise(count = n() )

per_cereal <- data_cereal %>%
  group_by(date) %>%
  mutate(countT= sum(count)) %>%
  group_by(cereal, add=TRUE) %>%
  mutate(per=round(100*count/countT,2))

per_cereal <- per_cereal[which(per_cereal$cereal == 1), ]

# Arrange cutre (poner diciembre arriba)
#per_cereal_dic <- per_cereal[c(29:55), ]
#per_cereal <- per_cereal[-c(29:55), ]
#per_cereal <- rbind(per_cereal_dic, per_cereal)

per_cereal$date <- factor(per_cereal$date, levels = unique(per_cereal$date)) # change factor to sort factor levels

setwd("D:/PhD/Fourth chapter/Results/Preliminar results")
pdf("posiciones_cereal.pdf",9,5)
barplot(per_cereal$per ~ per_cereal$date, ylab = "% Posiciones en cereal", xlab = "", xaxt = "n", las = 2) # Use vector "names" as well for xlabs 
abline(v = 108.7, lwd = 2)
abline(v = 202.3, lwd = 2)
mtext(dates, side = 1, line = 0.5, at = coordx, las = 2)

dev.off()


# IMPRIMIR DOS PLOTS JUNTOS

setwd("D:/PhD/Fourth chapter/Results/Preliminar results")

pdf("explore_gps.pdf",15,10)

par(mfrow = c(2,1))
barplot(table(positions_cereal$date), main = "", xlab = "", ylab = "Nº posiciones GPS", xaxt = "n", las = 2)
abline(v = 108.7, lwd = 2)
abline(v = 202.3, lwd = 2)
barplot(table(positions_cereal$date[which(positions_cereal$cereal == 1)]), col = adjustcolor("red", alpha.f = 0.8), xlab = "", xaxt = "n", yaxt = "n", add = TRUE)

legend("topleft", legend = c("Cereal"), col = c("red"), fill = c("red"), bty = "n")
mtext(dates, side = 1, line = 0.5, at = coordx, las = 2)

barplot(per_cereal$per ~ per_cereal$date, ylab = "% Posiciones en cereal", xlab = "", xaxt = "n", las = 2) # Use vector "names" as well for xlabs 
abline(v = 108.7, lwd = 2)
abline(v = 202.3, lwd = 2)
mtext(dates, side = 1, line = 0.5, at = coordx, las = 2)

dev.off()
