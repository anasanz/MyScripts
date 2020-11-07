
#################################################################################################
##################  DEFINICIÓN DE PERIODOS: POSICIONES GPS EN CEREAL   #########################
#################################################################################################

rm(list=ls())

library(rgdal)
library(raster)
library(dplyr)

# Posiciones GPS

setwd("D:/PhD/Fourth chapter/GIS/Capas_Rocío/GPS")
pos <- read.delim("D_gangas_no_regadio_ETRS89_tot.txt", dec = ",")
pos <- pos[-which(pos$Year == 2016), ] #♠?? preguntar rocío
pos$ID_pos <- seq(1,nrow(pos))

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
  pos[i, 26] <- pos[i, which(colnames(pos) == pos$Year[i])]
}

# Check: clean columns
pos <- pos[,c(1:9, 20:26)]

# Save data frame
setwd("D:/PhD/Fourth chapter/GIS/Capas_Rocío/GPS")
write.csv(pos, "positions_cereal.csv")

#############################################
# Load 

setwd("D:/PhD/Fourth chapter/GIS/Capas_Rocío/GPS")
pos <- read.csv("positions_cereal.csv")
positions_cereal <- pos[ ,c(2:12,17)]


# Create date column
positions_cereal$date <- paste(positions_cereal$Month,"/",positions_cereal$Day, sep = "")
positions_cereal <- positions_cereal[order(as.Date(positions_cereal$date, format= "%m/%d")),]

# Arrange cutre by date so that it is in order (paste manually the ones from december up):
pos_cereal_dic <- positions_cereal[grep("12/", as.character(positions_cereal$date)), ]
positions_cereal <- positions_cereal[-grep("12/", as.character(positions_cereal$date)), ]
positions_cereal <- rbind(pos_cereal_dic, positions_cereal)

# Add dates manually where there are no observations (1 row with NA per observation)

missing_dates <- c("12/28", "12/29", "12/30", "12/31","2/15", "2/16", "2/17", "2/18", "2/19", "2/20", "2/21", "2/22",
                   "2/23", "2/24", "2/25", "2/26", "2/27", "2/28", "3/1")

df <- as.data.frame(matrix(NA, nrow = 19, ncol = 13))
df[,13] <- missing_dates
colnames(df) <- colnames(positions_cereal)

positions_cereal <- rbind(positions_cereal[1:519,],df[1:4, ],positions_cereal[-(1:519),]) # Insert-December (after 519)
positions_cereal <- rbind(positions_cereal[1:1694,],df[5:19, ],positions_cereal[-(1:1694),])# Insert-Febrero 


# Plot

positions_cereal$date <- factor(positions_cereal$date, levels = unique(positions_cereal$date)) # change factor to sort factor levels

# Create vector for names x axes
dates <- c("1-Dic", "18-Ene", "20-Mar", "28-Ab", "6-Jun", "18-Jul", "29-Agos")
res <- barplot(table(positions_cereal$date), xaxs="i",xaxt="n") # To get midpoints
f <- table(positions_cereal$date)
names(f)
coordx <- res[c(1, 49, 110, 149, 188, 230, 272)]

setwd("D:/PhD/Fourth chapter/Results/Preliminar results")
pdf("posiciones_gps.pdf",9,5)
barplot(table(positions_cereal$date), main = "Nº posiciones GPS", xlab = "", ylab = "", xaxt = "n", las = 2)
abline(v = 131.5, lwd = 2)
abline(v = 225.1, lwd = 2)
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
abline(v = 131.5, lwd = 2)
abline(v = 225.1, lwd = 2)
mtext(dates, side = 1, line = 0.5, at = coordx, las = 2)

dev.off()

# Add in this figure a polygon with the dates for which there is NDVI in 2017: Feb[63] - Abril[151]
# Add also possible division dates (accordying to max ndvi = green; and to growing cereal = blue (1Marzo))
par(mfrow = c(2,1))

setwd("D:/PhD/Fourth chapter/Results/Preliminar results")
pdf("posiciones_cereal_dataNDVI2.pdf",9,5)
barplot(per_cereal$per ~ per_cereal$date, ylab = "% Posiciones en cereal", xlab = "", xaxt = "n", las = 2) # Use vector "names" as well for xlabs 
abline(v = 131.5, lwd = 2)
abline(v = 225.1, lwd = 2)
mtext(dates, side = 1, line = 0.5, at = coordx, las = 2)
polcoord <- cbind(c(res[77], res[77], res[137], res[137]), c(0,43, 43, 0))
polygon(polcoord[,1], polcoord[,2], col = NA, border = "red", lwd = 1.5)
dev.off()

# Check posiciones perdidas si quitamos del 1 al 7 de Marzo

which(rownames(positions_cereal) == 14929) # 1Marzo
which(rownames(positions_cereal) == 17190) # 7Marzo

delete1 <- positions_cereal[1710:1970, ] 
nrow(delete1)/nrow(positions_cereal)*100

# Check posiciones perdidas si quitamos del 1 al 10 de Junio
which(rownames(positions_cereal) == 629) # 1Junio
which(rownames(positions_cereal) == 18581)

delete2 <- positions_cereal[11439:12654, ]
nrow(delete2)/nrow(positions_cereal)*100

# Total:
sum(nrow(delete2),nrow(delete1))/nrow(positions_cereal)*100

# ---- Check distribution PER YEARS ----

# ---- 2017 ----

positions_cereal17 <- positions_cereal[which(positions_cereal$Year == 2017), ]
res17 <- barplot(table(positions_cereal2017$date), xaxs="i",xaxt="n") # To get midpoints
f17 <- table(positions_cereal$date)

positions_cereal17$cereal <- as.factor(positions_cereal17$cereal) # Need to be factor to not drop levels with empty observations (keep the 0 where there is no positions in cereal)

data_cereal17 <- positions_cereal17 %>%
  group_by(date, cereal, .drop = FALSE) %>%
  summarise(count = n() )

per_cereal17 <- data_cereal17 %>%
  group_by(date) %>%
  mutate(countT= sum(count)) %>%
  group_by(cereal, add=TRUE) %>%
  mutate(per=round(100*count/countT,2))

per_cereal17 <- per_cereal17[which(per_cereal17$cereal == 1), ]

per_cereal17$date <- factor(per_cereal17$date, levels = unique(per_cereal17$date)) # change factor to sort factor levels

# ---- 2018 ----

positions_cereal18 <- positions_cereal[which(positions_cereal$Year == 2018), ]
res18 <- barplot(table(positions_cereal18$date), xaxs="i",xaxt="n") # To get midpoints
f18 <- table(positions_cereal$date)

positions_cereal18$cereal <- as.factor(positions_cereal18$cereal) # Need to be factor to not drop levels with empty observations (keep the 0 where there is no positions in cereal)

data_cereal18 <- positions_cereal18 %>%
  group_by(date, cereal, .drop = FALSE) %>%
  summarise(count = n() )

per_cereal18 <- data_cereal18 %>%
  group_by(date) %>%
  mutate(countT= sum(count)) %>%
  group_by(cereal, add=TRUE) %>%
  mutate(per=round(100*count/countT,2))

per_cereal18 <- per_cereal18[which(per_cereal18$cereal == 1), ]

per_cereal18$date <- factor(per_cereal18$date, levels = unique(per_cereal18$date)) # change factor to sort factor levels

# ---- 2019 ----

positions_cereal19 <- positions_cereal[which(positions_cereal$Year == 2019), ]
res19 <- barplot(table(positions_cereal19$date), xaxs="i",xaxt="n") # To get midpoints
f19 <- table(positions_cereal$date)

positions_cereal19$cereal <- as.factor(positions_cereal19$cereal) # Need to be factor to not drop levels with empty observations (keep the 0 where there is no positions in cereal)

data_cereal19 <- positions_cereal19 %>%
  group_by(date, cereal, .drop = FALSE) %>%
  summarise(count = n() )

per_cereal19 <- data_cereal19 %>%
  group_by(date) %>%
  mutate(countT= sum(count)) %>%
  group_by(cereal, add=TRUE) %>%
  mutate(per=round(100*count/countT,2))

per_cereal19 <- per_cereal19[which(per_cereal19$cereal == 1), ]

per_cereal19$date <- factor(per_cereal19$date, levels = unique(per_cereal19$date)) # change factor to sort factor levels

# ---- PLOT ----

setwd("D:/PhD/Fourth chapter/Results/Preliminar results")
pdf("posiciones_cereal_dataNDVI_years.pdf")
par(mfrow = c(3,1))
#2017
barplot(per_cereal17$per ~ per_cereal17$date, ylab = "% Posiciones en cereal", xlab = "", xaxt = "n", las = 2, main = "2017") # Use vector "names" as well for xlabs 
abline(v = 131.5, lwd = 2)
abline(v = 225.1, lwd = 2)
mtext(dates, side = 1, line = 0.5, at = coordx, las = 2)
polcoord <- cbind(c(res[63], res[63], res[151], res[151]), c(0,43, 43, 0))
polygon(polcoord[,1], polcoord[,2], col = NA, border = "red", lwd = 1.5)
#2018
barplot(per_cereal18$per ~ per_cereal18$date, ylab = "% Posiciones en cereal", xlab = "", xaxt = "n", las = 2, main = "2018") # Use vector "names" as well for xlabs 
abline(v = 131.5, lwd = 2)
abline(v = 225.1, lwd = 2)
mtext(dates, side = 1, line = 0.5, at = coordx, las = 2)
polcoord <- cbind(c(res[63], res[63], res[151], res[151]), c(0,43, 43, 0))
polygon(polcoord[,1], polcoord[,2], col = NA, border = "red", lwd = 1.5)
#2019
barplot(per_cereal19$per ~ per_cereal19$date, ylab = "% Posiciones en cereal", xlab = "", xaxt = "n", las = 2, main = "2019") # Use vector "names" as well for xlabs 
abline(v = 131.5, lwd = 2)
abline(v = 225.1, lwd = 2)
mtext(dates, side = 1, line = 0.5, at = coordx, las = 2)
polcoord <- cbind(c(res[63], res[63], res[151], res[151]), c(0,43, 43, 0))
polygon(polcoord[,1], polcoord[,2], col = NA, border = "red", lwd = 1.5)
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
