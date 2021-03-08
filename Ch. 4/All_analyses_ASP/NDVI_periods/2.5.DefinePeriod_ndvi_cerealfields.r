
rm(list=ls())

library(rgdal)
library(raster)
library(dplyr)
library(sp)
library(rgeos)

# Calculate NDVI within cereal fields used during the period Feb-Ap

####################################################################
##################        2017        ##############################
####################################################################

# 1. ---- Identify cereal fields with > 5 positions ----

# Load positions and take positions in cereal in 2017

setwd("D:/PhD/Fourth chapter/GIS/Capas_Rocío/GPS")
pos <- read.csv("positions_cereal.csv")
pos17 <- pos[which(pos$Year == 2017), ]
#pos17_cereal <- pos17[which(pos17$cereal == 1), ]

coordinates(pos17) <- ~Longitude+Latitude
crs(pos17) <- "+proj=longlat +ellps=GRS80 +no_defs"
#plot(pos17)


# Campos cereal 2017

#dun17 <- readOGR("D:/PhD/Fourth chapter/GIS/Capas_Rocío/usos", "clip_dun17_usos_4326")
dun17 <- readOGR("D:/PhD/Fourth chapter/GIS/Capas_Rocío/usos", "clip_dun17_usos_WGS_1984_UTM_Zone_31N") # Load this one (I need UTM to do the gBuffer)

cereal17 <- dun17[which(dun17$uso == "CEREAL"), ]
cereal17$ID_field <- seq(1, nrow(cereal17@data))
  
crs(cereal17)
crs(pos17)

pos_2 <- spTransform(pos17, CRS = crs(cereal17))# Change CRS


#plot(cereal17)
#points(pos_2)

over_cereal <- over(pos_2, cereal17) # At the spatial location of pos2 retreives the position of cereal17
over_cereal <- over_cereal[complete.cases(over_cereal), ]

positions_cereal <- over_cereal %>%
  group_by(ID_field) %>%
  summarize(n())

# Remove fields with less than 5 positions
positions_cereal <- positions_cereal[-which(positions_cereal$`n()` < 5), ]

# Fields with more than 5 positions to extract mean NDVI
fields <- positions_cereal$ID_field 
cereal17_ndvi <- cereal17[which(cereal17$ID_field %in% fields), ]
#plot(cereal17_ndvi)

# Inner buffer to extract NDVI values

cereal17_ndvi_inner <- gBuffer(cereal17_ndvi, byid=TRUE, width = -10)
plot(cereal17_ndvi_inner)

# To check the buffer is good
# writeOGR(cereal17_ndvi_inner, "D:/PhD/Fourth chapter/GIS/Capas_Rocío/usos" , "cereal17_ndvi_inner", driver="ESRI Shapefile") 

#2.1 --- EXTRACT USED FIELDS AND PLOT ----

#LOAD RASTERS

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


ndvi <- list() # I comment this out so that it doesn't extract the values every time I run the script
for (i in 1:length(alldates)){
  ndvi[[i]] <- extract(allrasters[[i]], cereal17_ndvi_inner, df = TRUE, fun = mean)
}


ndvi2 <- do.call(cbind.data.frame, ndvi)
ndvi2 <- ndvi2[,-c(1,3,5,7,9,11)]

setwd("D:/PhD/Fourth chapter/Results/Preliminar results")
save(ndvi2, file = "ndvi_cereal_with_positions.RData")


#2.2 --- EXTRACT ALL FIELDS AND PLOT ----

rm(list=ls()) # LOAD EVERYTHING AGAIN BECAUSE THE FUNCTION EXTRACT DOESN'T WORK OTHERWISE

# Campos cereal 2017

#dun17 <- readOGR("D:/PhD/Fourth chapter/GIS/Capas_Rocío/usos", "clip_dun17_usos_4326")
dun17 <- readOGR("D:/PhD/Fourth chapter/GIS/Capas_Rocío/usos", "clip_dun17_usos_WGS_1984_UTM_Zone_31N") # Load this one (I need UTM to do the gBuffer)

cereal17 <- dun17[which(dun17$uso == "CEREAL"), ]
cereal17$ID_field <- seq(1, nrow(cereal17@data))

# Inner buffer a cereal17
#plot(cereal17)
all_cereal17_ndvi_inner <- gBuffer(cereal17, byid=TRUE, width = -10)
#plot(all_cereal17_ndvi_inner)

#LOAD RASTERS

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

# eXTRACT

ndvi_all <- list()
for (i in 1:length(alldates)){
  ndvi_all[[i]] <- extract(allrasters[[i]], all_cereal17_ndvi_inner, df = TRUE, fun = mean)
}

ndvi_all2 <- do.call(cbind.data.frame, ndvi_all)
ndvi_all2 <- ndvi_all2[,-c(1,3,5,7,9,11)]

setwd("D:/PhD/Fourth chapter/Results/Preliminar results")
save(ndvi_all2, file = "ndvi_cereal_ALL.RData")

# 3. --- PLOT ----

# Plot 1
setwd("D:/PhD/Fourth chapter/Results/Preliminar results")
load("ndvi_cereal_with_positions.RData")
load("ndvi_cereal_ALL.RData")

mean <- apply(ndvi2, 2, mean)
sd <- apply(ndvi2, 2, sd)
se <- apply(ndvi2, 2, sd) / sqrt(24)
lower <- mean - 1.96 * se
upper <- mean + 1.96 * se

df <- cbind(mean, lower, upper)

df <- as.data.frame(df[c(1,2,4,3,6,5), ])
rownames(df) <- gsub("X", "", rownames(df))
names <- rownames(df)
y <- seq(1,6)

setwd("D:/PhD/Fourth chapter/Results/Preliminar results")
pdf("ndvi_cereal.pdf")
par(mfrow = c(2,1), 
    mar = c(3, 4.1, 3, 2.1),
    oma = c(2,0,0,0))

plot(500, pch = 19, ylim = c(0.4, 0.9), xlim = c(1,6), axes = FALSE, xlab = " ", ylab = "NDVI", main = "Campos cereal con posiciones GPS (n = 24)")
points(df$mean, pch = 19)
axis(1, at = c(1, 2, 3, 4, 5, 6), labels = names, cex.axis = 1)
axis(2)
arrows(y, df$lower, y, df$upper, code = 3, angle = 90, length = 0.1)
points(df$mean, pch = 19, type = "l")
abline(v = 4.5, lwd = 2)
abline(v = 2.5, lwd = 2, col = "blue")


# Plot 2

mean <- apply(ndvi_all2, 2, mean)
sd <- apply(ndvi_all2, 2, sd)
se <- apply(ndvi_all2, 2, sd) / sqrt(24)
lower <- mean - 1.96 * se
upper <- mean + 1.96 * se

df2 <- cbind(mean, lower, upper)

df2 <- as.data.frame(df2[c(1,2,4,3,6,5), ])
rownames(df2) <- gsub("X", "", rownames(df2))
names <- rownames(df2)
y <- seq(1,6)


plot(500, pch = 19, ylim = c(0.4, 0.9), xlim = c(1,6), axes = FALSE, xlab = " ", ylab = "NDVI", main = "Todos campos cereal del area de estudio (n = 745)")
points(df2$mean, pch = 19)
axis(1, at = c(1, 2, 3, 4, 5, 6), labels = names, cex.axis = 1)
axis(2)
arrows(y, df2$lower, y, df2$upper, code = 3, angle = 90, length = 0.1)
points(df2$mean, pch = 19, type = "l")
abline(v = 4.5, lwd = 2)
abline(v = 2.5, lwd = 2, col = "blue")

mtext("Fecha (Imágenes NDVI)", line = 0, side = 1,  outer = TRUE)
dev.off()

#plot(cereal17)
#plot(cereal17_ndvi, add = TRUE, col = "red")
#points(pos_2, pch = 19, col = "green")



####################################################################
##################    2017-2018-2019      ##########################
####################################################################

library(adehabitatHR)

# MCP 99 to crop cereal fields 
pos <- readOGR("XYD_gangas_no_regadio_ETRS89_tot.shp")
pos <- pos[-which(pos$Year == 2016), ] 
pos$ID_pos <- seq(1,nrow(pos))


mcp_99 <- mcp(pos[,1], percent = 99) # Create MCP for each territory
plot(mcp_99)
points(pos)
writeOGR(mcp_99, "D:/PhD/Fourth chapter/GIS/Capas_Rocío/GPS", "mcp99_final", "D:/PhD/Fourth chapter/GIS/Capas_Rocío/GPS", driver = "ESRI Shapefile")

# Load cropped cereal fields in arcgis
dun17 <- readOGR("D:/PhD/Fourth chapter/GIS/Capas_Rocío/usos", "clipMCP_dun17_usos_WGS_1984_UTM_Zone_31N") # Load this one (I need UTM to do the gBuffer)
dun18 <- readOGR("D:/PhD/Fourth chapter/GIS/Capas_Rocío/usos", "clipMCP_dun18_usos_WGS_1984_UTM_Zone_31N") # Load this one (I need UTM to do the gBuffer)
dun19 <- readOGR("D:/PhD/Fourth chapter/GIS/Capas_Rocío/usos", "clipMCP_dun19_usos_WGS_1984_UTM_Zone_31N") # Load this one (I need UTM to do the gBuffer)

# Select cereal
cereal17 <- dun17[which(dun17$uso == "CEREAL"), ]
cereal17$ID_field <- seq(1, nrow(cereal17@data))

cereal18 <- dun18[which(dun18$uso == "CEREAL"), ]
cereal18$ID_field <- seq(1, nrow(cereal18@data))

cereal19 <- dun19[which(dun19$uso == "CEREAL"), ]
cereal19$ID_field <- seq(1, nrow(cereal19@data))

# Inner buffer 

all_cereal17_ndvi_inner <- gBuffer(cereal17, byid=TRUE, width = -10)
all_cereal18_ndvi_inner <- gBuffer(cereal18, byid=TRUE, width = -10)
all_cereal19_ndvi_inner <- gBuffer(cereal19, byid=TRUE, width = -10)


# LOAD RASTERS AND SAVE NDVI

# 2017 ----

rastlist <- list.files(path = "D:/PhD/Fourth chapter/GIS/NDVI/Marc/NDVI_real/Todas17", pattern='.tif$', 
                       all.files=TRUE, full.names=TRUE)
allrasters <- lapply(rastlist, raster)
crs(allrasters[[1]])
# Generate dates attribute (NECESSARY TO HAVE FILES IN THE FOLLOWING FORMAT: "2017-2-25.tif" (year-mes-day.tif)
allnames <- NULL # Create vector with all names
for (i in 1:length(allrasters)){
  allnames <- c(allnames, names(allrasters[[i]]))
}

alldates <- NULL 
for (i in 1:length(allnames)){
  allnames[[i]] <- gsub('\\.', '-', allnames[[i]])
  alldates[[i]] <- as.Date(substr(allnames[[i]], 2, 10))}

# eXTRACT

ndvi_all <- list()
for (i in 1:length(alldates)){
  ndvi_all[[i]] <- extract(allrasters[[i]], all_cereal17_ndvi_inner, df = TRUE, fun = mean)
}

ndvi_all17 <- do.call(cbind.data.frame, ndvi_all)
ndvi_all17 <- ndvi_all17[,-c(1,3,5,7,9,11)]

setwd("D:/PhD/Fourth chapter/Results/Preliminar results")
save(ndvi_all17, file = "ndvi_cereal_ALL17.RData")

# 2018 ----

rastlist <- list.files(path = "D:/PhD/Fourth chapter/GIS/NDVI/Marc/NDVI_real/Todas18", pattern='.tif$', 
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

# eXTRACT

ndvi_all <- list()
for (i in 1:length(alldates)){
  ndvi_all[[i]] <- extract(allrasters[[i]], all_cereal18_ndvi_inner, df = TRUE, fun = mean)
}

ndvi_all18 <- do.call(cbind.data.frame, ndvi_all)
ndvi_all18 <- ndvi_all18[,-c(1,3,5,7,9,11,13)]

setwd("D:/PhD/Fourth chapter/Results/Preliminar results")
save(ndvi_all18, file = "ndvi_cereal_ALL18.RData")

# 2019 ----

rastlist <- list.files(path = "D:/PhD/Fourth chapter/GIS/NDVI/Marc/NDVI_real/Todas19", pattern='.tif$', 
                       all.files=TRUE, full.names=TRUE)
allrasters <- lapply(rastlist, raster)
crs(allrasters[[1]])

# Generate dates attribute (NECESSARY TO HAVE FILES IN THE FOLLOWING FORMAT: "2017-2-25.tif" (year-mes-day.tif)
allnames <- NULL # Create vector with all names
for (i in 1:length(allrasters)){
  allnames <- c(allnames, names(allrasters[[i]]))
}

alldates <- NULL 
for (i in 1:length(allnames)){
  allnames[[i]] <- gsub('\\.', '-', allnames[[i]])
  alldates[[i]] <- as.Date(substr(allnames[[i]], 2, 10))}

# eXTRACT

ndvi_all <- list()
for (i in 1:length(alldates)){
  ndvi_all[[i]] <- extract(allrasters[[i]], all_cereal19_ndvi_inner, df = TRUE, fun = mean)
}

ndvi_all19 <- do.call(cbind.data.frame, ndvi_all)
ndvi_all19 <- ndvi_all19[,-c(1,3,5,7,9,11,13,15)]

setwd("D:/PhD/Fourth chapter/Results/Preliminar results")
save(ndvi_all17, file = "ndvi_cereal_ALL19.RData")

#####
# PLOT

setwd("D:/PhD/Fourth chapter/Results/Preliminar results")
load("ndvi_cereal_ALL17.RData")
load("ndvi_cereal_ALL18.RData")
load("ndvi_cereal_ALL19.RData")

# Remove images that are not valid because of clouds (bad NDVI)
# 8-14 Marzo 2018
ndvi_all18 <- ndvi_all18[,-4]
ndvi_all19 <- ndvi_all19[,-c(6,8)]


setwd("D:/PhD/Fourth chapter/Results/Preliminar results")
pdf("ndvi_cereal1719.pdf")

par(mfrow = c(3,1), 
    mar = c(3, 4.1, 3, 2.1),
    oma = c(2,0,0,0))

# 2017
mean <- apply(ndvi_all17, 2, mean)
sd <- apply(ndvi_all17, 2, sd)
se <- apply(ndvi_all17, 2, sd) / sqrt(24)
lower <- mean - 1.96 * se
upper <- mean + 1.96 * se

df <- cbind(mean, lower, upper)

df <- as.data.frame(df[c(1,2,4,3,6,5), ])
rownames(df) <- gsub("X", "", rownames(df))
names <- rownames(df)
y <- seq(1,6)


plot(500, pch = 19, ylim = c(0.4, 0.9), xlim = c(1,6), axes = FALSE, xlab = " ", ylab = "NDVI", main = "Campos cereal en MCP 2017")
points(df$mean, pch = 19)
axis(1, at = c(1, 2, 3, 4, 5, 6), labels = names, cex.axis = 1)
axis(2)
arrows(y, df$lower, y, df$upper, code = 3, angle = 90, length = 0.1)
points(df$mean, pch = 19, type = "l")
abline(v = 4.5, lwd = 2)
abline(v = 2.5, lwd = 2, col = "blue")

# 2018
mean <- apply(ndvi_all18, 2, mean, na.rm=TRUE)
sd <- apply(ndvi_all18, 2, sd, na.rm=TRUE)
se <- apply(ndvi_all18, 2, sd, na.rm=TRUE) / sqrt(24)
lower <- mean - 1.96 * se
upper <- mean + 1.96 * se

df <- cbind(mean, lower, upper)

df <- as.data.frame(df)
rownames(df) <- gsub("X", "", rownames(df))
rownames(df) <- gsub("_", "-", rownames(df))
names <- rownames(df)
y <- seq(1,6)


plot(500, pch = 19, ylim = c(0.25, 0.8), xlim = c(1,7), axes = FALSE, xlab = " ", ylab = "NDVI", main = "Campos cereal en MCP 2018")
points(df$mean, pch = 19)
axis(1, at = c(1, 2, 3, 4, 5, 6), labels = names, cex.axis = 1)
axis(2)
arrows(y, df$lower, y, df$upper, code = 3, angle = 90, length = 0.1)
points(df$mean, pch = 19, type = "l")
abline(v = 2.8, lwd = 2)
abline(v = 2, lwd = 2, col = "blue")

# 2019
mean <- apply(ndvi_all19, 2, mean, na.rm=TRUE)
sd <- apply(ndvi_all19, 2, sd, na.rm=TRUE)
se <- apply(ndvi_all19, 2, sd, na.rm=TRUE) / sqrt(24)
lower <- mean - 1.96 * se
upper <- mean + 1.96 * se

df <- cbind(mean, lower, upper)

df <- as.data.frame(df[c(1,2,5,3,4,6), ])
rownames(df) <- gsub("X", "", rownames(df))
rownames(df) <- gsub("_", "-", rownames(df))
names <- rownames(df)
y <- seq(1,6)


plot(500, pch = 19, ylim = c(0.4, 0.9), xlim = c(1,6), axes = FALSE, xlab = " ", ylab = "NDVI", main = "Campos cereal en MCP 2019")
points(df$mean, pch = 19)
axis(1, at = c(1, 2, 3, 4, 5, 6), labels = names, cex.axis = 1)
axis(2)
arrows(y, df$lower, y, df$upper, code = 3, angle = 90, length = 0.1)
points(df$mean, pch = 19, type = "l")
abline(v = 4, lwd = 2)
abline(v = 2, lwd = 2, col = "blue")

dev.off()
