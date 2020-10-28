
rm(list=ls())

library(rgdal)
library(raster)
library(dplyr)
library(sp)
library(rgeos)

# Calculate NDVI within cereal fields used during the period Feb-Ap

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

# 2. ---- Extract NDVI ----

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

#3.1 --- EXTRACT USED FIELDS AND PLOT ----

ndvi <- list()
for (i in 1:length(alldates)){
  ndvi[[i]] <- extract(allrasters[[i]], cereal17_ndvi_inner, df = TRUE, fun = mean)
}

ndvi2 <- do.call(cbind.data.frame, ndvi)
ndvi2 <- ndvi2[,-c(1,3,5,7,9,11)]

# Plot

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


plot(500, pch = 19, ylim = c(0.4, 0.9), xlim = c(1,6), axes = FALSE, xlab = "Fecha (Imágenes NDVI)", ylab = "NDVI", main = "Campos cereal con posiciones")
points(df$mean, pch = 19)
axis(1, at = c(1, 2, 3, 4, 5, 6), labels = names, cex.axis = 1)
axis(2)
arrows(y, df$lower, y, df$upper, code = 3, angle = 90, length = 0.1)
points(df$mean, pch = 19, type = "l", col = "red")
abline(v = 4.5, lwd = 2)


#3.2 --- EXTRACT ALL FIELDS AND PLOT ----

# Inner buffer a cereal17
plot(cereal17)
all_cereal17_ndvi_inner <- gBuffer(cereal17, byid=TRUE, width = -10)
plot(all_cereal17_ndvi_inner)

ndvi_all <- list()
for (i in 1:length(alldates)){
  ndvi_all[[i]] <- extract(allrasters[[i]], all_cereal17_ndvi_inner, df = TRUE, fun = mean)
}

ndvi_all2 <- do.call(cbind.data.frame, ndvi_all)
ndvi_all2 <- ndvi_all2[,-c(1,3,5,7,9,11)]

# Plot

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


plot(500, pch = 19, ylim = c(0.4, 0.9), xlim = c(1,6), axes = FALSE, xlab = "Fecha (Imágenes NDVI)", ylab = "NDVI", main = "Campos cereal con posiciones")
points(df$mean, pch = 19)
axis(1, at = c(1, 2, 3, 4, 5, 6), labels = names, cex.axis = 1)
axis(2)
arrows(y, df$lower, y, df$upper, code = 3, angle = 90, length = 0.1)
points(df$mean, pch = 19, type = "l", col = "red")
abline(v = 4.5, lwd = 2)


