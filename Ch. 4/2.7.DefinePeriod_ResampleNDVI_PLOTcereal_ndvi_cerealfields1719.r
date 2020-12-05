rm(list=ls())

library(rgdal)
library(raster)
library(dplyr)
library(sp)
library(rgeos)
library(adehabitatHR)

## -------------------------------------------------
##                Final NDVI analysis
## Resample NDVI layers and make final graph for 2017-2019
## ------------------------------------------------- 


## ---- 1. Moving window  ----

r1 <- raster(ncols=36, nrows=18, xmn=0)
values(r1) <- runif(ncell(r1)) 
plot(r1)

prop.hab<-function(r1,gri=3,na.rm=TRUE){
  require(raster)
  m <- matrix(rep(1,(gri*gri) ), byrow=T,nrow=gri)
  foc <- median(c(1:(gri*gri)))
  gri1  <- gri*gri
  func <- function(x) (sum(x)/(gri1) )
  r2<-focal(r1,m,fun=func)
  
  return(r2)
}
plot(r2)

# 2017 ----

rastlist <- list.files(path = "D:/PhD/Fourth chapter/Data/GIS/NDVI/Marc/NDVI_real/Todas17", pattern='.tif$', 
                       all.files=TRUE, full.names=TRUE)
allrasters <- lapply(rastlist, raster)
crs(allrasters[[1]])


allnames <- NULL # Create vector with all names
for (i in 1:length(allrasters)){
  allnames <- c(allnames, names(allrasters[[i]]))
}

alldates <- NULL 
for (i in 1:length(allnames)){
  allnames[[i]] <- gsub('\\.', '-', allnames[[i]])
  allnames[[i]] <- substr(allnames[[i]], 2, 10)
  alldates[[i]] <- as.Date(allnames[[i]])}

setwd("D:/PhD/Fourth chapter/Data/GIS/NDVI/Marc/NDVI_real/MovWin/Todas17")

for (i in 1:length(allrasters)){
ndvi_pro <- prop.hab(allrasters[[i]], gri = 3)
writeRaster(ndvi_pro, filename = allnames[[i]], format='GTiff', overwrite=TRUE)
}

# 2018 ----

rastlist <- list.files(path = "D:/PhD/Fourth chapter/Data/GIS/NDVI/Marc/NDVI_real/Todas18", pattern='.tif$', 
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


setwd("D:/PhD/Fourth chapter/Data/GIS/NDVI/Marc/NDVI_real/MovWin/Todas18")

for (i in 1:length(allrasters)){
  ndvi_pro <- prop.hab(allrasters[[i]], gri = 3)
  writeRaster(ndvi_pro, filename = allnames[[i]], format='GTiff', overwrite=TRUE)
}

# 2019 ----

rastlist <- list.files(path = "D:/PhD/Fourth chapter/Data/GIS/NDVI/Marc/NDVI_real/Todas19", pattern='.tif$', 
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


setwd("D:/PhD/Fourth chapter/Data/GIS/NDVI/Marc/NDVI_real/MovWin/Todas19")

for (i in 1:length(allrasters)){
  ndvi_pro <- prop.hab(allrasters[[i]], gri = 3)
  writeRaster(ndvi_pro, filename = allnames[[i]], format='GTiff', overwrite=TRUE)
}

## ---- 2. Select cereal (inner buffer) in layers  ----

setwd("D:/PhD/Fourth chapter/Data/GIS/Capas_Rocío/GPS")
# MCP 99 to crop cereal fields 
pos <- readOGR("XYD_gangas_no_regadio_ETRS89_tot.shp")
# pos <- pos[-which(pos$Year == 2016), ] 
pos$ID_pos <- seq(1,nrow(pos))


mcp_99 <- mcp(pos[,1], percent = 99) # Create MCP for each territory
plot(mcp_99)
points(pos)
# writeOGR(mcp_99, "D:/PhD/Fourth chapter/GIS/Capas_Rocío/GPS", "mcp99_final", "D:/PhD/Fourth chapter/GIS/Capas_Rocío/GPS", driver = "ESRI Shapefile")

# Load cropped cereal fields in arcgis
dun17 <- readOGR("D:/PhD/Fourth chapter/Data/GIS/Capas_Rocío/usos", "clipMCP_dun17_usos_WGS_1984_UTM_Zone_31N") # Load this one (I need UTM to do the gBuffer)
dun18 <- readOGR("D:/PhD/Fourth chapter/Data/GIS/Capas_Rocío/usos", "clipMCP_dun18_usos_WGS_1984_UTM_Zone_31N") # Load this one (I need UTM to do the gBuffer)
dun19 <- readOGR("D:/PhD/Fourth chapter/Data/GIS/Capas_Rocío/usos", "clipMCP_dun19_usos_WGS_1984_UTM_Zone_31N") # Load this one (I need UTM to do the gBuffer)

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


## ---- 3. Load re-sampled rasters and extract NDVI from inner buffers ----

# 2017 ----

rastlist <- list.files(path = "D:/PhD/Fourth chapter/Data/GIS/NDVI/Marc/NDVI_real/MovWin/Todas17", pattern='.tif$', 
                       all.files=TRUE, full.names=TRUE)
allrasters <- lapply(rastlist, raster)

allnames <- NULL # Create vector with all names
for (i in 1:length(allrasters)){
  allnames <- c(allnames, names(allrasters[[i]]))}

alldates <- NULL 
for (i in 1:length(allnames)){
  allnames[[i]] <- gsub('\\.', '-', allnames[[i]])
  allnames[[i]] <- substr(allnames[[i]], 2, 10)
  alldates[[i]] <- as.Date(allnames[[i]])}

# EXTRACT

ndvi_all <- list()
for (i in 1:length(alldates)){
  ndvi_all[[i]] <- extract(allrasters[[i]], all_cereal17_ndvi_inner, df = TRUE, fun = mean)
}

ndvi_all17 <- do.call(cbind.data.frame, ndvi_all)
ndvi_all17 <- ndvi_all17[,-c(1,3,5,7,9,11)]

setwd("D:/PhD/Fourth chapter/Data/GIS/NDVI/Marc/NDVI_real/MovWin")
save(ndvi_all17, file = "ndvi_cereal_ALL17.RData")

# 2018 ----

rastlist <- list.files(path = "D:/PhD/Fourth chapter/Data/GIS/NDVI/Marc/NDVI_real/MovWin/Todas18", pattern='.tif$', 
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
# EXTRACT

ndvi_all <- list()
for (i in 1:length(alldates)){
  ndvi_all[[i]] <- extract(allrasters[[i]], all_cereal18_ndvi_inner, df = TRUE, fun = mean)
}

ndvi_all18 <- do.call(cbind.data.frame, ndvi_all)
ndvi_all18 <- ndvi_all18[,-c(1,3,5,7,9,11,13)]

setwd("D:/PhD/Fourth chapter/Data/GIS/NDVI/Marc/NDVI_real/MovWin")
save(ndvi_all18, file = "ndvi_cereal_ALL18.RData")

# 2019 ----

rastlist <- list.files(path = "D:/PhD/Fourth chapter/Data/GIS/NDVI/Marc/NDVI_real/MovWin/Todas19", pattern='.tif$', 
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

# EXTRACT

ndvi_all <- list()
for (i in 1:length(alldates)){
  ndvi_all[[i]] <- extract(allrasters[[i]], all_cereal19_ndvi_inner, df = TRUE, fun = mean)
}

ndvi_all19 <- do.call(cbind.data.frame, ndvi_all)
ndvi_all19 <- ndvi_all19[,-c(1,3,5,7,9,11,13,15)]

setwd("D:/PhD/Fourth chapter/Data/GIS/NDVI/Marc/NDVI_real/MovWin")
save(ndvi_all19, file = "ndvi_cereal_ALL19.RData")

## ---- 4. Plot ----


setwd("D:/PhD/Fourth chapter/Data/GIS/NDVI/Marc/NDVI_real/MovWin")
load("ndvi_cereal_ALL17.RData")
load("ndvi_cereal_ALL18.RData")
load("ndvi_cereal_ALL19.RData")

# Remove images that are not valid because of clouds (bad NDVI)
# 8-14 Marzo 2018
ndvi_all18 <- ndvi_all18[,-4]
ndvi_all19 <- ndvi_all19[,-c(6,8)]


setwd("D:/PhD/Fourth chapter/Results/Figures")
pdf("ndviResamp_cereal1719.pdf", 5, 7)

par(mfrow = c(3,1), 
    mar = c(3, 4.1, 3, 2.1),
    oma = c(2,3,0,2))

# 2017
mean <- apply(ndvi_all17, 2, mean)
sd <- apply(ndvi_all17, 2, sd)
se <- apply(ndvi_all17, 2, sd) / sqrt(24)
lower <- mean - 1.96 * se
upper <- mean + 1.96 * se

df <- cbind(mean, lower, upper)

df <- as.data.frame(df[c(1,2,4,3,6,5), ])
rownames(df) <- gsub("X", "", rownames(df))
# names <- rownames(df)
names <- c("15\nFebruary", "25\nFebruary", "7\nMarch", "17\nMarch", "6\nApril", "16\nApril")
y <- seq(1,6)


plot(500, pch = 19, ylim = c(0.3, 0.9), xlim = c(1,7), axes = FALSE, xlab = " ", ylab = " ", main = " ")
points(df$mean, pch = 19)
axis(1, at = c(1, 2, 3, 4, 5, 6), labels = names, cex.axis = 1, mgp=c(3, 1.7, 0))
axis(2)
arrows(y, df$lower, y, df$upper, code = 3, angle = 90, length = 0.1)
points(df$mean, pch = 19, type = "l")
abline(v = 2.5, lwd = 1.5, col = "red")
mtext("A", side = 3, line = 0.5, at = 1)
mtext("NDVI 2017", side = 2, line = 2.5, cex = 0.8)

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
# names <- rownames(df)
names <- c("15-22\nFeb", "1-7\nMarch", "23-31\nMarch", "1-7\nApril", "15-22\nApril", "23-30\nApril")
y <- seq(1,6)


plot(500, pch = 19, ylim = c(0.25, 0.8), xlim = c(1,7), axes = FALSE, xlab = " ", ylab = " ", main = " ")
points(df$mean, pch = 19)
axis(1, at = c(1, 2, 3, 4, 5, 6), labels = names, cex.axis = 1, mgp=c(3, 1.7, 0))
axis(2)
arrows(y, df$lower, y, df$upper, code = 3, angle = 90, length = 0.1)
points(df$mean, pch = 19, type = "l")
abline(v = 2, lwd = 1.5, col = "red")
mtext("B", side = 3, line = 0.5, at = 1)
mtext("NDVI 2018", side = 2, line = 2.5, cex = 0.8)

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
# names <- rownames(df)
names <- c("15-22\nFebruary", "1-7\nMarch", "8-14\nMarch", "15-22\nMarch", "23-31\nMarch", "15-22\nApril")
y <- seq(1,6)


plot(500, pch = 19, ylim = c(0.3, 0.8), xlim = c(1,7), axes = FALSE, xlab = " ", ylab = " ", main = " ")
points(df$mean, pch = 19)
axis(1, at = c(1, 2, 3, 4, 5, 6), labels = names, cex.axis = 1, mgp=c(3, 1.7, 0))
axis(2)
arrows(y, df$lower, y, df$upper, code = 3, angle = 90, length = 0.1)
points(df$mean, pch = 19, type = "l")
abline(v = 2, lwd = 1.5, col = "red")
mtext("C", side = 3, line = 0.5, at = 1)
mtext("NDVI 2019", side = 2, line = 2.5, cex = 0.8)

dev.off()
