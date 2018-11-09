
# ==================================================================================
#                        EXTRACT BUFFER CALCULATING % OF AES AND SG
# ==================================================================================

rm(list=ls())

library(rgdal)
library(rgeos)
library(raster)
library(dplyr)

# ---- Load data ----

# Transects
tr <- readOGR("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS", "Transects_2010_2017_EPSG23031") # Contains transects sampled each year (1/0)

# AES
aes10 <- readOGR("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/AES/Only AES", layer = "AES_2010_EPSG23031")
aes11 <- readOGR("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/AES/Only AES", layer = "AES_2011_EPSG23031")
aes12 <- readOGR("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/AES/Only AES", layer = "AES_2012_EPSG23031")
aes13 <- readOGR("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/AES/Only AES", layer = "AES_2013_EPSG23031")
aes14 <- readOGR("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/AES/Only AES", layer = "AES_2014_EPSG23031")
aes15 <- readOGR("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/AES/Only AES", layer = "AES_2015_EPSG23031")
aes16 <- readOGR("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/AES/Only AES", layer = "AES_2016_EPSG23031")
aes17 <- readOGR("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/AES/Only AES", layer = "AES_2017_EPSG23031")

# SG
sg14 <- readOGR("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/SG", layer = "SG_2014_EPSG23031")
sg15 <- readOGR("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/SG", layer = "SG_2015_EPSG23031")
sg16 <- readOGR("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/SG", layer = "SG_2016_EPSG23031")
sg17 <- readOGR("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/SG", layer = "SG_2017_EPSG23031")


# ---- Create buffers and calculate area ----

buf <- gBuffer(tr, byid = TRUE, width = 500)
writeOGR(buf, dsn = "C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS", layer = "Buffer_transects", driver = "ESRI Shapefile")

layers <- list(aes10, aes11, aes12, aes13, aes14, aes15, aes16, aes17, sg14, sg15, sg16, sg17)
layers_names <- c("aes10", "aes11", "aes12", "aes13", "aes14", "aes15", "aes16", "aes17", "sg14", "sg15", "sg16", "sg17")


management <- as.data.frame(matrix(NA, nrow = nrow(buf), ncol = 1))
colnames(management)[1] <- "Codi"
management$Codi <- buf@data$Codi

for (i in 1:length(layers)){
  poli <- intersect(buf, layers[[i]]) # Intersect buffers with management fields polygons
  poli$m2_intersect_buffer <- area(poli) # Calculate area of what falls in the buffer (in m2)
  transect_area <- aggregate(m2_intersect_buffer ~ Codi, data = poli, FUN = sum) # Sum area of polygons belonging to a buffer
  colnames(transect_area)[2] <- paste("area", layers_names[i], sep = "_") # Change column name to store it
  management <- left_join(management, transect_area, by = "Codi") # Store area
  management[is.na(management)] <- 0 # Substitute NA by 0
  
}

poli <- intersect(buf, aes10)
buf@data
aes10@data
poli@data

plot(poli)
plot(buf[1, ])
plot(area_aes_10, add = TRUE)

plot(buf2)
plot(buf2[1, ])
plot(tr[1, ], add = T)

for (i in 1:nrow(tr@data)) {
  
}
