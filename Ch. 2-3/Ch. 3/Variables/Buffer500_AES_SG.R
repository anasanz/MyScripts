
# ==================================================================================
#                        EXTRACT BUFFER 500 M CALCULATING % OF AES AND SG
# ==================================================================================

rm(list=ls())

library(rgdal)
library(rgeos)
library(raster)
library(dplyr)

# ---- Load data ----

# Transects
tr <- readOGR("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS", "Transects_2010_2017_EPSG23031") # Contains transects sampled each year (1/0)

# AES and fix fields where needed
aes10 <- readOGR("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS/AES/Only AES/EPSG23031", layer = "AES_2010_EPSG23031")
aes11 <- readOGR("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS/AES/Only AES/EPSG23031", layer = "AES_2011_EPSG23031")
colnames(aes11@data)[2] <- "HA_SP"
aes12 <- readOGR("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS/AES/Only AES/EPSG23031", layer = "AES_2012_EPSG23031")
colnames(aes12@data)[2] <- "HA_SP"
aes13 <- readOGR("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS/AES/Only AES/EPSG23031", layer = "AES_2013_EPSG23031")
aes14 <- readOGR("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS/AES/Only AES/EPSG23031", layer = "AES_2014_EPSG23031")
aes15 <- readOGR("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS/AES/Only AES/EPSG23031", layer = "AES_2015_EPSG23031")
aes16 <- readOGR("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS/AES/Only AES/EPSG23031", layer = "AES_2016_EPSG23031")
aes16 <- aes16[which(aes16@data$PROD_NOM == "FALLOW"), ] #...select fields with only fallows and see if the fucking orphan hole is fixed
#writeOGR(aes16, dsn = "C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS/AES/Only AES/EPSG23031", 
#         layer = "AES_2016_EPSG23031_FALLOW", driver = "ESRI Shapefile") # Save layer to make intersect in arcgis
aes16 <- readOGR("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS/AES/Only AES/EPSG23031", layer = "AES_2016_EPSG23031_FALLOW_intersect")
# Layer of 16 with the intersection already done
aes17 <- readOGR("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS/AES/Only AES/EPSG23031", layer = "AES_2017_EPSG23031")
aes17 <- aes17[which(aes17@data$PROD_NOM == "FALLOW"), ] #...select fields with only fallows and see if the fucking orphan hole is fixed


# SG
sg14 <- readOGR("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS/SG", layer = "SG_2014_EPSG23031")
colnames(sg14@data)[colnames(sg14@data) == "Codi"] <- "Codi.2"
sg15 <- readOGR("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS/SG", layer = "SG_2015_EPSG23031")
sg16 <- readOGR("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS/SG", layer = "SG_2016_EPSG23031")
sg17 <- readOGR("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS/SG", layer = "SG_2017_EPSG23031")
sg18 <- readOGR("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS/SG", layer = "SG_2018_EPSG23031")



# ---- Create buffers and calculate area ----

buf <- gBuffer(tr, byid = TRUE, width = 500)
#writeOGR(buf, dsn = "C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS", layer = "Buffer_transects", driver = "ESRI Shapefile")

# Data frame to fill up:
management <- as.data.frame(matrix(NA, nrow = nrow(buf), ncol = 1))
colnames(management)[1] <- "Codi"
management$Codi <- buf@data$Codi

# ---- AES ----
# ------ 2010-2015 ----

# Calculate proportional area, because the strip of fallow is not digital 
# So calculate the HA_Fallow proportional to the intersected area

layers <- list(aes10, aes11, aes12, aes13, aes14, aes15)
layers_names <- c("aes10", "aes11", "aes12", "aes13", "aes14", "aes15")


for (i in 1:length(layers)){
  
  poli <- raster::intersect(buf, layers[[i]]) # Intersect buffers with management fields polygons
  poli$ha_intersect_buffer <- area(poli)/10000 # Calculate area of what falls in the buffer (in ha)
  
  # Proportional intersecting area of fallow:
  poli$ha_intersect_fallow <- poli$ha_intersect_buffer*poli$HA_Fallow/poli$HA_SP 
  
  transect_area <- aggregate(ha_intersect_fallow ~ Codi, data = poli, FUN = sum) # Sum area of polygons belonging to a buffer
  
  colnames(transect_area)[2] <- paste("area", layers_names[i], sep = "_") # Change column name to store it
  management <- left_join(management, transect_area, by = "Codi") # Store area
  management[is.na(management)] <- 0 # Substitute NA by 0
}

# ------ 2016 & 2017 ----

# Strip of fallow is digitalized
# So only sum HA of Fallow fields

layers <- list(aes16, aes17)
layers_names <- c("aes16", "aes17")


for (i in 1:length(layers)){
  
  poli <- raster::intersect(buf, layers[[i]]) # Intersect buffers with management fields polygons
  poli$ha_intersect_buffer <- area(poli)/10000 # Calculate area of what falls in the buffer (in ha)
  
  # HERE NOTHING
  # Proportional intersecting area of fallow:
 # poli$ha_intersect_fallow <- poli$ha_intersect_buffer*poli$HA_Fallow/poli$HA_SP 
  
  transect_area <- aggregate(ha_intersect_buffer ~ Codi, data = poli, FUN = sum) # Sum area of polygons belonging to a buffer
  
  colnames(transect_area)[2] <- paste("area", layers_names[i], sep = "_") # Change column name to store it
  management <- left_join(management, transect_area, by = "Codi") # Store area
  management[is.na(management)] <- 0 # Substitute NA by 0
}

# ---- SG 14-18 ----

layers <- list(sg14, sg15, sg16, sg17, sg18)
layers_names <- c("sg14", "sg15", "sg16", "sg17", "sg18")

for (i in 1:length(layers)){
  
  poli <- raster::intersect(buf, layers[[i]]) # Intersect buffers with management fields polygons
  poli$ha_intersect_buffer <- area(poli)/10000 # Calculate area of what falls in the buffer (in ha)
  
  # HERE NOTHING
  # Proportional intersecting area of fallow:
  # poli$ha_intersect_fallow <- poli$ha_intersect_buffer*poli$HA_Fallow/poli$HA_SP 
  
  transect_area <- aggregate(ha_intersect_buffer ~ Codi, data = poli, FUN = sum) # Sum area of polygons belonging to a buffer
  
  colnames(transect_area)[2] <- paste("area", layers_names[i], sep = "_") # Change column name to store it
  management <- left_join(management, transect_area, by = "Codi") # Store area
  management[is.na(management)] <- 0 # Substitute NA by 0
}

# Save
setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data")
write.csv(management, "management_area_500.csv")


