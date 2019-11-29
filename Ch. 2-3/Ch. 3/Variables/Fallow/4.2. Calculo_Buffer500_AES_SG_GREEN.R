
# ==================================================================================
#                        EXTRACT BUFFER 500 M CALCULATING Ha OF SG-AES-GREENING
# ==================================================================================

rm(list=ls())

library(rgdal)
library(rgeos)
library(raster)
library(dplyr)

# ---- Load data (layers without overlap) ----

# Transects
tr <- readOGR("C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS", "Trans_2018_EPSG23031") # Contains transects sampled each year (1/0)

# SG

sg14 <- readOGR("C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Fallow/SG", layer = "SG_2014_EPSG23031")
colnames(sg14@data)[colnames(sg14@data) == "Codi"] <- "Codi.2"
sg15 <- readOGR("C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Fallow/SG", layer = "SG_2015_EPSG23031")
sg16 <- readOGR("C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Fallow/SG", layer = "SG_2016_EPSG23031")
sg17 <- readOGR("C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Fallow/SG", layer = "SG_2017_EPSG23031")
#sg18 <- readOGR("C:/Users/ana.sanz/Documents/PhD/Second chapter/Data/GIS/SG", layer = "SG_2018_EPSG23031")
#sg19 <- readOGR("C:/Users/ana.sanz/Documents/PhD/Second chapter/Data/GIS/SG", layer = "SG_2019_EPSG23031")


# AES 

aes14 <- readOGR("C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Fallow/AES", layer = "AEScutted_2014_EPSG23031")
aes15 <- readOGR("C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Fallow/AES", layer = "AEScutted_2015_EPSG23031")
aes16 <- readOGR("C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Fallow/AES", layer = "AEScutted_2016_EPSG23031")
aes17 <- readOGR("C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Fallow/AES", layer = "AEScutted_2017_EPSG23031")
#aes18 <- readOGR("C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Fallow/AES", layer = "AEScutted_2018_EPSG23031")
#aes19 <- readOGR("C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Fallow/AES", layer = "AEScutted_2019_EPSG23031")


# GREEN

#?green14 <- readOGR("C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Fallow/Greening", layer = "GREENcutted_2014_EPSG23031")
green15 <- readOGR("C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Fallow/Greening", layer = "GREENcutted_2015_EPSG23031")
green16 <- readOGR("C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Fallow/Greening", layer = "GREENcutted_2016_EPSG23031")
green17 <- readOGR("C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Fallow/Greening", layer = "GREENcutted_2017_EPSG23031")
#green18 <- readOGR("C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Fallow/Greening", layer = "GREENcutted_2018_EPSG23031")
#green19 <- readOGR("C:/Users/ana.sanz/Documents/PhD/Third chapter/GIS/Fallow/Greening", layer = "GREENcutted_2019_EPSG23031")

# ---- Create buffers and calculate area ----

buf <- gBuffer(tr, byid = TRUE, width = 500)
#writeOGR(buf, dsn = "C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS", layer = "Buffer_transects", driver = "ESRI Shapefile")

# Data frame to fill up:
management <- as.data.frame(matrix(NA, nrow = nrow(buf), ncol = 1))
colnames(management)[1] <- "Codi"
management$Codi <- buf@data$Codi

# ---- AES ----
#### 2014-2015 ----

# Calculate proportional area, because the strip of fallow is not digital 
# So calculate the HA_Fallow proportional to the intersected area

layers <- list(aes14, aes15)
layers_names <- c("aes14", "aes15")


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

#### 2016 & 2017 ----

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
#### 2018 & 2019 : FALTA ----
# ---- SG 14-18 ----

layers <- list(sg14, sg15, sg16, sg17)
layers_names <- c("sg14", "sg15", "sg16", "sg17")

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

# ---- GREENING ----
#### 2014: Falta (???) ####
#### 2015 - 2017 ####

layers <- list(green15, green16, green17)
layers_names <- c("green15", "green16", "green17")

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

management <- management[ ,-c(2,6)] # Remove 2014

# Save
setwd("C:/Users/ana.sanz/Documents/PhD/Third chapter/Data")
write.csv(management, "management_area_15_17.csv")


