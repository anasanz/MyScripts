
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
tr <- readOGR("C:/Users/Ana/Documents/PhD/Third chapter/GIS", "Trans_2018_EPSG23031") # Contains transects sampled each year (1/0)

# SG

#sg14 <- readOGR("C:/Users/Ana/Documents/PhD/Third chapter/GIS/Fallow/SG", layer = "SG_2014_EPSG23031")
#colnames(sg14@data)[colnames(sg14@data) == "Codi"] <- "Codi.2"
sg15 <- readOGR("C:/Users/Ana/Documents/PhD/Third chapter/GIS/Fallow/SG", layer = "SG_2015_EPSG23031")
sg16 <- readOGR("C:/Users/Ana/Documents/PhD/Third chapter/GIS/Fallow/SG", layer = "SG_2016_EPSG23031")
sg17 <- readOGR("C:/Users/Ana/Documents/PhD/Third chapter/GIS/Fallow/SG", layer = "SG_2017_EPSG23031")
sg18 <- readOGR("C:/Users/Ana/Documents/PhD/Third chapter/GIS/Fallow/SG", layer = "SG_2018_EPSG23031")
sg19 <- readOGR("C:/Users/Ana/Documents/PhD/Third chapter/GIS/Fallow/SG", layer = "SG_2019_EPSG23031")


# AES 

#aes14 <- readOGR("C:/Users/Ana/Documents/PhD/Third chapter/GIS/Fallow/AES", layer = "AEScutted_2014_EPSG23031")
aes15 <- readOGR("C:/Users/Ana/Documents/PhD/Third chapter/GIS/Fallow/AES", layer = "AEScutted_2015_EPSG23031")
aes16 <- readOGR("C:/Users/Ana/Documents/PhD/Third chapter/GIS/Fallow/AES", layer = "AEScutted_2016_EPSG23031")
aes17 <- readOGR("C:/Users/Ana/Documents/PhD/Third chapter/GIS/Fallow/AES", layer = "AEScutted_2017_EPSG23031")
aes18 <- readOGR("C:/Users/Ana/Documents/PhD/Third chapter/GIS/Fallow/AES", layer = "AEScutted_2018_EPSG23031")
aes19 <- readOGR("C:/Users/Ana/Documents/PhD/Third chapter/GIS/Fallow/AES", layer = "AEScutted_2019_EPSG23031")

# SELECT ONLY FALLOW FIELDS IN AES (In 2016 it was already done, and in 2015 is all together)
aes17 <- aes17[which(aes17$PROD_NOM == "FALLOW"), ]
aes18 <- aes18[which(aes18$PROD_NOM == "FALLOW"), ]
aes19 <- aes19[which(aes19$PROD_NOM == "FALLOW"), ]


# GREEN

#?green14 <- readOGR("C:/Users/Ana/Documents/PhD/Third chapter/GIS/Fallow/Greening", layer = "GREENcutted_2014_EPSG23031")
green15 <- readOGR("C:/Users/Ana/Documents/PhD/Third chapter/GIS/Fallow/Greening", layer = "GREENcutted_2015_EPSG23031")
green16 <- readOGR("C:/Users/Ana/Documents/PhD/Third chapter/GIS/Fallow/Greening", layer = "GREENcutted_2016_EPSG23031")
green17 <- readOGR("C:/Users/Ana/Documents/PhD/Third chapter/GIS/Fallow/Greening", layer = "GREENcutted_2017_EPSG23031")
green18 <- readOGR("C:/Users/Ana/Documents/PhD/Third chapter/GIS/Fallow/Greening", layer = "GREENcutted_2018_EPSG23031")
green19 <- readOGR("C:/Users/Ana/Documents/PhD/Third chapter/GIS/Fallow/Greening", layer = "GREENcutted_2019_EPSG23031")

# ---- Create buffers and calculate area ----

buf <- gBuffer(tr, byid = TRUE, width = 500)

# Data frame to fill up:
management <- as.data.frame(matrix(NA, nrow = nrow(buf), ncol = 1))
colnames(management)[1] <- "Codi"
management$Codi <- buf@data$Codi

# ---- AES ----
#### 2015 ----

# Calculate proportional area, because the strip of fallow is not digital 
# So calculate the HA_Fallow proportional to the intersected area

layers <- list(aes15)
layers_names <- c("aes15")


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
#### 2018 & 2019 (added afterwards) ---- 

# Strip of fallow is digitalized
# So only sum HA of Fallow fields

layers <- list(aes18, aes19)
layers_names <- c("aes18", "aes19")


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

#### SAVE ####
setwd("C:/Users/Ana/Documents/PhD/Third chapter/Data")
write.csv(management, "AES_15_19.csv")


# ---- SG 15-19 ----

# Data frame to fill up:

management <- as.data.frame(matrix(NA, nrow = nrow(buf), ncol = 1))
colnames(management)[1] <- "Codi"
management$Codi <- buf@data$Codi


layers <- list(sg15, sg16, sg17, sg18, sg19)
layers_names <- c("sg15", "sg16", "sg17", "sg18", "sg19")

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

#### SAVE ####
setwd("C:/Users/Ana/Documents/PhD/Third chapter/Data")
write.csv(management, "SG_15_19.csv")

# ---- GREENING ----
#### 2015 - 2019 ####

management <- as.data.frame(matrix(NA, nrow = nrow(buf), ncol = 1))
colnames(management)[1] <- "Codi"
management$Codi <- buf@data$Codi

layers <- list(green15, green16, green17, green18, green19)
layers_names <- c("green15", "green16", "green17", "green18", "green19")

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

#### SAVE ####
setwd("C:/Users/Ana/Documents/PhD/Third chapter/Data")
write.csv(management, "GREEN_15_19.csv")



