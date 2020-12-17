
## -------------------------------------------------
##        Extract habitat variables available 
##                  + Join with used
## ------------------------------------------------- 

rm(list=ls())

library(rgdal)

## ----  Load ----

# Maps

DUN2017 <- readOGR(dsn = "D:/PhD/Fourth chapter/Data/Full_analysis_Rocio", layer = "clip_dun17_usos_25831")  
DUN2018 <- readOGR(dsn = "D:/PhD/Fourth chapter/Data/Full_analysis_Rocio", layer = "clip_dun18_usos_25831")
DUN2019 <- readOGR(dsn = "D:/PhD/Fourth chapter/Data/Full_analysis_Rocio", layer = "clip_dun19_usos_25831")
vegnat <- readOGR(dsn = "D:/PhD/Fourth chapter/Data/Full_analysis_Rocio", layer = "clip_sigpac_vegnat_25831")

# Extract the CRS of CORINE (ETRS89_UTM_zone31N) for locations
CRS.used <- DUN2017@proj4string  

# Random locations
temp <- setwd("D:/PhD/Fourth chapter/Results/RSF_Ana/Analisis 1/random_loc")
temp <- list.files(pattern = "*.csv")
data <- lapply(temp, read.csv, sep = ",")

# Initialize loop to construct data matrix with different set of random locations created (inside folder random_loc)

for(xxx in 1:length(temp)){ 

ran <- data[[xxx]]
coordinates(ran) <- ran[, c("x", "y")]
ran@proj4string <- CRS.used
ran@data$X <- seq(1,nrow(ran@data)) # Having a reference of the points for later
ran$usoDUN <- NA

## ---- Extract habitat ----

# 1. DUN:  Join the three years in one column

years <- c(2017,2018,2019)
dun <- c(DUN2017, DUN2018, DUN2019)

for(t in 1:length(years)){
  hab <- over(ran[which(ran$year %in% years[t]), ], dun[[t]])
  ran@data[which(ran@data$year %in% years[t]),7] <- hab$uso
}

# 2. Vegnat

hab <- over(ran, vegnat)

# If it says FS in uso sigpac and NA in usoDUN, set as "ALMENDRO" (Like Rocío)

ran@data <- cbind(ran@data, hab$us) 
colnames(ran@data)[colnames(ran@data) == "hab$us"] <- "vegnat_uso"

ran@data[which(is.na(ran@data$usoDUN) & ran@data$vegnat_uso == "FS"), ] # Estos

for(i in 1:nrow(ran@data)){
  ifelse(is.na(ran@data$usoDUN[i]) & ran@data$vegnat_uso[i] == "FS",  ran@data$usoDUN[i] <- "ALMENDRO", ran@data$usoDUN[i] <- ran@data$usoDUN[i])
}

ran@data <- ran@data[ ,-which(colnames(ran@data) %in% "vegnat_uso")] # Delete column

# Join habitat data in one column and prioritize DUN (Join in the DUN column)

ran@data <- cbind(ran@data, hab$pastos)
colnames(ran@data)[colnames(ran@data) == "hab$pastos"] <- "vegnat_pastos"

ran@data$usoDUN[which(is.na(ran@data$usoDUN))] <- ran@data$vegnat_pastos[which(is.na(ran@data$usoDUN))]
ran@data <- ran@data[ ,-which(colnames(ran@data) %in% "vegnat_pastos")] # Delete column

# 3. Check NA
ran@data[which(is.na(ran@data$usoDUN)), ]

# 4. Habitat as dummy
ran@data$usoDUN <- as.factor(ran@data$usoDUN)
dummy <- as.data.frame(model.matrix(~ ran@data$usoDUN-1, data = ran@data))
colnames(dummy) <- levels(ran@data$usoDUN)

ran@data <- cbind(ran@data, dummy)


## ---- Extract distance to human features ----

# Load

carreteras <- raster("D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/clip_asp_25831.tif")
caminos <- raster("D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/clip_camino_25831.tif")

road_asp <- projectRaster(carreteras, crs = "+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs") 
road_grav <- projectRaster(caminos, crs = "+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs") 

stack_human <- stack(road_asp, road_grav)

# Extract (this is a faster way than the extract function)
coord <- ran[ ,c("x","y")] 
cells <- cellFromXY(stack_human, coord) # 1. Tells the number of the cells where the coord. fall
cov_human <- stack_human[cells] # 2. Gets the raster values of those cells

ran@data <- cbind(ran@data, cov_human)

## ---- Extract slope ----

dem <- raster("D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/clip_dem_25831.tif")
dem <- projectRaster(dem, crs = "+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs") 
slope <- terrain(dem, opt = "slope", unit = "degrees", neighbors = 8)

cells <- cellFromXY(slope, coord) # 1. Tells the number of the cells where the coord. fall
slope_extract <- slope[cells] # 2. Gets the raster values of those cells

ran@data <- cbind(ran@data, slope_extract)




## ---- Join ----

# Used locations

setwd("D:/PhD/Fourth chapter/Results/RSF_Ana/Analisis 1")
used <- read.csv("used_loc.csv")

# Set columns from available locations (ran) with the same name and order than used

ran@data <- ran@data[ ,-which(colnames(ran@data) %in% "usoDUN")] # Remove column 
ran@data$month <- NA # Add columns from used
ran@data$day <- NA 
ran@data <- ran@data[ ,c(1:3, 19:20, 4:9, 11:15, 10, 18, 16:17)] #Re-order (as in used)

colnames(ran@data) <- colnames(used) # Same column names than used (and than Rocio)

used$STATUS <- 1
ran@data$STATUS <- 0

data_def <- rbind(used, ran@data)
data_def$ID_Year <- paste(data_def$Logger_ID, data_def$year, sep = "_") 

# Save

temp[xxx] <- substr(temp[xxx],1,nchar(temp[xxx])-4) # Name for saving
setwd("D:/PhD/Fourth chapter/Results/RSF_Ana/Analisis 1/data_matrix")
write.csv(data_def, paste("dm_", temp[xxx],".csv", sep = ""))
}
