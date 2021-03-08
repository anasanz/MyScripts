
## -------------------------------------------------
##        Extract habitat variables available 
##                  + Join with used
## ------------------------------------------------- 

rm(list=ls())

library(rgdal)
library(dplyr)

## ----  Load ----

# Maps

DUN2017 <- readOGR(dsn = "D:/PhD/Fourth chapter/Data/Full_analysis_Rocio", layer = "clip_dun17_usos_25831")  
CRS.used <- DUN2017@proj4string # Only to have the reference crs
vegnat <- readOGR(dsn = "D:/PhD/Fourth chapter/Data/Full_analysis_Rocio", layer = "clip_sigpac_vegnat_25831")

# Managed/Not managed
bar17 <- readOGR(dsn = "D:/PhD/Fourth chapter/Data/GIS/Capas_Rocío/barbechos y rebaños", layer = "barbechos17_4326")
bar18 <- readOGR(dsn = "D:/PhD/Fourth chapter/Data/GIS/Capas_Rocío/barbechos y rebaños", layer = "barbechos18_4326")
bar19 <- readOGR(dsn = "D:/PhD/Fourth chapter/Data/GIS/Capas_Rocío/barbechos y rebaños", layer = "barbechos19_4326")

bar17 <- spTransform(bar17, CRSobj = "+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs"  )
bar18 <- spTransform(bar18, CRSobj = "+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs"  )
bar19 <- spTransform(bar19, CRSobj = "+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs"  )


# Random locations
temp <- setwd("D:/PhD/Fourth chapter/Results/RSF_Ana/Analisis 2/random_loc")
temp <- list.files(pattern = "*.csv")
data <- lapply(temp, read.csv, sep = ",")

# Initialize loop to construct data matrix with different set of random locations created (inside folder random_loc)

for(xxx in 1:length(temp)){ 

ran <- data[[xxx]]
coordinates(ran) <- ran[, c("x", "y")]
ran@proj4string <- CRS.used
ran@data$X <- seq(1,nrow(ran@data)) # Having a reference of the points for later


## ---- Extract vegnat ----

hab <- over(ran, vegnat)

# Vegnat as dummy
hab$pastos[which(hab$pastos == "PASTOS")] <- 1
hab$pastos[which(is.na(hab$pastos))] <- 0

ran@data <- cbind(ran@data, hab$pastos)
colnames(ran@data)[colnames(ran@data) == "hab$pastos"] <- "vegnat"


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

head(ran@data)


## ---- Join before extracting fallow (because I need to do it from both) ----

# Used locations

setwd("D:/PhD/Fourth chapter/Results/RSF_Ana/Analisis 2")
used <- read.csv("used_loc.csv")
head(used)
used <- used[ ,-c(9:15, 17)] #• Remove all uses that are not useful in this analysis


# Set columns from available locations (ran) with the same name and order than used

ran@data$month <- NA # Add columns from used
ran@data$day <- NA 
ran@data <- ran@data[ ,c(1:3, 11:12, 4:7, 10, 8:9)] #Re-order (as in used)

colnames(ran@data) <- colnames(used) # Same column names than used (and than Rocio)

used$STATUS <- 1
ran@data$STATUS <- 0

data_def <- rbind(used, ran@data)
data_def$ID_Year <- paste(data_def$Logger_ID, data_def$year, sep = "_") 

# Make it spacial object to extract fallow

coordinates(data_def) <- data_def[, c("x", "y")]
data_def@proj4string <- CRS.used
data_def$barbecho <- NA

## ---- Extract fallow from all data ----

years <- c(2017,2018,2019)
bar <- c(bar17, bar18, bar19)

for(t in 1:length(years)){
  hab <- over(data_def[which(data_def$year %in% years[t]), ], bar[[t]])
  data_def@data$barbecho[which(data_def@data$year %in% years[t])] <- hab$manag
}

# Fallow as dummy

data_def@data$barbecho[which(is.na(data_def@data$barbecho))] <- "kk" # Porque no puede haber NA para crear la dummy variable
data_def@data$barbecho <- as.factor(data_def@data$barbecho)
dummy <- as.data.frame(model.matrix(~ data_def@data$barbecho -1, data = data_def@data))
colnames(dummy) <- levels(data_def@data$barbecho)

data_def@data <- cbind(data_def@data, dummy)

data_def@data <- data_def@data[ ,-c(15,16)] # Delete dummy columns
colnames(data_def@data)[colnames(data_def@data) == "NO"] <- "no_manag"
colnames(data_def@data)[colnames(data_def@data) == "SI"] <- "si_manag"

# Check that there are no observations out of vegnat, no_manag and si_manad
nrow(data_def@data[which(data_def@data$vegnat == 0 & data_def@data$no_manag == 0 & data_def@data$si_manag == 0), ]) # Only 45 out of 31648 obs, errores de digitalización seguramente,  delete

data_def <- data_def[-which(data_def@data$vegnat == 0 & data_def@data$no_manag == 0 & data_def@data$si_manag == 0), ]

## ---- Extract distance to ALL human features  ----
# I need to do this also after joining because it was not done for used locations.
# I will probably use this as unique distance to human variable in the second analysis
# I have calculated the layer "linear", but because the numbers extracted are slightly different
# I prefer to take the minimun value between "carreteras" y "caminos", which is the same (for consistency with the previous analysis)

data_def@data <- mutate(data_def@data, linear = pmin(carreteras, caminos))

# Save

temp[xxx] <- substr(temp[xxx],1,nchar(temp[xxx])-4) # Name for saving
setwd("D:/PhD/Fourth chapter/Results/RSF_Ana/Analisis 2/data_matrix")
write.csv(data_def, paste("dm_", temp[xxx],".csv", sep = ""))
}
