
# Human and terrain variables

rm(list = ls())

library(raster)
library(rgdal)

#### Load variables ####

setwd("S:/PhD/Fourth chapter/GIS/Capas_variables")

# Human
camin <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/camin_4326")
autop <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/autop_4326")
autov <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/autov_4326")
car2 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/car2_4326")
nuc <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/nuc_4326")


# Terrain
dem <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/dem_4326")
domin <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/curv_4326")

var <- c(camin, autop, autov, car2, dem, domin, nuc)


#### Clip ####

study_area <- readOGR("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326", "studyarea_pteroclids_EPSG_4326")

setwd("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips")

for (i in 1:length(var)){
  clip <- crop(var[[i]], extent(study_area), snap="out")
  writeRaster(clip, filename = paste("clip_", var[[i]]@data@names, sep = ""),format='GTiff')
  
}

#### Load clip ####

# Human
camin <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/clip_camin_4326.tif")
autop <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/clip_autop_4326.tif")
autov <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/clip_autov_4326.tif")
car2 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/clip_car2_4326.tif")
nuc <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/clip_nuc_4326.tif")

# Terrain
dem <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/clip_dem_4326.tif")
domin <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/clip_curv_4326.tif")

### Calculate Slope ####

slope <- terrain(dem,'slope', unit = 'degrees', neighbors = 8, filename = 'slope')

setwd("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips")
writeRaster(slope, filename = 'clip_slope_4326', format = 'GTiff')

### Calculate distance to the closest primary (asphalted) road ####
autop <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/clip_autop_4326.tif")
autov <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/clip_autov_4326.tif")
car2 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/clip_car2_4326.tif")

asph <- brick(autop, autov, car2)
dist_asphalted <- calc(asph, function(x){min(x)})
writeRaster(dist_asphalted, filename = 'clip_asp_4326', format = 'GTiff')
