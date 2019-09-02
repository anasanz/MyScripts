

rm(list = ls())

library(raster)
library(rgdal)
library(rgeos)

#### Load layers ####

# Human

camin <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/clip_camin_4326.tif")
carret <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/clip_asp_4326.tif")
nuc <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/clip_nuc_4326.tif")

# Terrain

slope <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/clip_slope_4326.tif")

# Usos

cereal_17 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/cereal_17.tif")
cereal_18 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/cereal_18.tif")

barbecho_17 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/barbecho_17.tif")
barbecho_18 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/barbecho_18.tif")

herb_secano_17 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/herb_secano_17.tif")
herb_secano_18 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/herb_secano_18.tif")

herb_regadio_17 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/herb_regadio_17.tif")
herb_regadio_18 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/herb_regadio_18.tif")

frut_regadio_17 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/frut_regadio_17.tif")
frut_regadio_18 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/frut_regadio_18.tif")

olivo_17 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/olivo_17.tif")
olivo_18 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/olivo_18.tif")

almendro_17 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/almendro_17.tif")
almendro_18 <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/almendro_18.tif")

pastos <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/pastos.tif")
forestal <- raster("S:/PhD/Fourth chapter/GIS/Capas_variables/EPSG_4326/clips/usos/forestal.tif")

