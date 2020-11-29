#-----------------------------------------------------------------------------------------
#             CALCULAR SLOPE PARA OBS GANGAS & RANDOM POINTS
#                               ROCIO TARJUELO
#-----------------------------------------------------------------------------------------

rm(list = ls())
options(digits = 20) 

# LIBRARIES-------------------------------------------------------------------------------
library(rgeos)
library(rgdal)
library(raster)

# FUNCTIONS-------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
#         START ANALYSIS
#-----------------------------------------------------------------------------------------

# Load data of observations---------------------------------------------------------------
datos.avai <- read.table("D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/2_random_points.txt", header = T, dec = ",", sep = "\t")

datos.bird <- read.table("D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/D_XYgps_positions_no_regadio_ETRS89_tot.txt", header = T, dec = ",",
                         sep = "\t")


# Crear objeto espacial para los puntos --------------------------------------------------
IDavai <- data.frame(Id = datos.avai$id)
xyIDavai <- datos.avai[, c("X_25831", "Y_25831")]
coordinates(IDavai) <- xyIDavai

IDbird <- data.frame(Id = datos.bird$Field1)
xyIDbird <- datos.bird[, c("X_25831", "Y_25831")]
coordinates(IDbird) <- xyIDbird


# DEM Mapa -------------------------------------------------------------------------------
DEM <- raster("D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/clip_dem_25831.tif")
plot(DEM)
CRS.used <- DEM@crs


# Calcular pendiente a partir del DEM ----------------------------------------------------
IDavai@proj4string <- CRS.used
IDbird@proj4string <- CRS.used

Terrain <- terrain(DEM, opt = "slope", unit = "degrees", neighbors = 8)
# image(DEM)
# points(IDavai, col = "black")
# image(Terrain)


# Extraer valores para los puntos aleatorios ---------------------------------------------
terrain.avai <- extract(x = Terrain, y = IDavai, df = TRUE) 
# df = T, results are returned as a dataframe with a sequential ID.
slope.avai <- cbind.data.frame(id = IDavai$Id, Slope = terrain.avai$slope)

# Extraer valores para las observaciones -------------------------------------------------
terrain.bird <- extract(x = Terrain, y = IDbird, df = TRUE) 
slope.bird <- cbind.data.frame(Field1 = IDbird$Id, Slope = terrain.bird$slope)


#-----------------------------------------------------------------------------------------
#         OUTPUT
#-----------------------------------------------------------------------------------------

write.table(x = slope.avai, file = "D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/2_slope_availability.txt", sep = "\t", dec = ",",
            col.names = T, row.names = F)

write.table(x = slope.bird, file = "D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/2_slope_use.txt", sep = "\t", dec = ",",
            col.names = T, row.names = F)

