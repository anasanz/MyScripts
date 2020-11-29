#-----------------------------------------------------------------------------------------
#        CALCULAR DISTANCIA A CAMINOS Y CARRETERAS PARA OBS GANGAS & RANDOM POINTS
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
carreteras <- raster("D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/clip_asp_25831.tif")
plot(carreteras)
caminos <- raster("D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/clip_camino_25831.tif")
plot(caminos)

CRS.used <- carreteras@crs
IDavai@proj4string <- CRS.used
IDbird@proj4string <- CRS.used

# Extraer valores para los puntos aleatorios----------------------------------------------
carreteras.avai <- extract(x = carreteras, y = IDavai, df = TRUE) 
# df = T, results are returned as a dataframe with a sequential ID.
caminos.avai <- extract(x = caminos, y = IDavai, df = TRUE) 

carrcam.avai <- cbind.data.frame(id = IDavai$Id,
                                 carreteras = carreteras.avai$clip_asp_25831,
                                 caminos = caminos.avai$clip_camino_25831)


# Extraer valores para las observaciones -------------------------------------------------
carreteras.bird <- extract(x = carreteras, y = IDbird, df = TRUE) 
caminos.bird <- extract(x = caminos, y = IDbird, df = TRUE) 

carrcam.bird <- cbind.data.frame(Field1 = IDbird$Id,
                                 carreteras = carreteras.bird$clip_asp_25831,
                                 caminos = caminos.bird$clip_camino_25831)


#-----------------------------------------------------------------------------------------
#         OUTPUT
#-----------------------------------------------------------------------------------------

write.table(x = carrcam.avai, file = "D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/2_carrcam_availability.txt", sep = "\t", 
            dec = ",", col.names = T, row.names = F)

write.table(x = carrcam.bird, file = "D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/2_carrcam_use.txt", sep = "\t", dec = ",",
            col.names = T, row.names = F)

