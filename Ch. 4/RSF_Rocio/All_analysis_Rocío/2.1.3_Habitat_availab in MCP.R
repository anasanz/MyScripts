#-----------------------------------------------------------------------------------------
#               CALCULAR DISPONIBILIDAD DE H?BITAT (USOS DEL SUELO)
#                           ROCIO TARJUELO
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

# Load data-------------------------------------------------------------------------------
datos <- read.table("D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/2_random_points.txt", header = T, dec = ",", sep = "\t")

which(is.na(datos$X_25831))  # No NA data
which(is.na(datos$Y_25831))  # No NA data


str(datos)
# coordinates must be numeric or integer type

# Mapas uso ------------------------------------------------------------------------------
DUN2017 <- readOGR(dsn = "D:/PhD/Fourth chapter/Data/Full_analysis_Rocio", layer = "clip_dun17_usos_25831")  
DUN2018 <- readOGR(dsn = "D:/PhD/Fourth chapter/Data/Full_analysis_Rocio", layer = "clip_dun18_usos_25831")
DUN2019 <- readOGR(dsn = "D:/PhD/Fourth chapter/Data/Full_analysis_Rocio", layer = "clip_dun19_usos_25831")
vegnat <- readOGR(dsn = "D:/PhD/Fourth chapter/Data/Full_analysis_Rocio", layer = "clip_sigpac_vegnat_25831")



# Extract the CRS of CORINE (ETRS89_UTM_zone31N) for locations
CRS.used <- DUN2017@proj4string  

# Maps
study.site <- readOGR(dsn = "D:/PhD/Fourth chapter/Data/Full_analysis_Rocio", layer = "MCP_tot")


MPC.vegnat <- intersect(vegnat, study.site)

MPC.vegnat@data$AREA2 <- gArea(MPC.vegnat, byid = T)
area.tot.MCP <- gArea(MPC.vegnat, byid = F)/1000000

# Calcular la proporcion de cada tipo de habitat
MPC.vegnat.prop <- with(MPC.vegnat@data, tapply(AREA2, pastos, sum))/1000000
MPC.vegnat.prop/area.tot.MCP


## DUN2017
MPC.DUN17 <- intersect(DUN2017, study.site)
MPC.DUN17@data$AREA2 <- gArea(MPC.DUN17, byid = T)
# Calcular la proporcion de cada tipo de h?bitat
MPC.DUN17.prop <- with(MPC.DUN17@data, tapply(AREA2, uso, sum))/1000000
MPC.DUN17.prop/area.tot.MCP


## DUN2018
MPC.DUN18 <- intersect(DUN2018, study.site)
MPC.DUN18@data$AREA2 <- gArea(MPC.DUN18, byid = T)
# Calcular la proporcion de cada tipo de h?bitat
MPC.DUN18.prop <- with(MPC.DUN18@data, tapply(AREA2, uso, sum))/1000000
MPC.DUN18.prop/area.tot.MCP

## DUN2019
MPC.DUN19 <- intersect(DUN2019, study.site)
MPC.DUN19@data$AREA2 <- gArea(MPC.DUN19, byid = T)
# Calcular la proporcion de cada tipo de h?bitat
MPC.DUN19.prop <- with(MPC.DUN19@data, tapply(AREA2, uso, sum))/1000000
MPC.DUN19.prop/area.tot.MCP


# Proporcion de obs en cada tipo de habitat
datos.use <- read.table("D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/2_datos_use_analysis.txt", header = T, dec = ",")
# Eliminar observaciones sin info de habitat
case.remove1 <- which(is.na(apply(datos.use[,9:17], 1, sum)))
datos.use.ok <- datos.use[-case.remove1, ]
# reemplazar año 2016 por 2017 para facilitar el analisis
datos.use.ok[datos.use.ok$year == 2016, "year"] <- 2017

head(datos.use.ok)
alm <- sum(datos.use.ok$almendro)/dim(datos.use.ok)[1]
barb <- sum(datos.use.ok$barbecho)/dim(datos.use.ok)[1]
cereal <- sum(datos.use.ok$cereal)/dim(datos.use.ok)[1]
frutreg <- sum(datos.use.ok$frutreg)/dim(datos.use.ok)[1]
herreg <- sum(datos.use.ok$herreg)/dim(datos.use.ok)[1]
olivo <- sum(datos.use.ok$olivo)/dim(datos.use.ok)[1]
hersec <- sum(datos.use.ok$otherhersec)/dim(datos.use.ok)[1]
vegnat <- sum(datos.use.ok$vegnat)/dim(datos.use.ok)[1]
forestal <- sum(datos.use.ok$forestal)/dim(datos.use.ok)[1]
