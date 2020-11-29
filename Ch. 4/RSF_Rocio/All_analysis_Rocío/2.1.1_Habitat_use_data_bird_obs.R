#-----------------------------------------------------------------------------------------
#                   CALCULAR USO DE H?BITAT (USOS DEL SUELO)
#                           ROCIO TARJUELO
#-----------------------------------------------------------------------------------------
options(digits = 20)

# LIBRARIES-------------------------------------------------------------------------------
library(rgeos)
library(rgdal)

# FUNCTIONS-------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
#         START ANALYSIS
#-----------------------------------------------------------------------------------------

# Load data-------------------------------------------------------------------------------
setwd("D:/PhD/Fourth chapter/Data/Full_analysis_Rocio")
datos <- read.table("D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/D_XYgps_positions_no_regadio_ETRS89_tot.txt", header = T, dec = ",", 
                    sep = "\t")

which(is.na(datos$X_25831))  # No NA data
which(is.na(datos$Y_25831))  # No NA data

str(datos[, c("X_25831", "Y_25831")])  
# Must be numeric or integer type

# Mapas uso ------------------------------------------------------------------------------
DUN2017 <- readOGR(dsn = "D:/PhD/Fourth chapter/Data/Full_analysis_Rocio", layer = "clip_dun17_usos_25831")  
DUN2018 <- readOGR(dsn = "D:/PhD/Fourth chapter/Data/Full_analysis_Rocio", layer = "clip_dun18_usos_25831")
DUN2019 <- readOGR(dsn = "D:/PhD/Fourth chapter/Data/Full_analysis_Rocio", layer = "clip_dun19_usos_25831")
vegnat <- readOGR(dsn = "D:/PhD/Fourth chapter/Data/Full_analysis_Rocio", layer = "clip_sigpac_vegnat_25831")


# Extraer el CRS de la capa DUN (ETRS89_UTM_zone31N)
CRS.used <- DUN2017@proj4string  

# Extraer uso del habitat ----------------------------------------------------------------
# 2017. Incluyendo los datos de 2016, que solo tiene mes de Diciembre
datos2017 <- subset(datos, Year == "2017" | Year == "2016") 
ID2017 <- data.frame(Id = datos2017$Logger_ID)
xyID2017 <- datos2017[, c("X_25831", "Y_25831")]
coordinates(ID2017) <- xyID2017
ID2017@proj4string <- CRS.used

habitatDUN2017 <- over(ID2017, DUN2017)
habitatvegnat2017 <- over(ID2017, vegnat)

which(rownames(habitatDUN2017) != rownames(datos2017))  
# Mismo orden de puntos en ambos objetos
habitat2017 <- cbind(datos2017, USODUN = habitatDUN2017$uso, 
                     VEGNAT = habitatvegnat2017$pastos, SIGPAC = habitatvegnat2017$us)

# 2018
datos2018 <- subset(datos, Year == "2018") 
ID2018 <- data.frame(Id = datos2018$Logger_ID)
xyID2018 <- datos2018[, c("X_25831", "Y_25831")]
coordinates(ID2018) <- xyID2018
ID2018@proj4string <- CRS.used

habitatDUN2018 <- over(ID2018, DUN2018)
habitatvegnat2018 <- over(ID2018, vegnat)

which(rownames(habitatDUN2018) != rownames(datos2018))  
# Mismo orden de puntos en ambos objetos
habitat2018 <- cbind(datos2018, USODUN = habitatDUN2018$uso, 
                     VEGNAT = habitatvegnat2018$pastos, SIGPAC = habitatvegnat2018$us)

# 2019
datos2019 <- subset(datos, Year == "2019") 
ID2019 <- data.frame(Id = datos2019$Logger_ID)
xyID2019 <- datos2019[, c("X_25831", "Y_25831")]
coordinates(ID2019) <- xyID2019
ID2019@proj4string <- CRS.used

habitatDUN2019 <- over(ID2019, DUN2019)
habitatvegnat2019 <- over(ID2019, vegnat)

which(rownames(habitatDUN2019) != rownames(datos2019))  
# Mismo orden de puntos en ambos objetos
habitat2019 <- cbind(datos2019, USODUN = habitatDUN2019$uso, 
                     VEGNAT = habitatvegnat2019$pastos, SIGPAC = habitatvegnat2019$us)


habitat.use.data <- rbind(habitat2017, habitat2018, habitat2019)
head(habitat.use.data)

# Reemplazar casos sin uso de habitat de la DUN ni de vegetacion natural del SIGPAC
# pero que en SIGPAC constan como FS. Seguramente son frutales que no han sido declarados. 
casos.replace <- habitat.use.data[is.na(habitat.use.data$USODUN) & 
                                    is.na(habitat.use.data$VEGNAT) & 
                                    habitat.use.data$SIGPAC == "FS", "Field1"]

# A estos casos les asigno valor de "Almendro" en el campo de USODUN
habitat.use.data[habitat.use.data$Field1%in%casos.replace, "USODUN"] <- "ALMENDRO"

# Para facilitar el trabajo posterior, voy a reunir toda la info de habitat en una sola 
# columna (en USODEF)
habitat.use.data$USODEF <- habitat.use.data$USODUN

# En las observaciones clasificadas como VEGNAT segun SIGPAC hay uso asignado tambien en 
# DUN (es decir, solapamiento entre las capas??). Dejo la info de DUN
vegnat <- subset(habitat.use.data, !is.na(VEGNAT))
table(vegnat$USODEF)
# Hay 20 casos donde esos puntos que caen en VEGNAT, tambien tienen asignado barbecho

# Identificar los casos sin dato en DUN y con info en VEGNAT
levels(habitat.use.data$USODEF) <- c(levels(habitat.use.data$USODEF), "PASTOS",
                                      "FORESTAL")
casos.replace2 <- habitat.use.data[is.na(habitat.use.data$USODEF) & 
                                    !is.na(habitat.use.data$VEGNAT), "Field1"]

hab.replace2 <- habitat.use.data[is.na(habitat.use.data$USODEF) & 
                                     !is.na(habitat.use.data$VEGNAT), "VEGNAT"]

# En la nueva variable USODED indicamos si estos casos corresponden a "PASTOS" o
# "FOREST", las dos categorias de uso DEF
habitat.use.data[habitat.use.data$Field1%in%casos.replace2, "USODEF"] <- hab.replace2
vegnat2 <- subset(habitat.use.data, !is.na(VEGNAT))
table(vegnat2$USODEF)

#-----------------------------------------------------------------------------------------
#         OUTPUT
#-----------------------------------------------------------------------------------------
setwd("D:/PhD/Fourth chapter/Data/Full_analysis_Rocio")
write.table(x = habitat.use.data, file = "D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/2_habitat_use.txt", sep = "\t", dec = ",",
            col.names = T, row.names = F)

