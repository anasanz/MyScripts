#-----------------------------------------------------------------------------------------
#               CALCULAR DISPONIBILIDAD DE H?BITAT (USOS DEL SUELO)
#                           ROCIO TARJUELO
#-----------------------------------------------------------------------------------------

rm(list = ls())
options(digits = 20)

# LIBRARIES-------------------------------------------------------------------------------
library(rgeos)
library(rgdal)

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

# Extraer uso del habitat ----------------------------------------------------------------
ID <- data.frame(Id = datos$id)
xyID <- datos[, c("X_25831", "Y_25831")]
coordinates(ID) <- xyID
ID@proj4string <- CRS.used

habitatDUN2017 <- over(ID, DUN2017)
habitatDUN2018 <- over(ID, DUN2018)
habitatDUN2019 <- over(ID, DUN2019)
habitatvegnat <- over(ID, vegnat)

which(rownames(habitatDUN2017) != rownames(datos))
which(rownames(habitatDUN2018) != rownames(datos))  
which(rownames(habitatDUN2019) != rownames(datos))  
# Mismo orden de puntos en ambos objetos

habitat.random.pts <- cbind(datos, 
                            USODUN17 = habitatDUN2017$uso, 
                            USODUN18 = habitatDUN2018$uso,
                            USODUN19 = habitatDUN2019$uso,
                            VEGNAT = habitatvegnat$pastos,
                            SIGPAC = habitatvegnat$us)
head(habitat.random.pts)
str(habitat.random.pts)

# Reclasificar los casos que caen en FS (o eliminar estos puntos que no tienen nada en 
# ninguna de las capas)
casos.replace17 <- habitat.random.pts[is.na(habitat.random.pts$USODUN17) & 
                                    is.na(habitat.random.pts$VEGNAT) & 
                                      habitat.random.pts$SIGPAC == "FS", "id"]

habitat.random.pts[habitat.random.pts$id%in%casos.replace17, "USODUN17"] <- "ALMENDRO"

casos.replace18 <- habitat.random.pts[is.na(habitat.random.pts$USODUN18) & 
                                        is.na(habitat.random.pts$VEGNAT) & 
                                        habitat.random.pts$SIGPAC == "FS", "id"]

habitat.random.pts[habitat.random.pts$id%in%casos.replace18, "USODUN18"] <- "ALMENDRO"

casos.replace19 <- habitat.random.pts[is.na(habitat.random.pts$USODUN19) & 
                                        is.na(habitat.random.pts$VEGNAT) & 
                                        habitat.random.pts$SIGPAC == "FS", "id"]

habitat.random.pts[habitat.random.pts$id%in%casos.replace19, "USODUN19"] <- "ALMENDRO"


# Para facilitar el trabajo posterior, voy a reunir toda la info de habitat en una sola 
# columna (en USODEF para cada año)
habitat.random.pts$USODEF17 <- habitat.random.pts$USODUN17
habitat.random.pts$USODEF18 <- habitat.random.pts$USODUN18
habitat.random.pts$USODEF19 <- habitat.random.pts$USODUN19

# En las observaciones clasificadas como VEGNAT segun SIGPAC hay uso asignado tambien en 
# DUN (es decir, solapamiento entre las capas??)
vegnat.data <- subset(habitat.random.pts, !is.na(VEGNAT))
table(vegnat.data$USODEF17)
table(vegnat.data$USODEF18)
table(vegnat.data$USODEF19)

# Hay casos donde esos puntos que caen en VEGNAT, tambien tienen asignado otros usos DUN
# Desajuste de la capa SIGPAC, mantengo el valor de la DUN. Tengo que hacerlo para cada
# año
# 2017
levels(habitat.random.pts$USODEF17) <- c(levels(habitat.random.pts$USODEF17), "PASTOS",
                                     "FORESTAL")
casos.replace2017.2 <- habitat.random.pts[is.na(habitat.random.pts$USODEF17) & 
                                     !is.na(habitat.random.pts$VEGNAT), "id"]

hab.replace2017.2 <- habitat.random.pts[is.na(habitat.random.pts$USODEF17) & 
                                   !is.na(habitat.random.pts$VEGNAT), "VEGNAT"]

habitat.random.pts[habitat.random.pts$id%in%casos.replace2017.2,
                   "USODEF17"] <- hab.replace2017.2 

vegnat2017 <- subset(habitat.random.pts, !is.na(VEGNAT))
table(vegnat2017$USODEF17)
table(vegnat2017$VEGNAT)

# 2018
levels(habitat.random.pts$USODEF18) <- c(levels(habitat.random.pts$USODEF18), "PASTOS",
                                         "FORESTAL")
casos.replace2018.2 <- habitat.random.pts[is.na(habitat.random.pts$USODEF18) & 
                                            !is.na(habitat.random.pts$VEGNAT), "id"]

hab.replace2018.2 <- habitat.random.pts[is.na(habitat.random.pts$USODEF18) & 
                                          !is.na(habitat.random.pts$VEGNAT), "VEGNAT"]

habitat.random.pts[habitat.random.pts$id%in%casos.replace2018.2,
                   "USODEF18"] <- hab.replace2018.2 
vegnat2018 <- subset(habitat.random.pts, !is.na(VEGNAT))
table(vegnat2018$USODEF18)
table(vegnat2018$VEGNAT)

# 2019
levels(habitat.random.pts$USODEF19) <- c(levels(habitat.random.pts$USODEF19), "PASTOS",
                                         "FORESTAL")
casos.replace2019.2 <- habitat.random.pts[is.na(habitat.random.pts$USODEF19) & 
                                            !is.na(habitat.random.pts$VEGNAT), "id"]

hab.replace2019.2 <- habitat.random.pts[is.na(habitat.random.pts$USODEF19) & 
                                          !is.na(habitat.random.pts$VEGNAT), "VEGNAT"]

habitat.random.pts[habitat.random.pts$id%in%casos.replace2019.2,
                   "USODEF19"] <- hab.replace2019.2 
vegnat2019 <- subset(habitat.random.pts, !is.na(VEGNAT))
table(vegnat2019$USODEF19)
table(vegnat2019$VEGNAT)

str(habitat.random.pts)

#-----------------------------------------------------------------------------------------
#         OUTPUT
#-----------------------------------------------------------------------------------------

write.table(x = habitat.random.pts, file = "D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/2_habitat_avail.txt", sep = "\t",
            dec = ",", col.names = T, row.names = F)

