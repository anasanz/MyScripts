#-----------------------------------------------------------------------------------------
#                     CREAR LA MATRIZ FINAL PARA LA RSPF
#                               ROCIO TARJUELO
#-----------------------------------------------------------------------------------------

# LIBRARIES-------------------------------------------------------------------------------


# FUNCTIONS-------------------------------------------------------------------------------

rm(list = ls())

#-----------------------------------------------------------------------------------------
#         START ANALYSIS
#-----------------------------------------------------------------------------------------

# Load data-------------------------------------------------------------------------------
habuse <- read.table("D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/2_habitat_use.txt", header = T, dec = ",", sep = "\t")
habavai <- read.table("D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/2_habitat_avail.txt", header = T, dec = ",", sep = "\t")
slopeuse <- read.table("D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/2_slope_use.txt", header = T, dec = ",", sep = "\t")
slopeavai <- read.table("D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/2_slope_availability.txt", header = T, dec = ",",
                        sep = "\t")
carreterasuse <- read.table("D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/2_carrcam_use.txt", header = T, dec = ",", sep = "\t")
carreterasavai <- read.table("D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/2_carrcam_availability.txt", header = T, dec = ",", 
                           sep = "\t")

levels(habuse$USODEF)

# Matriz de observaciones ----------------------------------------------------------------
# Reclasificar las variables categoricas en variables dummy para la RSPF -----------------
almendrouse <- ifelse(habuse$USODEF == "ALMENDRO", 1, 0)

barbechouse <- ifelse(habuse$USODEF == "BARBECHO", 1, 0)

cerealuse <- ifelse(habuse$USODEF == "CEREAL", 1, 0)

frutreguse <- ifelse(habuse$USODEF == "FRUTALES DE REGADIO", 1, 0)

herreguse <- ifelse(habuse$USODEF == "HERBACEOS DE REGADIO", 1, 0)

olivouse <- ifelse(habuse$USODEF == "OLIVO", 1, 0)

otroshersecuse <- ifelse(habuse$USODEF == "OTROS HERBACEOS DE SECANO", 1, 0)

vegnat <- ifelse(habuse$USODEF == "PASTOS", 1, 0)

forestal <- ifelse(habuse$USODEF == "FORESTAL", 1, 0)


datoshabuse <- cbind.data.frame(habuse$Field1, habuse$Logger_ID, habuse$Year, 
                                habuse$Month, habuse$Day, 
                                #habuse$periodo2, 
                                habuse$X_25831, habuse$Y_25831, 
                                almendrouse, barbechouse, cerealuse, frutreguse, 
                                herreguse, olivouse, otroshersecuse, vegnat, forestal)
colnames(datoshabuse) <- c("Field1", "Logger_ID", "year", "month", "day", 
                           #"periodo",
                           "x", "y", "almendro","barbecho", "cereal", "frutreg", 
                           "herreg", "olivo", "otherhersec", "vegnat", "forestal")                              
head(datoshabuse)

# Check
length(which(is.na(apply(datoshabuse[9:16],1,sum))))  # = 510 # ASP = 616 (NA = CAMINOS...)
length(which(apply(datoshabuse[9:16],1,sum)==1))  # = 18965. # ASP = 21164
# La suma de ambos es el numero de filas de datos.use, es decir, no hay valores distintos
# de 1 o NA, lo que implica que solo tenemos 1 unico sustrato (o ninguno) para cada obs.
# OK
# ASP -> 21164 + 616 = 21780 -> No 22384 (HAB.USE??)

which((slopeuse$Field1 == carreterasuse$Field1) == FALSE)
# Mismo orden de id de observaci?n para la matriz de slopeuse y carreterasuse.
slopecarruse <- cbind(slopeuse, carreteras = carreterasuse$carreteras, 
                      caminos = carreterasuse$caminos)
head(slopecarruse)


which((datoshabuse$Field1 == slopecarruse$Field1) == FALSE)
# No hay el mismo orden entre ambas matrices. Hay que unir usando merge

datos.use <- merge(datoshabuse, slopecarruse, by = "Field1")

# Asignar columna de estado
datos.use$STATUS <- 1
str(datos.use)


# Matriz de random points ----------------------------------------------------------------
# Reclasificar las variables categ?ricas en variables dummy para la RSPF -----------------
# 2017 
levels(habavai$USODEF17)

almendroavai17 <- ifelse(habavai$USODEF17 == "ALMENDRO", 1, 0)

barbechoavai17 <- ifelse(habavai$USODEF17 == "BARBECHO", 1, 0)

cerealavai17 <- ifelse(habavai$USODEF17 == "CEREAL", 1, 0)

frutregavai17 <- ifelse(habavai$USODEF17 == "FRUTALES DE REGADIO", 1, 0)

herregavai17 <- ifelse(habavai$USODEF17 == "HERBACEOS DE REGADIO", 1, 0)

olivoavai17 <- ifelse(habavai$USODEF17 == "OLIVO", 1, 0)

otroshersecavai17 <- ifelse(habavai$USODEF17 == "OTROS HERBACEOS DE SECANO", 1, 0)

vegnatavai17 <- ifelse(habavai$USODEF17 == "PASTOS", 1, 0)

forestalavai17 <- ifelse(habavai$USODEF17 == "FORESTAL", 1, 0)


datoshabavai17 <- cbind.data.frame(habavai$id, Logger = NA, year = 2017, month = NA,
                                   day = NA, periodo = NA, 
                                   habavai$X_25831, habavai$Y_25831, habavai[, 4:15],
                                   almendroavai17, barbechoavai17, cerealavai17, 
                                   frutregavai17, herregavai17,olivoavai17, 
                                   otroshersecavai17, vegnatavai17, forestalavai17)
colnames(datoshabavai17) <- c("Field1", "Logger_ID", "year", "month", "day", "periodo",
                              "x", "y", colnames(habavai)[4:15], "almendro", "barbecho",
                              "cereal", "frutreg", "herreg", "olivo", "otherhersec", 
                              "vegnat", "forestal")                              
head(datoshabavai17)

# Check
length(which(is.na(apply(datoshabavai17[21:29],1,sum))))  # = 3041
length(which(apply(datoshabavai17[21:29],1,sum)==1))  # = 24898.
# La suma de ambos es el n?mero de filas de habavai, es decir, no hay valores distintos
# de 1 o NA, lo que implica que s?lo tenemos 1 ?nico sustratos (o ninguno) para cada obs.
# OK
# ASP = En este caso OK 2923+25016=27939

# 2018 
levels(habavai$USODEF18)

almendroavai18 <- ifelse(habavai$USODEF18 == "ALMENDRO", 1, 0)

barbechoavai18 <- ifelse(habavai$USODEF18 == "BARBECHO", 1, 0)

cerealavai18 <- ifelse(habavai$USODEF18 == "CEREAL", 1, 0)

frutregavai18 <- ifelse(habavai$USODEF18 == "FRUTALES DE REGADIO", 1, 0)

herregavai18 <- ifelse(habavai$USODEF18 == "HERBACEOS DE REGADIO", 1, 0)

olivoavai18 <- ifelse(habavai$USODEF18 == "OLIVO", 1, 0)

otroshersecavai18 <- ifelse(habavai$USODEF18 == "OTROS HERBACEOS DE SECANO", 1, 0)

vegnatavai18 <- ifelse(habavai$USODEF18 == "PASTOS", 1, 0)

forestalavai18 <- ifelse(habavai$USODEF18 == "FORESTAL", 1, 0)


datoshabavai18 <- cbind.data.frame(habavai$id, Logger = NA, year = 2018, periodo = NA,
                                   day = NA, periodo = NA, 
                                   habavai$X_25831, habavai$Y_25831, habavai[, 4:15],
                                   almendroavai18, barbechoavai18, cerealavai18, 
                                   frutregavai18, herregavai18, olivoavai18, 
                                   otroshersecavai18, vegnatavai18, forestalavai18)
colnames(datoshabavai18) <- c("Field1", "Logger_ID", "year", "month", "day", "periodo",
                              "x", "y", colnames(habavai)[4:15], "almendro", "barbecho",
                              "cereal", "frutreg", "herreg", "olivo", "otherhersec", 
                              "vegnat", "forestal")                                
head(datoshabavai18)

# Check
length(which(is.na(apply(datoshabavai18[21:28],1,sum))))  # = 3036
length(which(apply(datoshabavai18[21:29],1,sum)==1))  # = 24903.
# La suma de ambos es el n?mero de filas de habavai, es decir, no hay valores distintos
# de 1 o NA, lo que implica que s?lo tenemos 1 ?nico sustratos (o ninguno) para cada obs.
# OK


# 2019 
levels(habavai$USODEF19)

almendroavai19 <- ifelse(habavai$USODEF19 == "ALMENDRO", 1, 0)

barbechoavai19 <- ifelse(habavai$USODEF19 == "BARBECHO", 1, 0)

cerealavai19 <- ifelse(habavai$USODEF19 == "CEREAL", 1, 0)

frutregavai19 <- ifelse(habavai$USODEF19 == "FRUTALES DE REGADIO", 1, 0)

herregavai19 <- ifelse(habavai$USODEF19 == "HERBACEOS DE REGADIO", 1, 0)

olivoavai19 <- ifelse(habavai$USODEF19 == "OLIVO", 1, 0)

otroshersecavai19 <- ifelse(habavai$USODEF19 == "OTROS HERBACEOS DE SECANO", 1, 0)

vegnatavai19 <- ifelse(habavai$USODEF19 == "PASTOS", 1, 0)

forestalavai19 <- ifelse(habavai$USODEF19 == "FORESTAL", 1, 0)


datoshabavai19 <- cbind.data.frame(habavai$id, Logger = NA, year = 2019, periodo = NA, 
                                   month = NA, day = NA,
                                   habavai$X_25831, habavai$Y_25831, habavai[, 4:15],
                                   almendroavai19, barbechoavai19, cerealavai19, 
                                   frutregavai19, herregavai19, olivoavai19, 
                                   otroshersecavai19, vegnatavai19, forestalavai19)
colnames(datoshabavai19) <- c("Field1", "Logger_ID", "year", "month", "day", "periodo",
                              "x", "y", colnames(habavai)[4:15], "almendro", "barbecho",
                              "cereal", "frutreg", "herreg", "olivo", "otherhersec", 
                              "vegnat", "forestal")                                 
head(datoshabavai19)

# Check
length(which(is.na(apply(datoshabavai19[21:29],1,sum))))  # = 3391
length(which(apply(datoshabavai19[21:29],1,sum)==1))  # = 24548.
# La suma de ambos es el n?mero de filas de habavai, es decir, no hay valores distintos
# de 1 o NA, lo que implica que s?lo tenemos 1 ?nico sustratos (o ninguno) para cada obs.
# OK

datoshabavai <- rbind(datoshabavai17, datoshabavai18, datoshabavai19)


which((slopeavai$id == carreterasavai$id) == FALSE)
# Mismo orden de id de observaci?n para la matriz de slopeuse y carreterasuse.
slopecarravai <- cbind(slopeavai, carreteras = carreterasavai$carreteras, 
                      caminos = carreterasavai$caminos)
head(slopecarravai)


which((datoshabavai$id == slopecarravai$id) == FALSE)
# Todos est?n en el mismo orden. Aun as? voy a unir usando merge

datos.avai <- merge(datoshabavai, slopecarravai, by.x = "Field1", by.y = "id")

# Asignar columna de estado
datos.avai$STATUS <- 0
str(datos.avai)

# Correlacion entre variables continuas
cor.test(x = c(datos.use$Slope, datos.avai[datos.avai$year=="2017", "Slope"]), 
         y = c(datos.use$carreteras, datos.avai[datos.avai$year=="2017", "carreteras"]),
         method = "pearson")

cor.test(x = c(datos.use$caminos, datos.avai[datos.avai$year=="2017", "caminos"]), 
         y = c(datos.use$carreteras, datos.avai[datos.avai$year=="2017", "carreteras"]),
         method = "pearson")

cor.test(x = c(datos.use$Slope, datos.avai[datos.avai$year=="2017", "Slope"]), 
         y = c(datos.use$caminos, datos.avai[datos.avai$year=="2017", "caminos"]),
         method = "pearson")

# La correlaci?n es muy baja entre los tres pares de variables.

#-----------------------------------------------------------------------------------------
#         OUTPUT
#-----------------------------------------------------------------------------------------

write.table(datos.use, "D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/2_datos_use_analysis.txt", col.names = T, dec = ",", 
            sep = "\t")
write.table(datos.avai,"D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/2_datos_avai_analysis.txt", col.names = T, dec = ",", 
            sep = "\t")


