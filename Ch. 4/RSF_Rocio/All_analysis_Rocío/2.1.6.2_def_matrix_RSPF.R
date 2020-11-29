#-----------------------------------------------------------------------------------------
#                   CREAR LA MATRIZ DEFINITIVA PARA LAS RSPF
#                              ROCIO TARJUELO
#-----------------------------------------------------------------------------------------

# LIBRARIES-------------------------------------------------------------------------------

rm(list = ls())

# FUNCTIONS-------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
#         START ANALYSIS
#-----------------------------------------------------------------------------------------

# Load data-------------------------------------------------------------------------------

datos.use <- read.table("D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/2_datos_use_analysis.txt", header = T, dec = ",")
datos.avai <- read.table("D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/2_datos_avai_analysis.txt", header = T, dec = ",") # ASP: Hay 83.817 porque saca la disponibilidad para los 3 años (27.000 de los puntos originalmente creados x3) 

# Cada fila de puntos aleatorios tiene un nombre distinto
which(table(rownames(datos.avai))>1)

# Eliminar observaciones sin info de h?bitat
#case.remove1 <- which(is.na(apply(datos.use[,9:17], 1, sum))) # ASP: Las columnas son distintas porque no existe la de periodo?
case.remove1 <- which(is.na(apply(datos.use[,8:16], 1, sum)))
datos.use.ok <- datos.use[-case.remove1, ]

case.remove2 <- which(is.na(apply(datos.avai[,21:29], 1, sum))) 
datos.avai.ok <- datos.avai[-case.remove2, ]


# Estandarizar las variables continuas
mean.slope <- mean(c(datos.use.ok$Slope, datos.avai.ok$Slope))
sd.slope <- sd(c(datos.use.ok$Slope, datos.avai.ok$Slope))
datos.use.ok$slope.st <- (datos.use.ok$Slope - mean.slope) / sd.slope
datos.avai.ok$slope.st <- (datos.avai.ok$Slope - mean.slope) / sd.slope
range(c(datos.use.ok$slope.st), datos.avai.ok$slope.st)


carr.na <- which(is.na(datos.avai.ok$carreteras))
# Algunos puntos aleatorios caen en pixeles sin informaci?n, situados sobre una autopista
# o autov?a, as? que a estos les asigno valor 0
datos.avai.ok$carreteras[carr.na] <- 0
which(is.na(datos.avai.ok$carreteras))

mean.carr <- mean(c(datos.use.ok$carreteras, datos.avai.ok$carreteras))
sd.carr <- sd(c(datos.use.ok$carreteras, datos.avai.ok$carreteras))
datos.use.ok$carreteras.st <- (datos.use.ok$carreteras - mean.carr) / sd.carr
datos.avai.ok$carreteras.st <- (datos.avai.ok$carreteras - mean.carr) / sd.carr
range(c(datos.use.ok$carreteras.st), datos.avai.ok$carreteras.st)


which(is.na(datos.avai.ok$caminos))
mean.cam <- mean(c(datos.use.ok$caminos, datos.avai.ok$caminos))
sd.cam <- sd(c(datos.use.ok$caminos, datos.avai.ok$caminos))
datos.use.ok$caminos.st <- (datos.use.ok$caminos - mean.cam) / sd.cam
datos.avai.ok$caminos.st <- (datos.avai.ok$caminos - mean.cam) / sd.cam
range(c(datos.use.ok$caminos.st), datos.avai.ok$caminos.st)

# reemplazar a?o 2016 por 2017 para facilitar el an?lisis
datos.use.ok[datos.use.ok$year == 2016, "year"] <- 2017

# ordenar por a?o e id
datos.use.ord <- datos.use.ok[order(datos.use.ok$year, 
                                    datos.use.ok$Logger_ID),
                                    #datos.use.ok$periodo), 
                                    ]

# Seleccionar los puntos aleatorios que se van a emplear en los an?lisis
# Observaciones que hay por individuo y a?o
n.year.id <- table(list(datos.use.ord$year, datos.use.ord$Logger_ID))
n2017.id <- n.year.id[1, ]
n2018.id <- n.year.id[2, ]
n2019.id <- n.year.id[3, ]

n.id <- 1:length(levels(datos.use.ord$Logger_ID))

# levels(datos.use.ord$Logger_ID) == colnames(n.year.id)  # Mismo orden

# Seleccionar una muestra de los puntos aleatorios teniendo en cuenta los a?os y los
# MCPs de cada individuo
avai.ok.pre17.id.s.list  <- list()
for (i in 1:length(n2017.id)){
  if(n2017.id[i] > 0){
    column <- which(colnames(datos.avai.ok) == names(n2017.id[i])) # Select ID
    avai.ok.pre17 <- subset(datos.avai.ok, year == 2017 & datos.avai.ok[, column] == 1) # Select available points from that ID
    samp17.id <- sample(dim(avai.ok.pre17)[1], size = n2017.id[i])  # Take randomly the same number of random than used point for that year and individual
    avai.ok.pre17.sub <- avai.ok.pre17[samp17.id, ]
    avai.ok.pre17.sub$Logger_ID <- names(n2017.id[i])
    #periodo <- datos.use.ord[datos.use.ord$Logger_ID == names(n2017.id[i]) & # No tengo periodo en mi base de datos
    #                           datos.use.ord$year == 2017, "periodo"] 
    #avai.ok.pre17.sub$periodo <- periodo
    avai.ok.pre17.id.s.list[[i]] <- avai.ok.pre17.sub
  }
}
avai.ok.pre17.id.s <- do.call(rbind.data.frame, avai.ok.pre17.id.s.list)
sum(n2017.id) == dim(avai.ok.pre17.id.s)[1]

avai.ok.pre18.id.s.list  <- list()
for (i in 1:length(n2018.id)){
  if(n2018.id[i] > 0){
    column <- which(colnames(datos.avai.ok) == names(n2018.id[i]))
    avai.ok.pre18 <- subset(datos.avai.ok, year == 2018 & datos.avai.ok[, column] == 1)
    samp18.id <- sample(dim(avai.ok.pre18)[1], size = n2018.id[i])
    avai.ok.pre18.sub <- avai.ok.pre18[samp18.id, ]
    avai.ok.pre18.sub$Logger_ID <- names(n2018.id[i])
    #periodo <- datos.use.ord[datos.use.ord$Logger_ID == names(n2018.id[i]) & 
    #                          datos.use.ord$year == 2018, "periodo"]
    #avai.ok.pre18.sub$periodo <- periodo
    avai.ok.pre18.id.s.list[[i]] <- avai.ok.pre18.sub
  }
}
avai.ok.pre18.id.s <- do.call(rbind.data.frame, avai.ok.pre18.id.s.list)
sum(n2018.id) == dim(avai.ok.pre18.id.s)[1]

avai.ok.pre19.id.s.list  <- list()
for (i in 1:length(n2019.id)){
  if(n2019.id[i] > 0){
    column <- which(colnames(datos.avai.ok) == names(n2019.id[i]))
    avai.ok.pre19 <- subset(datos.avai.ok, year == 2019 & datos.avai.ok[, column] == 1)
    samp19.id <- sample(dim(avai.ok.pre19)[1], size = n2019.id[i])
    avai.ok.pre19.sub <- avai.ok.pre19[samp19.id, ]
    avai.ok.pre19.sub$Logger_ID <- names(n2019.id[i])
    #periodo <- datos.use.ord[datos.use.ord$Logger_ID == names(n2019.id[i]) & 
    #                           datos.use.ord$year == 2019, "periodo"]
    #avai.ok.pre19.sub$periodo <- periodo
    avai.ok.pre19.id.s.list[[i]] <- avai.ok.pre19.sub
  }
}
avai.ok.pre19.id.s <- do.call(rbind.data.frame, avai.ok.pre19.id.s.list)
sum(n2019.id) == dim(avai.ok.pre19.id.s)[1]

# Unimos la info de todos los puntos aleatorios
datos.avai.def <- rbind.data.frame(avai.ok.pre17.id.s[,-(9:20)],
                                   avai.ok.pre18.id.s[,-(9:20)], 
                                   avai.ok.pre19.id.s[,-(9:20)])

# Check que haya mismo orden en observaciones y puntos aleatorios respecto a a?o, 
# individuo y periodo
which((datos.avai.def$year == datos.use.ord$year)==F)
which((datos.avai.def$Logger_ID == datos.use.ord$Logger_ID)==F)
which((datos.avai.def$periodo == datos.use.ord$periodo)== F)

# Unir la informaci?n de observaciones y puntos aleatorios.
datos.tot <- rbind.data.frame(datos.use.ord, datos.avai.def)

# A?ado una nueva columna Logger_ID2 donde indico a qu? individuo pertenece cada 
# localizaci?n para las observaciones, y para los puntos aleatorios, en qu? MCP caen.
# A?adir informaci?n en el campo Logger ID de en qu? MCP cae cada punto aleatorio.
# datos.tot$Logger_ID2 <- rep(c(rep(n.id, as.numeric(n2017.id)), 
#                               rep(n.id, as.numeric(n2018.id)),
#                               rep(n.id, as.numeric(n2019.id))), 2)

#datos.tot$Logger_ID == datos.tot$Logger_ID2
str(datos.tot)

#-----------------------------------------------------------------------------------------
#         OUTPUT
#-----------------------------------------------------------------------------------------
#write.table(datos.tot, "data/2_matrix_RSPF.txt", col.names = T, dec = ",", sep = "\t")

