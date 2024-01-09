

## -------------------------------------------------
##              Rev1: Check aggregation    
## ------------------------------------------------- 
# Code RT to get the % of positions < 100 m = belonging to the same flock

rm(list=ls())

# FUNCTIONS
# Function to compute euclidean distance between two points
# DE = square root ((x2-x1)^2+(y2-y1)^2)
eucl.dist<-function(x1,x2,y1,y2){
  dist.x<-(x2-x1)^2
  dist.y<-(y2-y1)^2
  squareroot<-sqrt(dist.x+dist.y)
  names(squareroot)<-"Dist(m)"
  return(squareroot)
}

## ---- Calculate simple aggregation with data from 1st submission paper ----
##                      (Distance between locations)

# Load GPS locations of data used paper for RSPF

data <- read.table("D:/PhD/Fourth chapter/Data/2_matrix_RSPF.txt", header = T, dec = ",")
data <- data[data$STATUS == 1, c(2:20)]

datos.periodo <- subset(data, periodo%in%"Rep") # 1. Period: Especially interested in winter 

datos.periodo$fecha <- as.Date(paste(datos.periodo$day,datos.periodo$month,datos.periodo$year,sep = "/"), format = "%d/%m/%Y")
dias <- names(table(datos.periodo$fecha))

distancias.periodo <- list()

for(k in 1:length(dias)){
  datos.dia <- subset(datos.periodo, fecha == dias[k])
  
  if(dim(datos.dia)[1] > 1){  # Si solo hay una observación no se hace nada
    distances.dia <- list()
    
    for (i in 1:(dim(datos.dia)[1]-1)){  # El último loop no tiene ya datos. 
      obs.x <- datos.dia$x[1]           # No aparece i porque se elimina el primero
      obs.y <- datos.dia$y[1]
      obs <- datos.dia$Logger_ID[1]
      # Eliminar el primer registro de la matriz en cada loop que corresponde con el loop
      # Al eliminar el primer registro, al volverse a correr el loop coge el primer registro
      # Esta es la forma de que se calcule la distancia de todas a todas las locations
      datos.dia <- datos.dia[-1,]
      distances <- matrix(NA, nrow = dim(datos.dia)[1], ncol = 3)
      for (j in 1:dim(datos.dia)[1]){
        distances[j,1] <- obs
        distances[j,2] <- datos.dia$Logger_ID[j]
        distances[j,3] <- eucl.dist(x1 = obs.x, x2 = datos.dia$x[j],
                                    y1 = obs.y, y2 = datos.dia$y[j])
      }
      distances.dia[[i]] <- distances
    }
    distancias.periodo[[k]] <- do.call(rbind, distances.dia)
    rm(distances)
  }
}

distancias.def <- do.call(rbind.data.frame, distancias.periodo)
colnames(distancias.def) <- c("Obs1", "Obs2", "dist(m)")
distancias.def$bando <- ifelse(distancias.def[, 3] < 100, 1, 0)
table(distancias.def$bando)
prop.bando <- length(which(distancias.def$bando == 1)) / dim(distancias.def)[1] * 100
prop.bando # % de localizaciones diarias a menos de 100 m, teniendo en cuenta las localizaciones del mismo individuo

# Eliminando la distancia entre las localizaciones del mismo individuo (que seria mas autocorrelacion temporal que pertenencia a un bando)

distancias.def$sameind <- ifelse(distancias.def[, 1] == distancias.def[, 2], 1, 0)
distancias.def_difind <- distancias.def[-which(distancias.def$sameind == 1),]
prop.bando <- length(which(distancias.def_difind$bando == 1)) / dim(distancias.def_difind)[1] * 100
prop.bando

## ---- Simple aggregation with all data (including newdata revision and flying locations) ----

rm(list=ls())

eucl.dist<-function(x1,x2,y1,y2){
  dist.x<-(x2-x1)^2
  dist.y<-(y2-y1)^2
  squareroot<-sqrt(dist.x+dist.y)
  names(squareroot)<-"Dist(m)"
  return(squareroot)
}

# Load all GPS locations of data used paper + new locations

setwd("D:/PhD/Fourth chapter/Data")
data <- read.csv("allgps_newDataPre_revision.csv")

datos.periodo <- subset(data, periodo%in%"Rep") # 1. Period: Especially interested in winter 

dias <- names(table(datos.periodo$fecha))

distancias.periodo <- list()

for(k in 1:length(dias)){
  datos.dia <- subset(datos.periodo, fecha == dias[k])
  
  if(dim(datos.dia)[1] > 1){  # Si solo hay una observación no se hace nada
    distances.dia <- list()
    
    for (i in 1:(dim(datos.dia)[1]-1)){  # El último loop no tiene ya datos. 
      obs.x <- datos.dia$x[1]           # No aparece i porque se elimina el primero
      obs.y <- datos.dia$y[1]
      obs <- datos.dia$Logger_ID[1]
      # Eliminar el primer registro de la matriz en cada loop que corresponde con el loop
      # Al eliminar el primer registro, al volverse a correr el loop coge el primer registro
      # Esta es la forma de que se calcule la distancia de todas a todas las locations
      datos.dia <- datos.dia[-1,]
      distances <- matrix(NA, nrow = dim(datos.dia)[1], ncol = 3)
      for (j in 1:dim(datos.dia)[1]){
        distances[j,1] <- obs
        distances[j,2] <- datos.dia$Logger_ID[j]
        distances[j,3] <- eucl.dist(x1 = obs.x, x2 = datos.dia$x[j],
                                    y1 = obs.y, y2 = datos.dia$y[j])
      }
      distances.dia[[i]] <- distances
    }
    distancias.periodo[[k]] <- do.call(rbind, distances.dia)
    rm(distances)
  }
}

distancias.def <- do.call(rbind.data.frame, distancias.periodo)
colnames(distancias.def) <- c("Obs1", "Obs2", "dist(m)")
distancias.def$`dist(m)` <- as.numeric(distancias.def$`dist(m)`)

distancias.def$bando <- ifelse(distancias.def[, 3] < 100, 1, 0)
table(distancias.def$bando)
prop.bando <- length(which(distancias.def$bando == 1)) / dim(distancias.def)[1] * 100
prop.bando # % de localizaciones diarias a menos de 100 m, teniendo en cuenta las localizaciones del mismo individuo

# Eliminando la distancia entre las localizaciones del mismo individuo (que seria mas autocorrelacion temporal que pertenencia a un bando)

distancias.def$sameind <- ifelse(distancias.def[, 1] == distancias.def[, 2], 1, 0)
distancias.def_difind <- distancias.def[-which(distancias.def$sameind == 1),]
prop.bando <- length(which(distancias.def_difind$bando == 1)) / dim(distancias.def_difind)[1] * 100
prop.bando




## ---- Index of dynamic interaction ----

rm(list=ls())

library(adehabitatLT)
library(wildlifeDI)
library(dplyr)

# Load trajectories
# This is from when I was trying to add individuals
#setwd("D:/PhD/Fourth chapter/Data")
#data <- read.csv("allgps_newDataPre_revision.csv")

# Load all GPS locations of data used paper 

datos.allgps <- read.table("D:/PhD/Fourth chapter/GPS Cataluña/Ganga/Updated_24-2-2020/FINAL_ALLpos_no_regadio_ETRS89.txt", header = T, dec = ",",
                           sep = "\t")
colnames(datos.allgps)[8] <- "Y_Lat"
colnames(datos.allgps)[9] <- "X_Lon"
colnames(datos.allgps)[13] <- "periodo"

# Date and time columns
datos.allgps$fecha <- as.Date(paste(datos.allgps$Day,datos.allgps$Month,datos.allgps$Year,sep = "/"), format = "%d/%m/%Y")
datos.allgps$hora <- as.POSIXlt(paste(datos.allgps$Hour,datos.allgps$Minute,datos.allgps$Second,sep = ":"), format = "%H:%M:%OS")
datos.allgps$hora <- format(datos.allgps$hora,"%H:%M:%OS")

# Transform coordinate system to make it fit
datos.allgps$Y_Lat <- as.numeric(datos.allgps$Y_Lat)
datos.allgps$X_Lon <- as.numeric(datos.allgps$X_Lon)
coordinates(datos.allgps) <- datos.allgps[,c("X_Lon","Y_Lat")]
datos.allgps@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Coordinate system of origin is WGS84

CRS.used <- "+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs"

datos.allgps <- spTransform(datos.allgps, CRS.used)
datos.allgps$x <- coordinates(datos.allgps)[,1]
datos.allgps$y <- coordinates(datos.allgps)[,2]

datos.allgps$STATUS <- 1 # Used locations
datos.allgps$SpeedUnit <- "knots"

names(datos.allgps)
datos.allgps <- datos.allgps[,c(1,15,16,8,9,17,18,13,19,10,20)]

datos.periodo <- subset(datos.allgps, datos.allgps$periodo %in%"Rep")

# Summary locations

datos.periodo@data %>% group_by(Logger_ID) %>%
  summarise(n())

# Prepare for ltraj
coordinates(datos.periodo) <- datos.periodo[,c(7,8)] # Coordinates
xy <- coordinates(datos.periodo)
timeDate <- as.POSIXct(paste(datos.periodo$fecha,datos.periodo$hora)) # Date
id <- datos.periodo$Logger_ID

indiv <- unique(datos.periodo$Logger_ID)
trajectories <- list()

for (i in 1:length(indiv)){
  tr <- as.ltraj(xy[id == indiv[i], ],
                 timeDate[id == indiv[i]],
                 id[id == indiv[i]],
                 typeII = TRUE)
  trajectories[[i]] <- tr
}

# Matrix to store coefficient

index <- expand.grid(1:8,1:8)
index <- index[-which(index[,1] == index[,2]), ]
index$Ca <- NA

for(i in 1:nrow(index)){
  over <- checkTO(trajectories[[index[i,1]]],trajectories[[index[i,2]]])
  if(over$TO == FALSE) next
  Ca <- Ca(trajectories[[index[i,1]]],trajectories[[index[i,2]]], tc = 3600*24, dc = 500) #3600 seg tiene 1 h * 24 h
  index$Ca[i] <- Ca
}

3600*24

## ---- Flock association: FRANCESC CUSCO, I DON'T UNDERSTAND ----
# Calculate Half-Weight Index (HWI) for flock association (Cusco et. al) within loop
# HWIab = x / ½ (ya + yb), x is the number of times in which two females (a and b) were associated in a same flock 
#ya and yb correspond to the total of locations for the female a and for the female b

setwd("D:/PhD/Fourth chapter/Data")
data <- read.csv("allgps_newDataPre_revision.csv")

datos.periodo <- subset(data, periodo%in%"Pre") # 1. Period: Especially interested in winter 

distancias.periodo <- list()

for(k in 1:length(dias)){
  datos.dia <- subset(datos.periodo, fecha == dias[k])
  
  if(dim(datos.dia)[1] > 1){  # Si solo hay una observación no se hace nada
    distances.dia <- list()
    
    for (i in 1:(dim(datos.dia)[1]-1)){  # El último loop no tiene ya datos. 
      obs.x <- datos.dia$x[1]           # No aparece i porque se elimina el primero
      obs.y <- datos.dia$y[1]
      obs <- datos.dia$Logger_ID[1]
      # Eliminar el primer registro de la matriz en cada loop que corresponde con el loop
      # Al eliminar el primer registro, al volverse a correr el loop coge el primer registro
      # Esta es la forma de que se calcule la distancia de todas a todas las locations
      datos.dia <- datos.dia[-1,]
      distances <- matrix(NA, nrow = dim(datos.dia)[1], ncol = 3)
      for (j in 1:dim(datos.dia)[1]){
        distances[j,1] <- obs
        distances[j,2] <- datos.dia$Logger_ID[j]
        distances[j,3] <- eucl.dist(x1 = obs.x, x2 = datos.dia$x[j],
                                    y1 = obs.y, y2 = datos.dia$y[j])
      }
      distances.dia[[i]] <- distances
    }
    all.distances.dia <- do.call(rbind.data.frame, distances.dia)
    
    # Calculo HWI
    
    all.distances.dia$sameind <- ifelse(all.distances.dia[, 1] == all.distances.dia[, 2], 1, 0) # Only distances between different individuals
    all.distances.dia$bando <- ifelse(as.numeric(all.distances.dia[, 3]) < 100, 1, 0)
    
    
    
    # Remove the data on the distances between the same individual
    dif_id <- all.distances.dia[-which(all.distances.dia$sameind == 1), ]
    
    if(nrow(dif_id) == 0) next # If there are no different individuals I don't calculate it (k = 17 to try)
    
    #Check whether they belong to the same flock 
    dif_id$bando <- ifelse(as.numeric(dif_id[, 3]) < 100, 1, 0)
    
    id <- unique(c(dif_id$V1,dif_id$V2)) # Individuals
    if(length(id) > 2) {print("MORETHAN1")}
    
    # HWIab = x / ½ (ya + yb) 
    
    x <- sum(dif_id$bando)
    
    x/nrow(all.distances.dia)
    datos.dia <- subset(datos.periodo, fecha == dias[k]) # Reestablecer datos dias para contar numero de posiciones de cada individuo
    yA <- nrow(datos.dia[which(datos.dia$Logger_ID %in% id[1]), ])
    yB <- nrow(datos.dia[which(datos.dia$Logger_ID %in% id[2]), ])
    
    hwi_day <- x/(yA+yB) 
    
    distancias.periodo[[k]] <- do.call(rbind, distances.dia)
    rm(distances)
  }
}



