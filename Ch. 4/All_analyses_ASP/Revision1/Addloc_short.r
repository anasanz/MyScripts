

rm(list=ls())

library(dplyr)

## ---- Locations used in RSPF ----

datos_all <- read.table("D:/PhD/Fourth chapter/Data/2_matrix_RSPF.txt", header = T, dec = ",")
datos <- datos_all[datos_all$STATUS == 1, c(2:20)]


datos$fecha <- as.Date(paste(datos$day,datos$month,datos$year,sep = "/"), format = "%d/%m/%Y")
datos <- datos %>% arrange(Logger_ID,fecha)

# Check dates pre-bottleneck
datos.periodo <- subset(datos, periodo%in%"Pre") # 1. Period: Especially interested in winter 
sum_datos_pre1 <- datos.periodo %>% group_by(Logger_ID) %>%
  summarise(min(fecha),
            max(fecha))


## ---- Locations that can be added  ----

setwd("D:/PhD/Fourth chapter/Data")
datos.periodo.extra <- read.csv("Consulta4_Gangas_Datos ultimos años.csv", sep = ";")
datos.periodo.extra <- datos.periodo.extra[,c(1,6,7,8,9)]

# Format date-hour
fh <- t(as.data.frame(strsplit(datos.periodo.extra$Fecha_hora,' '))) 
datos.periodo.extra <- cbind(datos.periodo.extra,fh)

colnames(datos.periodo.extra)[1] <- "Logger_ID"
colnames(datos.periodo.extra)[3] <- "Y_Lat"
colnames(datos.periodo.extra)[4] <- "X_Lon"
colnames(datos.periodo.extra)[5] <- "Speed" #♥ En km/h
colnames(datos.periodo.extra)[6] <- "fecha"
colnames(datos.periodo.extra)[7] <- "hora"




datos.periodo.extra$fecha <- as.Date(datos.periodo.extra$fecha, format = "%d/%m/%Y")
datos.periodo.extra <- datos.periodo.extra %>% mutate(year = lubridate::year(fecha), 
                                                      month = lubridate::month(fecha), 
                                                      day = lubridate::day(fecha))

# Select dates prebottleneck (1Dec-25 Feb)
datos.periodo.extra <- datos.periodo.extra[which(datos.periodo.extra$month == 12 | 
                                                   datos.periodo.extra$month == 1 | 
                                                   datos.periodo.extra$month == 2), ]
datos.periodo.extra <- datos.periodo.extra[-which(datos.periodo.extra$month == 2 &
                                                    datos.periodo.extra$day > 25), ]

# Select locations from Cataluña only

datos.periodo.extra <- datos.periodo.extra[which(datos.periodo.extra$X_Lon > 0.59), ] # Select positions from Cataluña


sum_datos_pre2 <- datos.periodo.extra %>% group_by(Logger_ID) %>%
  summarise(min(fecha),
            max(fecha))

# From this data, I need to add:
#   - all locations from the short cereal period of: GUE01 and GUE05
#   - locations of PIC02: Del 01/12/2019 - 25/02/2020

# So I remove the locations of PIC02 before 14/02/2019, already contained in our data.
# and these are the extra locations to add to our analysis:
datos.periodo.extra <- datos.periodo.extra[-which(datos.periodo.extra$Logger_ID == "PIC02"
                                                 & datos.periodo.extra$fecha < "2019-02-14"), ]

# Transform coordinate system to make it fit
datos.periodo.extra$Y_Lat <- as.numeric(datos.periodo.extra$Y_Lat)
datos.periodo.extra$X_Lon <- as.numeric(datos.periodo.extra$X_Lon)
coordinates(datos.periodo.extra) <- datos.periodo.extra[,c("X_Lon","Y_Lat")]
datos.periodo.extra@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # Coordinate system of origin is WGS84

CRS.used <- "+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs"

datos.periodo.extra <- spTransform(datos.periodo.extra, CRS.used)
datos.periodo.extra$x <- coordinates(datos.periodo.extra)[,1]
datos.periodo.extra$y <- coordinates(datos.periodo.extra)[,2]

# Add extra fields

datos.periodo.extra$periodo <- "Pre"
datos.periodo.extra$STATUS <- 1

rownames(datos.periodo.extra@data) <- seq(1,nrow(datos.periodo.extra))
names(datos.periodo.extra) # De aqui retomo para quitar flying locations, pero primero lo uno con la de all gps


## ---- Join with whole dataset for movement analysis ----

# Format new data to join with whole dataset

datos.periodo.extra2 <- datos.periodo.extra[ ,c(1,6,7,3,4,11,12,13,14,5)]
datos.periodo.extra2$SpeedUnit <- "km/h"
names(datos.periodo.extra2)

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

# Join
datos_movement <- rbind(datos.allgps@data,datos.periodo.extra2@data)

setwd("D:/PhD/Fourth chapter/Data")
write.csv(datos_movement, file = "allgps_newDataPre_revision.csv")



## ---- Remove flying locations (>1.5 knots) for RSPF; There is also no incubation in winter ----
# 1.5 knots = 27,78 km/h

datos.periodo.extra <- datos.periodo.extra[,c(1,8,9,10,13,11,12,5,6,4,3,14)]

rownames(datos.periodo.extra@data) <- seq(1,nrow(datos.periodo.extra))
names(datos.periodo.extra)

datos.periodo.extra@data$Speed <- as.numeric(datos.periodo.extra@data$Speed)

datos.periodo.extra <- datos.periodo.extra[-which(datos.periodo.extra@data$Speed > 27,78), ]


setwd("D:/PhD/Fourth chapter/Data")
write.csv(datos.periodo.extra@data, file = "matrix_RSPF_newDataPre_revision.csv")
# From here, I could check if there are locations in irrigation? JOIN AND SAVE:

## ---- Join data without flying locations for rspf ----

# Load GPS locations

data <- read.table("D:/PhD/Fourth chapter/Data/2_matrix_RSPF.txt", header = T, dec = ",")
data <- data[data$STATUS == 1, c(2:20)]

# Join with new data pre-bottleneck

data.pre <- read.csv("matrix_RSPF_newDataPre_revision.csv")
data.pre <- data.pre[ ,-c(1,9:13)]

df <- data.frame(matrix(NA,nrow = nrow(data.pre), ncol = 12)) # Same columns than in matrix rspf
colnames(df) <- colnames(data)[8:19]
data.pre <- cbind(data.pre,df)

data <- rbind(data,data.pre)

# Save to check

setwd("D:/PhD/Fourth chapter/Data")

write.csv(data, file = "matrix_RSPF_all_revision.csv")

# CHECKED IN ARCGIS: Ninguna en regadío, todas dentro del MCP TOTAL

