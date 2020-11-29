#-----------------------------------------------------------------------------------------
#           NÚMERO DE LOCALIZACIONES Y FECHAS DE SEGUIMIENTO DE LAS PERDICES
#                                 ROCIO TARJUELO
#-----------------------------------------------------------------------------------------

# LIBRARIES-------------------------------------------------------------------------------
library(chron)
library(rgdal)

# FUNCTIONS-------------------------------------------------------------------------------
# Function to obtain the Julian date from 1 Jan
# The origin date is considered as 0 (the last day of the previous year)
Julian.date <- function(d, m, y, y2){
  dts <- julian(x = m, d = d, y = y, origin = c(month = 12, day = 31, year = y2 - 1))
  return(dts)
}


# Función para clasificar cada observación en los periodos inicialmente definidos para 
# analizar: 1-Dic al 24-Feb prereproducción; 7-Marzo al 31 de Mayo preyreproduccion
# 11 de Junio hasta 31-Agosto Reproducción; las observaciones que caen fuera de esa
# ventana temporal se eliminan (= "out")
Periodo <- function(m, d, y){
  if(m == 12){
    int <- paste("Pre", y + 1, sep = ";")  
  } else if (m == 1){
    int <- paste("Pre", y, sep = ";")
  } else if (m == 2 & d <= 24) {
    int <- paste("Pre", y, sep = ";")
  } else if (m == 3 & d > 7){
    int <- paste("PreRep", y, sep = ";")
  } else if (m == 4 | m == 5){
    int <- paste("PreRep", y, sep = ";")
  } else if (m == 6 & d > 10){
    int <- paste ("Rep", y, sep = ";")
  } else if (m == 7 | m == 8){
    int <- paste ("Rep", y, sep = ";")
  } else {
    int <- paste("out", y, sep = ";") 
  }
  return(int)
}


#-----------------------------------------------------------------------------------------
#         START ANALYSIS
#-----------------------------------------------------------------------------------------


# Load data-------------------------------------------------------------------------------
datos <- read.table("data/D_XYgps_positions_no_regadio_ETRS89_tot.txt", header = T, 
                    dec = ",", sep = "\t")

head(datos)
str(datos)
table(datos$Logger_ID)

year.min <- min(datos$Year)
datos$Julian <- mapply(Julian.date, datos$Day, datos$Month, datos$Year, year.min)

Bird.ID <- unique(datos$Logger_ID)

# Start and end dates of records
min.fecha <- character()
max.fecha <- character()
n.obs <- numeric()
n.dias <- numeric()
for (i in 1:length(Bird.ID)){
  Bird.case <- subset(datos, Logger_ID%in%Bird.ID[i])
  min.fecha[i] <- paste(Bird.case[which.min(Bird.case$Julian), 
                                  c("Day","Month","Year")], collapse="_")
  max.fecha[i] <- paste(Bird.case[which.max(Bird.case$Julian), 
                                  c("Day","Month","Year")], collapse="_")
  n.obs[i] <- dim(Bird.case)[1]
  n.dias[i] <- max(Bird.case$Julian) - min(Bird.case$Julian)
}

fechas.seguimiento <- cbind.data.frame(Bird.ID, min.fecha, max.fecha, n.dias, n.obs)


# Eliminar los meses que no consideramos para analizar: Sep; Oct; Nov
datos2 <- datos[!(datos$Month == 9 | datos$Month == 10 | datos$Month == 11), ]
table(datos2$Month)

# Eliminar los datos con un time-lag corto (errores). Ver final código de home range tot
casos.remove <- c("15642", "13272", "26634", "9739", "11918", "21350")
datos.def <- subset(datos2, !Field1%in%casos.remove)

# Asignar periodos
# Asignar época (reproduccion o invernada) a cada localizacion
datos.def$periodo <- as.factor(mapply(Periodo, m = datos.def$Month, d = datos.def$Day, 
                                      y = datos.def$Year))
str(datos.def)
out <- strsplit(as.character(datos.def$periodo), ";", fixed = TRUE)
out2 <- do.call(rbind, out)
datos.def$periodo2 <- as.factor(out2[, 1])
str(datos.def)

# Dejar fuera las observaciones que caen fuera de la ventana temporal de los periodos
datos.def.def <- subset(datos.def, periodo2 != "out")

#-----------------------------------------------------------------------------------------
# OUTPUT
#-----------------------------------------------------------------------------------------
fechas.seguimiento[order(fechas.seguimiento$Bird.ID), ]

write.table(x = datos.def.def, file = "data/D_gangas_no_regadio_ETRS89_tot.txt", 
            sep = "\t", dec = ",", col.names = T, row.names = F)


#-----------------------------------------------------------------------------------------
