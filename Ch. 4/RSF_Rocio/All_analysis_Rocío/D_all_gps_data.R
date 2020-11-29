#-----------------------------------------------------------------------------------------
#          AÑADIR NUEVAS OBSERVACIONES A LA PRIMERA MATRIZ DE LOCALIZACIONES DE ANA
#                                 ROCIO TARJUELO
#-----------------------------------------------------------------------------------------

# LIBRARIES-------------------------------------------------------------------------------
library(rgdal)
library(sp)
# FUNCTIONS-------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
#         START ANALYSIS
#-----------------------------------------------------------------------------------------

# Load data-------------------------------------------------------------------------------
ganga <- readOGR(dsn = "data/GIS", layer = "XYgps_positions_no_regadio_ETRS89")
new.data <- readOGR(dsn = "data/GIS", layer = "XYPIC02_20200422_ETRS89")

colnam.ganga <- colnames(ganga@data)
colnam.new <- colnames(new.data@data)

# Quedarme solo con las columnas que hay en "ganga"
new.data2  <- new.data[ , colnam.new[colnam.new%in%colnam.ganga]]
colnames(new.data2@data)

# Hay que añadir Field1. Lo elimino de ganga y lo añado a posteriori
# Eliminar incu y rem
ganga2 <- ganga[, !colnames(ganga@data)%in%c("Field1", "incu", "rem")]
colnames(ganga2@data) == colnames(new.data2@data)

# Adaptar el tipo de variables
str(ganga2@data)
ganga2@data[, c(1, 11, 12)] <- apply(ganga2@data[, c(1, 11, 12)], 2, 
                                     function(x) as.character(x))
ganga2@data[, c(2:10, 13:14)] <- apply(ganga2@data[, c(2:10, 13:14)], 2, 
                                     function(x) as.numeric(as.character(x)))
str(ganga2@data)

str(new.data2@data)
new.data2@data[, c(1, 11, 12)] <- apply(new.data2@data[, c(1, 11, 12)], 2, 
                                     function(x) as.character(x))
new.data2@data[, c(2:7, 13:14)] <- apply(new.data2@data[, c(2:7, 13:14)], 2, 
                                       function(x) as.numeric(as.character(x)))
str(new.data2@data)

obs.total <- rbind(ganga2, new.data2)
# Mantengo los mismos Field1 originales
new.field.id <- max(as.numeric(as.character(ganga$Field1))) + 1

obs.total$Field1 <- c(as.numeric(as.character(ganga$Field1)),
                      new.field.id:(new.field.id + length(new.data2)-1))

#-----------------------------------------------------------------------------------------
# OUTPUT
#-----------------------------------------------------------------------------------------
write.table(x = obs.total, file = "data/D_XYgps_positions_no_regadio_ETRS89_tot.txt",
            sep = "\t", dec = ",", col.names = T, row.names = F)

writeOGR(obs.total, dsn = "data/GIS", layer = "D_XYgps_positions_no_regadio_ETRS89_tot", 
         driver="ESRI Shapefile")


