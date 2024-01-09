#-----------------------------------------------------------------------------------------
#       DISTANCE BETWEEN BIRDS IN THE SAME FLOCK TO REPLY THE COMMENT OF A REVIEWER
#                               ROCIO TARJUELO
#-----------------------------------------------------------------------------------------

# LIBRARIES-------------------------------------------------------------------------------


# FUNCTIONS-------------------------------------------------------------------------------
# Function to compute euclidean distance between two points
# DE = square root ((x2-x1)^2+(y2-y1)^2)
eucl.dist<-function(x1,x2,y1,y2){
  dist.x<-(x2-x1)^2
  dist.y<-(y2-y1)^2
  squareroot<-sqrt(dist.x+dist.y)
  names(squareroot)<-"Dist(m)"
  return(squareroot)
}

# Funci?n para convertir hora:min a decimal
Hora <- function(x1, x2) {
  x1 + (x2/60)
}

#-----------------------------------------------------------------------------------------
#         START ANALYSIS
#-----------------------------------------------------------------------------------------

# Load data-------------------------------------------------------------------------------
setwd("D:/PhD/MyScripts_PhD/Ch. 4/All_analyses_ASP/Revision1/Example_rocio")
datos<-read.table("5_habitat_use_bird_locations_subsample.txt", header=T, dec=",", sep="\t")

which(is.na(datos$CODE_12))
# No hay obs sin datos de h?bitat


# Distancias entre individuos del mismo bando para todo el conjunto de datos -------------
datos.W <- subset(datos, YEAR_INT%in%"B")
dias <- names(table(datos.W$DATE2))
distancias.w <- list()
for(k in 1:length(dias)){
  datos.dia <- subset(datos.W, DATE2 == dias[k])
  if(dim(datos.dia)[1] > 1){  # Si solo hay una observaci?n no se hace nada
    distances.dia <- list()
    for (i in 1:(dim(datos.dia)[1]-1)){  # El ?ltimo loop no tiene ya datos
      obs.x <- datos.dia$X_ETRS89[1]
      obs.y <- datos.dia$Y_ETRS89[1]
      obs <- datos.dia$ID_Todos[1]
      # Eliminar el primer registro de la matriz en cada loop que corresponde con el loop
      datos.dia <- datos.dia[-1,]
      distances <- matrix(NA, nrow = dim(datos.dia)[1], ncol = 3)
      for (j in 1:dim(datos.dia)[1]){
        distances[j,1] <- obs
        distances[j,2] <- datos.dia$ID_Todos[j]
        distances[j,3] <- eucl.dist(x1 = obs.x, x2 = datos.dia$X_ETRS89[j],
                                    y1 = obs.y, y2 = datos.dia$Y_ETRS89[j])
      }
      distances.dia[[i]] <- distances
    }
    distancias.w[[k]] <- do.call(rbind, distances.dia)
    rm(distances)
  }
}

distancias.def <- do.call(rbind.data.frame, distancias.w)
colnames(distancias.def) <- c("Obs1", "Obs2", "dist(m)")
distancias.def$bando <- ifelse(distancias.def[, 3] < 100, 1, 0)
table(distancias.def$bando)
prop.bando <- length(which(distancias.def$bando == 1)) / dim(distancias.def)[1] * 100
prop.bando
