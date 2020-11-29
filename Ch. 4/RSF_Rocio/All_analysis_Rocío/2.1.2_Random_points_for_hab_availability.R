#-----------------------------------------------------------------------------------------
#           GENERAR PUNTOS ALEATORIOS PARA CALCULAR LA DISPONIBILIDAD DE HABITAT
#                               ROCIO TARJUELO
#-----------------------------------------------------------------------------------------

# LIBRARIES-------------------------------------------------------------------------------
library(rgeos)
library(rgdal)
library(spatstat)


# FUNCTIONS-------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
#         START ANALYSIS
#-----------------------------------------------------------------------------------------

# Load data-------------------------------------------------------------------------------
Bird <- read.table("D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/D_XYgps_positions_no_regadio_ETRS89_tot.txt", header = T, dec = ",",
                     sep = "\t")
Bird.ID <- unique(Bird$Logger_ID)

# Maps
study.site <- readOGR(dsn = "D:/PhD/Fourth chapter/Data/Full_analysis_Rocio", layer = "MCP_tot")

MCP <- list()
for (i in 1:length(Bird.ID)){
  MCP[[i]] <- readOGR(dsn = "D:/PhD/Fourth chapter/Data/Full_analysis_Rocio", layer = paste("mcp", Bird.ID[i], sep = "_"))
}
names(MCP) <- Bird.ID 


# Generate random points within the study site limits ------------------------------------
area.studysite <- gArea(study.site, byid = F)/1000000
n.puntos <- round(area.studysite * 500, 0)
random.pts <- spsample(x = study.site, n = n.puntos, type = "random")

plot(study.site)
points(random.pts)

pts <- cbind.data.frame(X_25831 = random.pts$x, Y_25831 = random.pts$y)

dim(pts)

# Because spsample generates an estimate sample size in non-square random box, I test 
# whether the number of points generated is equal to the number of points pre-defined.

dim(pts)[1] == n.puntos

pts$id <- 1:dim(pts)[1]
str(pts)

# Indicar en que MCP individuales cae un determinado punto aleatorio
ID <- data.frame(Id = pts$id)
xyID <- pts[, c("X_25831", "Y_25831")]
coordinates(ID) <- xyID
ID@proj4string <- study.site@proj4string 

pts.mcp <- matrix(NA, nrow = dim(pts)[1], ncol = length(Bird.ID))
for (i in 1:length(MCP)){
  puntos <- over(ID, MCP[[i]])
  pts.mcp[, i] <- puntos$id
}

# Cuando no hay solapamiento entre poligono y punto asigna NA, los sustituyo por 0
pts.mcp[which(is.na(pts.mcp))] <- 0
pts.mcp2 <- cbind(pts, pts.mcp)
colnames(pts.mcp2)[4:15] <- as.character(Bird.ID)


# comprobacion de que nombre de filas corresponde con el id del objeto espacial de los
# puntos "ID"
# casos <- which(puntos$id == 1)
# puntos1 <- ID[casos,]
# plot(MCP[[i]])
# points(ID)
# points(puntos1, col = "red")0

# ASP: Cuantos puntos hay por home range?
colSums(as.numeric(as.data.frame(pts.mcp)))
pts.mcp_123 <- as.data.frame(pts.mcp)
pts.mcp_123 <- sapply(pts.mcp_123,as.numeric)
colSums(pts.mcp_123) # Different number of locations per MCP?

#-----------------------------------------------------------------------------------------
#         OUTPUT
#-----------------------------------------------------------------------------------------

write.table(x = pts.mcp2, file="D:/PhD/Fourth chapter/Data/Full_analysis_Rocio/2_random_points.txt", sep = "\t", dec = ",", 
            col.names = T, row.names = F)



