
## -------------------------------------------------
##            GPS DATA - SupMaterial
## ------------------------------------------------- 



rm(list = ls())

datos <- read.table("D:/PhD/Fourth chapter/Data/2_matrix_RSPF.txt", header = T, dec = ",")

periods <- c("Pre", "PreRep", "Rep")

locations_ID <- data.frame(matrix(0, ncol = length(periods), nrow = length(unique(datos$Logger_ID))))
colnames(locations_ID) <- periods
rownames(locations_ID) <- unique(datos$Logger_ID)

loc <- list()

for (i in 1:length(periods)) {
  
  datos.rspf <- subset(datos, periodo%in%periods[i])
  
  n.id <- table(list(datos.rspf[datos.rspf$STATUS == 1, "Logger_ID"]))
  
  locations_ID[rownames(locations_ID) %in% names(n.id), colnames(locations_ID) %in% periods[i]] <- n.id
}

setwd("D:/PhD/Fourth chapter/Results")
write.csv(locations_ID, file = "SM_GPS_info.csv")

# Check start and end dates

d <- datos[which(datos$Logger_ID == "GUE05" & datos$STATUS == 1), ]
d[1, 2:5]
d[nrow(d), 2:5]
