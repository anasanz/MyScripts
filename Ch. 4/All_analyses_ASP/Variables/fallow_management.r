


rm(list=ls())

library(rgdal)
library(raster)

# Fallow variable

gest17 <- readOGR("D:/PhD/Fourth chapter/GIS/Capas_Rocío", "Gestion17_4326")
gest18 <- readOGR("D:/PhD/Fourth chapter/GIS/Capas_Rocío", "Gestion18_4326")
gest19 <- readOGR("D:/PhD/Fourth chapter/GIS/Capas_Rocío", "Gestion19_4326")


#### Load clips DUN ####
dun17 <- readOGR("D:/PhD/Fourth chapter/GIS/SIGPAC_DUN", "clip_dun17_4326")
dun18 <- readOGR("D:/PhD/Fourth chapter/GIS/SIGPAC_DUN", "clip_dun18_4326")
dun19 <- readOGR("D:/PhD/Fourth chapter/GIS/SIGPAC_DUN", "clip_dun19_4326")

fal17 <- dun17[which(dun17$Grup == "GUARET"), ]
fal18 <- dun18[which(dun18$Grup == "GUARET"), ]
fal19 <- dun19[which(dun19$Grup == "GUARET"), ]

# Remove parts that overlap with management (to get the not managed fallows)
e17 <- erase(fal17,gest17)
e18 <- erase(fal18,gest18) 
e19 <- erase(fal19,gest19) 

# Create column that designs management 
e17$manag <- "NO"
e18$manag <- "NO"
e19$manag <- "NO"

gest17$manag <- "SI"
gest18$manag <- "SI"
gest19$manag <- "SI"

# Join not managed (e17,e18,e19) con managed (gest)

# Prepare

e17_simple <- e17
e18_simple <- e18
e19_simple <- e19

e17_simple@data <- e17_simple@data[,c(1,12)]
e18_simple@data <- e18_simple@data[,c(1,10)]
e19_simple@data <- e19_simple@data[,c(1,12)]

gest17_simple <- gest17
gest18_simple <- gest18
gest19_simple <- gest19


gest17_simple@data <- gest17_simple@data[,c(1,24)]
gest18_simple@data <- gest18_simple@data[,c(1,53)]
gest19_simple@data <- gest19_simple@data[,c(1,16)]

colnames(gest17_simple@data)[1] <- "Campanya"
colnames(gest18_simple@data)[1] <- "Campanya"
colnames(gest19_simple@data)[1] <- "Campanya"

# Save and join in arcgis (capa barbecho17_4326)
writeOGR(gest17_simple, dsn = "D:/PhD/Fourth chapter/GIS/Capas_Rocío", layer = "gest17_4326", driver = "ESRI Shapefile")
writeOGR(gest18_simple, dsn = "D:/PhD/Fourth chapter/GIS/Capas_Rocío", layer = "gest18_4326", driver = "ESRI Shapefile")
writeOGR(gest19_simple, dsn = "D:/PhD/Fourth chapter/GIS/Capas_Rocío", layer = "gest19_4326", driver = "ESRI Shapefile")

writeOGR(e17_simple, dsn = "D:/PhD/Fourth chapter/GIS/Capas_Rocío", layer = "nogest17_4326", driver = "ESRI Shapefile")
writeOGR(e18_simple, dsn = "D:/PhD/Fourth chapter/GIS/Capas_Rocío", layer = "nogest18_4326", driver = "ESRI Shapefile")
writeOGR(e19_simple, dsn = "D:/PhD/Fourth chapter/GIS/Capas_Rocío", layer = "nogest19_4326", driver = "ESRI Shapefile")





