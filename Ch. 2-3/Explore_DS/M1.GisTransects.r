library(rgdal)

setwd("S:/PhD/Second chapter/Data")
d <- read.csv("DataDS_ready_ALL.csv") # Load all observations all species
transects_obs <- unique(d$transectID)

setwd("S:/PhD/Second chapter/Data/GIS")
t <- readOGR(dsn = "S:/PhD/Second chapter/Data/GIS", layer = "Trans_2018_EPSG23031") # Load GIS layer
transects_obs_layer <- t[which(t@data$Codi %in% transects_obs), ]

writeOGR(transects_obs_layer, dsn = "S:/PhD/Second chapter/Data/GIS",layer = "Trans_2010_2018_analyzed_EPSG23031", driver ="ESRI Shapefile")

trans <- transects_obs_layer@data
