

rm(list=ls())

library(rgdal)

setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/Data")
d <- read.csv("DataDS_ch3_20_18_final.csv") # Load all observations all species
transects_obs <- unique(d$transectID)

setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Second chapter/Data/GIS")
t <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Second chapter/Data/GIS", layer = "Trans_2018_EPSG23031") # Load GIS layer
transects_obs_layer <- t[which(t@data$Codi %in% transects_obs), ]

writeOGR(transects_obs_layer, dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS",layer = "Trans_2010_2018_ch3_EPSG23031", driver ="ESRI Shapefile")

trans <- transects_obs_layer@data
