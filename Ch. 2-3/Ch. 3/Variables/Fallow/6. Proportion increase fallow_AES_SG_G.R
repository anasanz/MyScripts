# ==================================================================================
#               Calculate proportion of increase FALLOW SG-AES-GREENING
# ==================================================================================

rm(list=ls())

library(rgdal)
library(rgeos)
library(raster)
library(dplyr)

## ---- Load layers from dun with only fallow uses ----

gre15 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "gre_15")
gre16 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "gre_16")
gre17 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "gre_17")
gre18 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "gre_18")
gre19 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/Greening", layer = "gre_19")

gre15$area <- area(gre15)
area_15 <- sum(gre15$area)

gre19$area <- area(gre19)
area_19 <- sum(gre19$area)

# % increase
inc <- area_19-area_15
prop_inrease <- (inc/area_15)*100

