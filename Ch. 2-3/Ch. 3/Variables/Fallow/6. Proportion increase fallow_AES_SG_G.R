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

# ==================================================================================
#               Calculate total HA of FALLOW SG (this is for fourth chapter)
# ==================================================================================
sg14 <- readOGR("D:/PhD/First chapter/Datos/Datos barbechos arrendados/GIS layers", layer = "SG_2014_EPSG23031")
sg14 <- sg14[which(sg14$estat2 %in% c("Guaret", "Alfals")), ]
sg15 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/SG", layer = "SG15_23031")
sg16 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/SG", layer = "SG16_23031")
sg17 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/SG", layer = "SG17_23031")
sg18 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/SG", layer = "SG18_23031")
sg19 <- readOGR("D:/PhD/Third chapter/GIS/Fallow/SG", layer = "SG19_23031")

area_m2 <- sum(area(sg14)) + sum(area(sg15)) + sum(area(sg16)) + sum(area(sg17)) + sum(area(sg18)) + sum(area(sg19))
area_m2/10000

# For thesis, also 2020

sg20 <- readOGR("D:/PhD/GIS Ana_14_Mayo/Finques 2020", layer = "Finques_ASG_2020")
crs(sg20)
unique(sg20@data$General_)
sg20 <- sg20[which(sg20$General_ %in% c("Guaret", "Alfals", "Guaret pasturable", "Guaret ADJAC")), ]

x <- sg20@data
by_year <- c(sum(area(sg14)), sum(area(sg15)), sum(area(sg16)), sum(area(sg17)), sum(area(sg18)), sum(area(sg19)), sum(area(sg20)))
by_year/10000

area_m2 <- sum(area(sg14)) + sum(area(sg15)) + sum(area(sg16)) + sum(area(sg17)) + sum(area(sg18)) + sum(area(sg19) + sum(area(sg20)))
area_m2/10000

