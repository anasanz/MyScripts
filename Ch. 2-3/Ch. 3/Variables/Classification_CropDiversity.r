
rm(list=ls())

library(rgdal)
library(rgeos)
library(raster)
library(dplyr)

#### Create buffers DUN to see how to classify crops for crop diversification ####

# Buffers created in Arcgis and cropped with each of the dun layers

setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Buffers_cropdiver_classification")

dun15 <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Buffers_cropdiver_classification", layer = "dun15_crop") 
dun16 <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Buffers_cropdiver_classification", layer = "dun16_crop") 
dun17 <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Buffers_cropdiver_classification", layer = "dun17_crop") 
dun18 <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Buffers_cropdiver_classification", layer = "dun18_crop") 
dun19 <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Buffers_cropdiver_classification", layer = "dun19_crop") 

# Ordenar cultivos en tabla
head(dun15@data)

cul15 <- as.data.frame(unique(dun15@data$Cultiu))
colnames(cul15)[1] <- "Cultiu"

cul15_cult_grup <- dun15@data[,c(6,7)]
cul15_cult_grup <- cul15_cult_grup[-which(duplicated(cul15_cult_grup)), ]

cul15 <- inner_join(cul15, cul15_cult_grup, by = "Cultiu")

cul15$year <- "2015"


cul16 <- as.data.frame(unique(dun16@data$Cultiu))
colnames(cul16)[1] <- "Cultiu"

cul16_cult_grup <- dun16@data[,c(6,7)]
cul16_cult_grup <- cul16_cult_grup[-which(duplicated(cul16_cult_grup)), ]

cul16 <- inner_join(cul16, cul16_cult_grup, by = "Cultiu")

cul16$year <- "2016"


cul17 <- as.data.frame(unique(dun17@data$Cultiu))
colnames(cul17)[1] <- "Cultiu"

cul17_cult_grup <- dun17@data[,c(6,7)]
cul17_cult_grup <- cul17_cult_grup[-which(duplicated(cul17_cult_grup)), ]

cul17 <- inner_join(cul17, cul17_cult_grup, by = "Cultiu")

cul17$year <- "2017"

cul18 <- as.data.frame(unique(dun18@data$Cultiu))
colnames(cul18)[1] <- "Cultiu"

cul18_cult_grup <- dun18@data[,c(6,7)]
cul18_cult_grup <- cul18_cult_grup[-which(duplicated(cul18_cult_grup)), ]

cul18 <- inner_join(cul18, cul18_cult_grup, by = "Cultiu")

cul18$year <- "2018"

cul19 <- as.data.frame(unique(dun19@data$Cultiu))
colnames(cul19)[1] <- "Cultiu"

cul19_cult_grup <- dun19@data[,c(6,7)]
cul19_cult_grup <- cul19_cult_grup[-which(duplicated(cul19_cult_grup)), ]

cul19 <- inner_join(cul19, cul19_cult_grup, by = "Cultiu")

cul19$year <- "2019"

c <- full_join(cul15,cul16, by = "Cultiu", "Grup")
c2 <- full_join(c,cul17, by = "Cultiu", "Grup")
c3 <- full_join(c2,cul18, by = "Cultiu", "Grup")
c4 <- full_join(c3,cul19, by = "Cultiu", "Grup")


setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/Data")
write.csv(c4, "clasificacion_cultivos_div.csv")
