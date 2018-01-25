
library(rgdal)
library(sp)
library(raster)
library(rgeos)


#1. Clip DUN layers with extent of study area

study_area<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/Fincas", "Study_area")
setwd("C:/Users/ana.sanz/Documents/GIS Ana/DUN")


#2015
dun2015<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/DUN", "DUN_2015")

o15<-over(dun2015,as(study_area,"SpatialPolygons"),returnList = FALSE) #Fast way to clip. over returns a list with NAs when it doesnt 
                                                                        #overlap that can be used to subset the data afterwards
dun2015<-dun2015[!is.na(o15),]

#writeOGR(dun2015,"C:/Users/ana.sanz/Documents/GIS Ana/DUN",layer = "clip_dun2015",driver="ESRI Shapefile")

#2016
dun2016<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/DUN", "DUN_2016")
dun2016<-spTransform(dun2016,CRS("+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs"))

o16<-over(dun2016,as(study_area,"SpatialPolygons"),returnList = FALSE)
dun2016<-dun2016[!is.na(o16),]

#writeOGR(dun2016,"C:/Users/ana.sanz/Documents/GIS Ana/DUN",layer = "clip_dun2016",driver="ESRI Shapefile")

#2017
dun2017<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/DUN", "DUN_2017")

o17<-over(dun2017,as(study_area,"SpatialPolygons"),returnList = FALSE)
dun2017<-dun2017[!is.na(o17),]

#writeOGR(dun2017,"C:/Users/ana.sanz/Documents/GIS Ana/DUN",layer = "clip_dun2017",driver="ESRI Shapefile")
