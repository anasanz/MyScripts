

#Check distribution of clusters

library(dplyr)
library(rgdal)
library(raster)

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")
fl<-read.csv("Cluster_tipo_barbecho.csv",sep = ",",header=TRUE,fill = TRUE,na.strings="")

fl15<-fl[which(fl$Any == "2015"),c(3,4,5)] #Keep only the cluster


#Join to the attribute table 
fin15<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/Fincas", "FINQUES_2015_SELECCION_BUENA")
f<-merge(fin15,fl15,by = "Codi_Finca")

setwd("C:/Users/ana.sanz/Documents/GIS Ana/Fincas")
writeOGR(f,"f","fin15_clus", driver="ESRI Shapefile") 


