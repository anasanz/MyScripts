
rm(list = ls())


library(vegan)
setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")
fl<-read.csv("Cobertura_media_flora.csv",sep = ",",header=TRUE,fill = TRUE,na.strings="")
fl<-fl[,c(1,215,216,2:214)]

sim<-as.data.frame(diversity(fl[,4:216],index = "simpson"))

fl<-fl[,c(2,3)]
fl$Simpson<-sim$`diversity(fl[, 4:216], index = "simpson")`

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")
#write.csv(fl,file = "Simpson_plant_diversity.csv")
