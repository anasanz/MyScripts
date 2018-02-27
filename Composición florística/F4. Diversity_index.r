
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

#ALL(With 2017)

library(vegan)
setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Consultas")
fl<-read.csv("Cobertura_media_flora_FINAL.csv",sep = ",",header=TRUE,fill = TRUE,na.strings="")
fl<-fl[,c(1,232,233,2:231)]

sim<-as.data.frame(diversity(fl[,4:233],index = "simpson"))

fl<-fl[,c(2,3)]
fl$Simpson<-sim$`diversity(fl[, 4:233], index = "simpson")`

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")
#write.csv(fl,file = "Simpson_plant_diversity_FINAL.csv")
