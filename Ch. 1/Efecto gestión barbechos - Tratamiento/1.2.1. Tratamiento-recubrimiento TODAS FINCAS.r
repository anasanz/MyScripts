

rm(list=ls())

library(dplyr)

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")


c<-read.csv("TODOS_TRATAMIENTOS.csv",sep = ",",
            header=TRUE,fill = TRUE)
ab<-read.csv("EvolucioOcells_abundance.csv",sep = ";",
             header=TRUE,fill = TRUE)
ab<-ab[-which(ab$Contatge == 99),]


#Seleccionar sólo recubrimiento y altura vegetación
v<-ab[ab$EspecieObj == "BUOED", ]
v<-v[,c(1,3,5,11,12,13)]
vtra<-left_join(v,c)
vtra<-vtra[,-c(7:10)]


par(mfrow = c(3,1),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,0) + 0.1)
boxplot(Recob_plotViu ~ Tractament, vtra)
mtext("% Veg viva",side=2,line=3, cex = 0.9)
boxplot(Recob_plotMort ~ Tractament, vtra) 
mtext("% Veg morta",side=2,line=3, cex = 0.9)
boxplot(PromigAltura1Plot ~ Tractament, vtra)
mtext("Mitjana Altura",side=2,line=3, cex = 0.9)

mtext("Tractament",side=1,line=1,outer = TRUE,cex = 0.9)
