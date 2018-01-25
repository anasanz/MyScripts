
#CREAR VARIABLE HETEROGENEIDAD COB_ALT

#Utilizar el indice de hetrogeneidad basado en la heterogeneidad de las parcelas

rm(list=ls())

library(dplyr)
library(tidyr)
library(FactoMineR)
library(factoextra)

#1. Heterogeneity index: LEVIN'S INDEX
    #Calculates niche breath of species (Species = Finca; Niche breath = Heterogeneity)

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")
ab<-read.csv("Birddata_abundance.csv",sep = ";",
             header=TRUE,fill = TRUE)
ab<-ab[-which(ab$Contatge == 99),]
wid<-spread(ab,EspecieObj,Contatge) 
wid$CF_A<-paste(wid$Codi_Finca,wid$Any,sep="_")

#Eliminar variables 6b,7b,8b,9b de las variables cob_altura asignandoselo a la categoría 6,7,8,9

for (i in 1:nrow(wid)){
  if (wid$Cobert_Alt4b[i] > 0){
    wid$Cobert_Alt4[i]<-wid$Cobert_Alt4b[i]
  } 
  if (wid$Cobert_Alt5b[i] > 0){
    wid$Cobert_Alt5[i]<-wid$Cobert_Alt5b[i]
  } 
  if (wid$Cobert_Alt6b[i] > 0){
    wid$Cobert_Alt6[i]<-wid$Cobert_Alt6b[i]
  } 
  if (wid$Cobert_Alt7b[i] > 0){
    wid$Cobert_Alt7[i]<-wid$Cobert_Alt7b[i]
  } 
  if (wid$Cobert_Alt8b[i] > 0){
    wid$Cobert_Alt8[i]<-wid$Cobert_Alt8b[i]
  } 
  if (wid$Cobert_Alt9b[i] > 0){
    wid$Cobert_Alt9[i]<-wid$Cobert_Alt9b[i]
  } 
}

wide<-wid[ ,-c(1:11,16,18,20,22,24,26,28:45)]
wide<-wide[ ,-c(11)]
wide$ind<-0


levin<-function(species, stand=TRUE){
  
  x <- 1/(sum((species/100)^2)) 
  
  if(stand==TRUE){
     
    x<-(x-1)/(length(which(species >0))-1)
  }
  return(x)
}


for (i in 1:nrow(wide)) {
  
  lev<-levin(species = wide[i,],stand = TRUE)
  wide$ind[[i]] <-lev
}

wid$lev_ind<-wide$ind
wid<-wid[ ,-c(6:11,16,18,20,22,24,26,28:46)]
wid$lev_ind[wid$lev_ind == -Inf]<-0
wid$lev_ind[wid$lev_ind == Inf]<-0
wid$lev_ind[is.na(wid$lev_ind)]<-0
setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")

#Los Na aparecen porque el índice es 0 al haber sólo una clase del 100% (Luego la herogeneidad es muy 
#baja, los Na podrian ser considerados 0). El índice sale 1 y cuando lo estandarizas es 0/0 = NA
# Los infinitos son porque todos los valores son 0 y 1/0 es infinito (?????)
#AF115B tiene 99 en todos. En access solo aparece un 100 en Cob_alt9..la quito o la dejo?Comprobar datos de pajaros

# Hay filas que NO SUMAN 100, por lo que el levin index sale mayor que uno
which(wid$lev_ind > 1) #12 observaciones ... merece la pena añadir esta variable?
wid$lev_ind[which(wid$lev_ind > 1)] <- NA

#write.csv(wid,file = "Fallow_heterogeneity.csv") 


sp1<-wide[1,]
sp2<-wide[2,]

levin(species = sp2,stand = TRUE)


