

#CLEAN DATA TREATMENTS

rm(list=ls())

#1. Fincas muestreadas 1,2 o 3 años unidas a USOS

mues<-read.csv("Muestreo_fincas.csv",sep = ";",
               header=TRUE,fill = TRUE)

usos<-read.csv("Finca_usos_456.csv",sep = ";",
                     header=TRUE,fill = TRUE)
colnames(usos)[4]<-"Any"

library(dplyr)

fin_us<-left_join(mues,usos,by = c("ID","Any")) #Tiene más observaciones. Ver las que se han añadido.

which(duplicated(fin_us)) #Hay observaciones duplicadas. Eliminar

fin_us<-fin_us[-which(duplicated(fin_us)),]

fin_us_sub<-fin_us[,-c(7:12)]
which(duplicated(fin_us_sub)) #Hay observaciones con dos usos. Quedarse con los que proceden de la fuente CTFC







