
#CHECK FALLOW AGE

library(rgdal)
library(rgeos)
library(dplyr)

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")

#### Campos camp11, 12 etc. -> 1 es que se ha arrendado y gestionado ese aÃ±o (barbecho) 

#2015#

f15<- readOGR("C:/Users/ana.sanz/Documents/GIS Ana/Fincas", "FINQUES ASG 2015")
f15<-as.data.frame(f15@data)
f15<-f15[,c(3,11:14)]

# Quitar 1 si el aÃ±o posterior es un 0 y no se ha gestionado
# Goal: Check the number of CONSECUTIVE years a field has been fallow

for (i in 1:493){
  if(f15$Camp_2012[i] == 0){
    f15$Camp_2011[i] <- 0} 
    else {f15$Camp_2011[i]}
  if(f15$Camp_2013 [i] == 0){
    f15$Camp_2012 [i] <- 0} 
    else {f15$Camp_2011[i]}
}

x<-as.matrix(f15[,c(2:5)])
f15$Age_1415<-rowSums(x)

#2016#

f16<- readOGR("C:/Users/ana.sanz/Documents/GIS Ana/Fincas", "Finques ASG 2016")
f16<-as.data.frame(f16@data)
f16$Camp_2015 <- 1
f16<-f16[,c(1,9)]

f<-full_join(f15,f16)

f$Camp_2015[which(is.na(f$Camp_2015))]<-0
f$Age_1415[which(is.na(f$Age_1415))]<-0
x<-as.matrix(f[,c(6,7)])
f$Age_1516<-rowSums(x)

#Make 0 the age of the field that is fallow if it hasn't been fallow one year (to make it consecutive)

for (i in 1:792){
  if(f$Camp_2015[i] == 0){
    f$Age_1516[i] <- 0} 
  else {f$Camp_2015[i]}
}

write.csv(f, file = "Fallow_age.csv")


#Check data from ASG to see if it is the same. A partir de los datos de VARIABLES

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")
v<-read.csv(file = "Variables.csv", header = TRUE, sep = ",")
v<-v[which(v$EspecieObj == "ALRUF"),]
v<-v[,c(2,4:6,23)]

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")
as<-read.csv(file = "Edad_barbecho_ASG.csv", header = TRUE, sep = ";")
as<-as[,c(1:3,10:11)]
colnames(as)[4]<-"Any"
colnames(as)[2]<-"IDfinca"

library(dplyr)
x<-left_join(as, v, by = c("IDfinca", "Codi_Finca", "Any"))
x<-x[-which(duplicated(x)),]
unique(x$Tipus)
#Delete non relevant "Tipus"
x<-x[-which(x$Tipus %in% c("Arbrat","Olivera amb sòl guaret",
                        "Vegetació natural (Bosc)","Civada (100% concentracio normal productiu)",
                        "Ray-grass (100% concentracio normal productiu)","Cereal",
                        "85 Kg/ha veça + 21 Kg/ha civada","75 kg/ha Veça + 15 kg/ha Civada (75% de la conc. Normal)",
                        "120 kg/ha Veça + 25 kg/ha Civada (120% de la conc. Normal)","Ordi 180 Kg/ha (100% de la concentració normal)",
                        "Ordi 130 Kg/ha (75% de la concentració normal)","100 kg/ha Veça + 20 kg/ha Civada (100% de la conc. Normal)")), ]

g<-x[which(x$Tipus %in% c("Arbrat","Olivera amb sòl guaret",
                     "Vegetació natural (Bosc)","Civada (100% concentracio normal productiu)",
                     "Ray-grass (100% concentracio normal productiu)","Cereal",
                     "85 Kg/ha veça + 21 Kg/ha civada","75 kg/ha Veça + 15 kg/ha Civada (75% de la conc. Normal)",
                     "120 kg/ha Veça + 25 kg/ha Civada (120% de la conc. Normal)","Ordi 180 Kg/ha (100% de la concentració normal)",
                     "Ordi 130 Kg/ha (75% de la concentració normal)","100 kg/ha Veça + 20 kg/ha Civada (100% de la conc. Normal)")), ]


#????Hay tipo "arboles" en fincas arrendadas, Ejemplo 516

#Recalculate the age without those fincas
x$age2<-NA
ID <- unique(x$IDfinca)

for (i in 1:length(ID)){
  
  tmp <- nrow(x[which(x$IDfinca == ID[i]),])
  x$age2[which(x$IDfinca == ID[i])]<- tmp
}

x <- x[,c(6,8)]
age <- left_join(v, x, by = "CF_A")



i=6

