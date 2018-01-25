


#UNIÓN DE TODAS LAS FINCAS (MUESTREADAS 1, 2 Y 3 AÑOS A LOS TRATAMIENTOS)

rm(list=ls())

library(dplyr)

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")
vfin<-read.csv("EvolucioOcells_consulta.csv",sep = ";",
              header=TRUE,fill = TRUE)

vfin<-vfin[vfin$especieObjectiu == "BUOED", ]
vrec<-vfin[ ,c(2,3,5,11,12,13)]
vrec$Recob_plotViu<-sub(",",".", vrec$Recob_plotViu)
vrec$Recob_plotMort<-sub(",",".", vrec$Recob_plotMort)
vrec$PromigAltura1Plot <-sub(",",".", vrec$PromigAltura1Plot)

vrec$Recob_plotViu<-as.numeric(vrec$Recob_plotViu)
vrec$Recob_plotMort<-as.numeric(vrec$Recob_plotMort)
vrec$PromigAltura1Plot <-as.numeric(vrec$PromigAltura1Plot)

#Sustituir 99 por NA
vrec[vrec == 99] <- NA
vrec$Sector1<-vrec$Sector
vrec$Sector1[vrec$Sector1 == 4] <- "AF"
vrec$Sector1[vrec$Sector1 == 2] <- "BE"
vrec$Sector1[vrec$Sector1 == 3] <- "BM"
vrec$Sector1[vrec$Sector1 == 5] <- "GR"
vrec$Sector1[vrec$Sector1 == 1] <- "SI"
vrec$Sector1[vrec$Sector1 == 10] <- "UT"

vrec$Secano<-vrec$Sector
vrec$Secano[vrec$Sector == 4] <- "OCCIDENTAL"
vrec$Secano[vrec$Sector == 2] <- "ORIENTAL"
vrec$Secano[vrec$Sector == 3] <- "ORIENTAL"
vrec$Secano[vrec$Sector == 5] <- "OCCIDENTAL"
vrec$Secano[vrec$Sector == 1] <- "ORIENTAL"
vrec$Secano[vrec$Sector == 10] <- "OCCIDENTAL"


#USO 
##Separar Barbechos de Alfalfa para los secanos orientales (Sisón)

#1. Unir al ID finca
setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")
fin3<-read.csv("Finca.csv",sep = ";",
              header=TRUE,fill = TRUE)

#Llamo a esto fin3, pero son todas las fincas (muestreadas 1, 2 o 3 años)
fin3<-fin3[,c(2,5)]
#Uno el ID a las fincas para asociarlo después con el tratamiento y uso
library(dplyr)
fid<-left_join(vrec,fin3,by = "Codi_Finca")

#Unir fincas a usos (FUS)
us<-read.csv("Finca_usos.csv",sep = ";",
             header=TRUE,fill = TRUE)
us$Any<-us$id_campanya
us<-us[,c(1,5,8)]

usid<-us[which(us$ID %in% fid$ID),]
usid<-arrange(usid,ID)
usid$US[usid$US == 63] <- 4
usid$US[usid$US == 87] <- 4
usid$US[usid$US == 3] <- 2

usid<-usid[!duplicated(usid),]
usid$US<-as.factor(usid$US)

fus<-left_join(fid,usid,by=NULL) #En 2016 no hay datos de usos

#Faltan usos del 2016. Unirlos
e<-read.csv("Finques_2016_usos.csv",sep = ";",
            header=TRUE,fill = TRUE)
e<-e[,c(3,21)]
#Columna con usos: 4 (Barbecho) y 2 (Alfalfa)
e$US<-e$Us

#Barbecho
g<-e[grep("Guaret",e$Us),]
g$US <- 4
g$Any <- 2016

f<-g[fus$ID %in% g$ID,]

gus<-left_join(fus,g,by = c("ID" = "ID"))
gus<-gus[,-c(11,13)]

gus$usos <- gus[, "US.x"]
gus[is.na(gus[, "usos"]), "usos"] <- gus[is.na(gus[, "usos"]), "US.y"]

#Alfalfa
a<-e[grep("Alfals",e$Us),]
a$US <- 2
a$usa<-a$US
a<-a[,-c(3)]
a$Any <- 2016
aus<-left_join(gus,a,by = c("ID" = "ID"))
aus<-aus[,-c(13,15)]

aus$usos2 <- aus[, "usos"]
aus[is.na(aus[, "usos2"]), "usos2"] <- aus[is.na(aus[, "usos2"]), "usa"]

aus<-aus[,-c(10,11,12,13)]
aus$US<-aus$usos2
aus<-aus[,-c(10)]



#Importar tratamientos de primavera

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")
tra<-read.csv("Consulta_tratamientos.csv",sep = ";",
              header=TRUE,fill = TRUE)
tra<-arrange(con,Codi_Finca)
#Éstos (tra) son los tratamientos en primavera de fincas muestreadas 1,2, o 3 años

#Las 18 fincas restantes no tienen tratamientos, o tienen en invierno o otoño-> USARLAS COMO CONTROL

#Unir tratamientos primavera
tra$Temporada<-as.character(tra$Temporada)
tra$Temporada[tra$Temporada == "2013-2014"]<-"2014"
tra$Temporada[tra$Temporada == "2014-2015"]<-"2015"
tra$Temporada[tra$Temporada == "2015-2016"]<-"2016"
names(tra)[names(tra) == 'Temporada'] <- 'Any'
names(aus)[names(aus) == "Any.x"]<-"Any"
aus$Any<-as.character(aus$Any)
vtra<-left_join(aus,tra,by = c("Codi_Finca","Any"))

#FINCAS CON DOBLE OBSERVACIÓN TRATAMIENTOS!
#Estas fincas ya están arregladas para las fincas muestreadas los 3 años. Importar esas y 
#sólo arreglar las sobrantes

fin3<-read.csv("Fincas-tratamientos.csv",sep = ",",
               header=TRUE,fill = TRUE)

v3<-vtra[-which(vtra$Codi_Finca %in% fin3$Codi_Finca),]
rownames(v3) <- seq(length=nrow(v3))
which(duplicated(v3[,2:3])) #Duplicados que hay que mirar (146)


v3<-v3[-c(153,691,693,695,712,716,750,757,770,774,778,834,862,971,1043,1051,
              1120,1198,1201,1203,1216,1220,1246,1249,1253,1255,1258,1260,1266,1270,
              1272,1332,1435,1471,1487,1572),]

v3$Tractament[v3$Codi_Finca == "BM11A" & v3$Any == "2015"]<- 20
v3$Tractament[v3$Codi_Finca == "GR17B" & v3$Any == "2015"]<- 20

v3$Tractament[v3$Codi_Finca == "GR1A" & v3$Any == "2014"]<- 20
v3$Tractament[v3$Codi_Finca == "GR1A" & v3$Any == "2015"]<- 20

v3$Tractament[v3$Codi_Finca == "GR22A" & v3$Any == "2015"]<- 20

v3$Tractament[v3$Codi_Finca == "GR23A" & v3$Any == "2015"]<- 20

v3$Tractament[v3$Codi_Finca == "GR3B" & v3$Any == "2015"]<- 20

v3$Tractament[v3$Codi_Finca == "GR4A" & v3$Any == "2014"]<- 20

v3$Tractament[v3$Codi_Finca == "GR6A" & v3$Any == "2014"]<- 20
v3$Tractament[v3$Codi_Finca == "GR6A" & v3$Any == "2015"]<- 20

v3$Tractament[v3$Codi_Finca == "GR6B" & v3$Any == "2014"]<- 20
v3$Tractament[v3$Codi_Finca == "GR6B" & v3$Any == "2015"]<- 20

v3$Tractament[v3$Codi_Finca == "GR7B" & v3$Any == "2014"]<- 20

v3$Tractament[v3$Codi_Finca == "GR7C" & v3$Any == "2014"]<- 20
v3$Tractament[v3$Codi_Finca == "GR7C" & v3$Any == "2015"]<- 20

 #110 left

#Eliminar las fincas que contengan herbicidar y labrar, conservando curronar (último trat)
#Primero hacer un subset de estas fincas
rownames(v3) <- seq(length=nrow(v3))
which(duplicated(v3[,2:3]))
v3<-v3[-c(167,178,227,234,238,242,273,310,368,372,400,552,556,560,564,567,569,
          596,598,601,603,606,608,611,613,622,624,627,629,632,634,664,666,669,
          671,674,676,685,687,692,694,696,698,700),]

rownames(v3) <- seq(length=nrow(v3))
which(duplicated(v3[,2:3]))

##En duplicados que son Herbicidar + Alfalfa, quedarse con Alfalfa
#(el primer año de alfalfa la herbicidan para que compita mejor)
v3<-v3[-which(v3$US == 2 & v3$Tractament == 5),]
rownames(v3) <- seq(length=nrow(v3))
which(duplicated(v3[,2:3]))

v3<-v3[-c(653,655,767,769,771),]
rownames(v3) <- seq(length=nrow(v3))

#Eliminar parcelas problemas BROLLA
v3<-v3[-which(v3$Codi_Finca == "BE44A"),]
v3<-v3[-which(v3$Codi_Finca == "BE45A"),]
v3<-v3[-which(v3$Codi_Finca == "BE49A"),]
v3<-v3[-which(v3$Codi_Finca == "BE83A"),]
v3<-v3[-which(v3$Codi_Finca == "SI96A"),]
v3<-v3[-which(v3$Codi_Finca == "SI99A"),]

rownames(v3) <- seq(length=nrow(v3))
which(duplicated(v3[,2:3]))

v3<-v3[-c(816,819,821,823,832,834,888,890,896,898,925,927,
          942,944,961,968,1028,1036,1037,1038,1040,1041,1042,
          1044,1045,1046,1128,1130,1131,1132,1133,1135,1136:1139,
          1140,1142,1143,1145,1147,1150,1216,1273,1572),]

v3<-v3[-which(v3$Codi_Finca == "SI116A"),]

rownames(v3) <- seq(length=nrow(v3))
which(duplicated(v3[,2:3]))

v3<-v3[-c(817),]

vtra<-v3

#v3 contiene todos los usos y tratamientos en primavera de parcelas muestreadas 1,2 o 3 años.
#De momento en las parcelas de alfalfa con algún tratamiento (e.g. labrar) se consideran como
#tratamiento "alfalfa"
#Añadir tratamiento "alfalfa" y tratamiento "control" donde no hay tratamiento

vtra$Tractament[vtra$US == "2"]<-"11" #Tratamiento "alfalfa"
vtra<-vtra[,-c(1,2,12,13,14)]
colnames(vtra)[colnames(vtra) == "ID.x"]<-"ID"

#Añadir información sobre qué año se ha muestreado cada finca (unir)

library(tidyr)

fin<-read.csv("most_fin.csv",sep = ";",
               header=TRUE,fill = TRUE) #Convert to long to join it by year (change names of the variables)

colnames(fin)[colnames(fin) == "mostreig2014"]<-"2014"
colnames(fin)[colnames(fin) == "mostreig2015"]<-"2015"
colnames(fin)[colnames(fin) == "mostreig2016"]<-"2016"

most<-gather(fin,Any,Most,3:5,factor_key = TRUE)
most<-arrange(most,Codi_Finca,ID)

#Seleccionar los ID de fin que tienen tratamientos alguno de los años

most<-most[which(most$ID %in% vtra$ID),]
rownames(most) <- seq(length=nrow(most))
most<-most[-c(1:45),]
rownames(most) <- seq(length=nrow(most))

#Unir tratamientos a información de muestreo de fincas
vtra$Any<-as.factor(vtra$Any)
vfin<-left_join(vtra,most)




#Añadir tratamiento 21 "Control" para los que no tienen tratamiento en barbecho (CR) 
levels(vfin$Tractament)<-c(levels(vfin$Tractament),"21")

vfin$Tractament[is.na(vfin$Tractament)]<- "21"

#Añadir picar+herbicidar para 3 fincas que le faltan

vfin$Tractament[vfin$Codi_Finca == "GR24A" & vfin$Any == "2015"]<- 20

vfin$Tractament[vfin$Codi_Finca == "GR2B" & vfin$Any == "2014"]<- 20
vfin$Tractament[vfin$Codi_Finca == "GR2B" & vfin$Any == "2015"]<- 20

vfin$Tractament[vfin$Codi_Finca == "GR3B" & vfin$Any == "2014"]<- 20


#Make the data nice and save file

vtra<-vfin
vtra$Tractament[vtra$Tractament == "21"] <- "Control"
vtra$Tractament[vtra$Tractament == "6"] <- "Llaurar"
vtra$Tractament[vtra$Tractament == "8"] <- "Llaurar" #Considero todos los tipos de labrados igual
vtra$Tractament[vtra$Tractament == "2"] <- "Retirada de soques"
vtra$Tractament[vtra$Tractament == "4"] <- "Triturat de restes vegetals"
vtra$Tractament[vtra$Tractament == "1"] <- "Curronar"
vtra$Tractament[vtra$Tractament == "11"] <- "Alfals"
vtra$Tractament[vtra$Tractament == "3"] <- "Sembra directa civada i veça"
vtra$Tractament[vtra$Tractament == "9"] <- "Pasturar"
vtra$Tractament[vtra$Tractament == "5"] <- "Herbicidar"
vtra$Tractament[vtra$Tractament == "20"] <- "Picar i herbicidar"

which(vtra$Tractament == "Herbicidar") #Sólo hay dos herbicidar sin picar, los unifico en categoría
#picar i herbicidar
vtra$Tractament<-as.factor(vtra$Tractament)
levels(vtra$Tractament) #9 tratamientos

vtra$Tractament<-as.character(vtra$Tractament)

vtra$Tractament[vtra$Tractament == "10"] <- "Picar"
vtra$Tractament[vtra$Tractament == "12"] <- "Llaurar"
vtra$Tractament[vtra$Tractament == "7"] <- "Llaurar"
vtra$Tractament[vtra$Tractament == "14"] <- "Control"
vtra$Tractament[vtra$Tractament == "16"] <- "Retirar arbres arrancats i acopiar arrels"

#Vtra contiene las fincas que no había unido a los tratamientos antes (fincas muestreadas solo 1 o 2 años)
#Unir a fincas muestreadas 3 años (FIN3) para tener todas las fincas unidas a los tratamientos

fin3<-read.csv("Fincas-tratamientos.csv",sep = ",",
               header=TRUE,fill = TRUE) 


#Crear columna de Most VERDADERO porque se han muestreado todos los años

fin3$Most<-"VERDADERO"

#Unir

fin3$Any<-as.factor(fin3$Any)

colnames(fin3)[colnames(fin3) == "ID.x"]<-"ID"

todas<-full_join(vtra,fin3)

#CUIDADO!!!!Éstos datos NO tienen el recubrimiento de fincas muestreadas los 3 años (porq esa info
#no está en Fincas-tratamientos.csv)

#Guardar relación tratamiento-año-finca TODAS FINCAS
todas<-todas[,-c(12)]
write.csv(todas,file = "Fincas-tratamientos TODOS prov.csv")

