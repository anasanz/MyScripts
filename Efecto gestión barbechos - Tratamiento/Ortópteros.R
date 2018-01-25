

#GESTIÓN BARBECHOS: TENDENCIAS ORTÓPTEROS


#En 2014 no se cogieron datos de ortópteros, por lo que sólo se puede mirar 2015 y 2016. Elegir sólo
#las fincas muestreadas en 2015 y 2016

rm(list=ls())
setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")

dat<-read.csv("Datos corregidos.csv",sep = ";", header=TRUE,fill = TRUE)

#Selección de fincas que se han muestreado en 2015 y 2016
dat<-dat[-which(dat$Any == 2014),]

ID <- unique(dat$Codi_Finca)# Object with the IDs (462)
dat$Codi_Finca <- as.character(dat$Codi_Finca)
ls <- 0

for( i in 1:length(ID)){
  
  tmp <- dat[dat$Codi_Finca==ID[i], ] #Subset of the rows with the same ID
  if((sum (tmp$Contatge %in% 99) == 0) == TRUE){  #True is added as a 1
    ls[[i]] <- ID[i]
  }
}

ls<-ls[!is.na(ls)]
ls<-ls[-c(1)] #Lista con fincas muestreadas los 3 años
ls<-as.data.frame(ls)

dat2<-filter(dat, dat$Codi_Finca %in% ls$ls) #Filtro en los datos iniciales para tener la especie

write.csv(dat2,file="Fincas 2015-2016.csv")

###

rm(list=ls())
setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")

fin<-read.csv("Fincas 2015-2016.csv",sep = ",",
              header=TRUE,fill = TRUE)

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")
veg<-read.csv("EvolucioOcells_20170503.csv",sep = ";",
              header=TRUE,fill = TRUE)

#Sólo datos de fincas muestreadas en 2015 y 2016

veg<-veg[-which(veg$Any == 2014),]
vfin<-veg[which(veg$Codi_Finca %in% fin$Codi_Finca), ]

vfin<-vfin[vfin$especieObjectiu == "BUOED", ]
vrec<-vfin[ ,c(1,2,3,5,8,9,10)]

#Sustituir los 99 por NA values
vrec$OrtopterPetits[vrec$OrtopterPetits == 99] <- NA
vrec$OrtopterMitjans[vrec$OrtopterMitjans == 99] <- NA
vrec$OrtoptersGrans[vrec$OrtoptersGrans == 99] <- NA

#Evolución de ortópteros por sector y secano

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



#POR SECTOR

boxplot(OrtopterPetits ~ Sector1, vrec, main = "Ortópteros pequeños")
boxplot(OrtopterMitjans ~ Sector1, vrec, main = "Ortópteros medianos")
boxplot(OrtoptersGrans ~ Sector1, vrec, main = "Ortópteros grandes")

#POR SECANO
boxplot(OrtopterPetits ~ Secano, vrec, main = "Ortópteros pequeños")
boxplot(OrtopterMitjans ~ Secano, vrec, main = "Ortópteros medianos")
boxplot(OrtoptersGrans ~ Secano, vrec, main = "Ortópteros grandes")

  #Tendencia temporal por secano
voc <- vrec[vrec$Secano == "OCCIDENTAL", ]

par(mfrow = c(1,3),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,0) + 0.1)

boxplot(OrtopterPetits ~ Any, voc, main = "Ortópteros pequeños")
boxplot(OrtopterMitjans ~ Any, voc, main = "Ortópteros medianos")
boxplot(OrtoptersGrans ~ Any, voc, main = "Ortópteros grandes")
mtext("SECANOS OCCIDENTALES: Tendencia temporal de la ortópteros",side=3,line=1,outer = TRUE, cex = 1.1)


vor <- vrec[vrec$Secano == "ORIENTAL", ]
par(mfrow = c(1,3),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,0) + 0.1)

boxplot(OrtopterPetits ~ Any, vor, main = "Ortópteros pequeños")
boxplot(OrtopterMitjans ~ Any, vor, main = "Ortópteros medianos")
boxplot(OrtoptersGrans ~ Any, vor, main = "Ortópteros grandes")
mtext("SECANOS ORIENTALES: Tendencia temporal de la ortópteros",side=3,line=1,outer = TRUE, cex = 1.1)





#SEPARAR BARBECHO Y ALFALFA PARA LOS SECANOS ORIENTALES
setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")
fin<-read.csv("Finca.csv",sep = ";",
              header=TRUE,fill = TRUE)

#Selecciono las filas que contengan los Codi_finca de fincas muestreadas EN 2015 Y 2016
fin2<- fin[which(fin$Codi_Finca %in% vrec$Codi_Finca), ]
fin2<-fin2[,c(2,5)]
#Uno el ID a las fincas(2 años) para asociarlo después con el tratamiento y uso
library(dplyr)
fid<-left_join(vrec,fin2,by = "Codi_Finca")

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
aus<-left_join(gus,a,by = c("IDfinca" = "ID"))
aus<-aus[,-c(13,15)]

aus$usos2 <- aus[, "usos"]
aus[is.na(aus[, "usos2"]), "usos2"] <- aus[is.na(aus[, "usos2"]), "usa"]

aus<-aus[,-c(10,11,12,13)]
aus$US<-aus$usos2
aus<-aus[,-c(10)]

#En los secanos orientales, separar la evolución de la vegetación en Barbecho/Alfalfa
#TENDENCIA TEMPORAL BARBECHO
sor <- aus[aus$Secano == "ORIENTAL", ]
barb <- sor[sor$US == "4",]
barb <- barb[,-c(10)]

barb <- barb[complete.cases(barb),]# Al faltar los de belianes 2016 se eliminan muchas observaciones

par(mfrow = c(1,3),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,0) + 0.1)
boxplot(OrtopterPetits ~ Any.x, barb, main = "Ortópteros pequeños")
boxplot(OrtopterMitjans ~ Any.x, barb, main = "Ortópteros medianos")
boxplot(OrtoptersGrans ~ Any.x, barb, main = "Ortópteros grandes")
mtext("SECANOS ORIENTALES BARBECHO: Tendencia temporal de ortópteros",side=3,line=1,outer = TRUE, cex = 1.1)


#TENDENCIA TEMPORAL ALFALFA
sor <- aus[aus$Secano == "ORIENTAL", ]
alf <- sor[sor$US == "2",]
alf <- alf[,-c(10)]
alf <- alf[complete.cases(alf),] #Faltan Belianes!

par(mfrow = c(1,3),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,0) + 0.1)
boxplot(OrtopterPetits ~ Any.x, alf, main = "Ortópteros pequeños")
boxplot(OrtopterMitjans ~ Any.x, alf, main = "Ortópteros medianos") 
boxplot(OrtoptersGrans ~ Any.x, alf, main = "Ortópteros grandes")

mtext("Año",side=1,line=1,outer = TRUE,cex = 0.9)
mtext(" ",side=2,line=1,outer = TRUE, cex = 0.9)
mtext ("ALFALFA ORIENTAL: Tendencia temporal de ortópteros",side=3,line=1,outer = TRUE, cex = 1.1)

