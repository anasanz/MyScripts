

rm(list=ls())

library(dplyr)

###############################Primero, cargar las fincas de 3 años unidas al recubrimiento

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")
tra<-read.csv("Fincas-tratamientos.csv",sep = ",",
              header=TRUE,fill = TRUE)
veg<-read.csv("EvolucioOcells_consulta.csv",sep = ";",
              header=TRUE,fill = TRUE)

vfin<-veg[which(veg$Codi_Finca %in% tra$Codi_Finca), ]

#Escoger variables recubrimiento plots
#Como no voy a ver el efecto de las especies, restringir los datos para 1 observación por año y por parcela

vfin<-vfin[vfin$especieObjectiu == "BUOED", ]
vrec<-vfin[ ,c(3,5,11,12,13)]
vrec$Recob_plotViu<-sub(",",".", vrec$Recob_plotViu)
vrec$Recob_plotMort<-sub(",",".", vrec$Recob_plotMort)
vrec$PromigAltura1Plot <-sub(",",".", vrec$PromigAltura1Plot)

vrec$Recob_plotViu<-as.numeric(vrec$Recob_plotViu)
vrec$Recob_plotMort<-as.numeric(vrec$Recob_plotMort)
vrec$PromigAltura1Plot <-as.numeric(vrec$PromigAltura1Plot)

#Unir recubrimiento a tratamientos
vtra<-left_join(tra,vrec)
vtra$Tractament<-as.character(vtra$Tractament)
levels(vtra$Tractament)

vtra$Tractament[vtra$Tractament == "Picar i herbicidar"] <- "Herb"
vtra$Tractament[vtra$Tractament == "Triturat de restes vegetals"] <- "Trit"
vtra$Tractament[vtra$Tractament == "Sembra directa civada i veça"] <- "Civ_ve"
vtra$Tractament[vtra$Tractament == "Retirada de soques"] <- "Ret_soq"

#Unir columna de muestreo verdadero

vtra$Most<-"VERDADERO"
colnames(vtra)[colnames(vtra) == "ID.x"]<-"ID"

#Cargar todos los recubrimientos de todas las finas (1,2,3 años)

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")

vtod<-read.csv("Fincas-tratamientos TODOS prov.csv",sep = ",",
               header=TRUE,fill = TRUE)

#Quitar las fincas muestreadas los 3 años porque el recubrimiento no es válido (NA)

vtod<-vtod[-which(vtod$Codi_Finca %in% vtra$Codi_Finca),] 

#vtod Contiene fincas muestreadas 1 y 2 años con datos de recubrimiento válido
#Unir a fincas muestreadas los 3 años con datos de recubrimiento válido

v<-full_join(vtod,vtra)
names(vtod)
names(vtra)

v<-arrange(v,Codi_Finca,Any)


#VISUALIZACIÓN DE LOS DATOS PARA 2016:

vtra<-v[v$Any == "2016" & v$Most == "VERDADERO",]

row.names(vtra)<-seq(length = nrow (vtra))

#Hay fincas el 2016 que tenían de tratamiento herbicidar y picar y no se ha añadido

veg<-read.csv("Fincas Ariza 2016.csv",sep = ";",
              header=TRUE,fill = TRUE)

veg$Tractament1<-"Herb"
veg$Any<-2016

veg<-veg[,c(1,9,10)]

vtra<-full_join(vtra,veg)

vtra$Tractament[vtra$Tractament1 == "Herb"]<- "Herb"
vtra<-vtra[,-c(13)]
#Guardar como TRATAMIENTOS BUENOS 2016

write.csv(vtra,file = "Tratamientos_buenos_2016.csv")

#Eliminar los usos que no sean barbecho, alfalfa o NA

vtra$US[is.na(vtra$US)]<-"1" #Convertir NA en 1

class(vtra$US)

vtra$US<-as.integer(vtra$US)
vtra <- vtra[vtra$US %in% c(1,2,4), ]


vtra$Tractament<-as.factor(vtra$Tractament)
levels(vtra$Tractament)

length(which(vtra$Tractament == "Civ_ve"))
length(which(vtra$Tractament == "Trit"))
length(which(vtra$Tractament == "Alfals"))
length(which(vtra$Tractament == "Control"))
length(which(vtra$Tractament == "Curronar"))
length(which(vtra$Tractament == "Herb"))
length(which(vtra$Tractament == "Llaurar"))
length(which(vtra$Tractament == "Picar"))

#Eliminar tratamientos "Civ_ve" y "Trit" por pocas observaciones

vtra<-vtra[vtra$Tractament %in% c("Alfals","Control","Curronar","Llaurar","Picar","Herb"),]
vtra$Tractament<-as.character(vtra$Tractament)

#Llamar a control "No tratamiento"?


#TODOS LOS SECANOS Y AÑOS

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

mtext("Efecte del tractament en recobriment de vegetació",side=3,line=1,outer = TRUE, cex = 1.1)

#SECANOS OCCIDENTALES: TODOS

voc <- vtra[vtra$Secano == "OCCIDENTAL", ]
par(mfrow = c(3,1),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,0) + 0.1)
boxplot(Recob_plotViu ~ Tractament, voc)
mtext("% Veg viva",side=2,line=3, cex = 0.9)
boxplot(Recob_plotMort ~ Tractament, voc) 
mtext("% Veg morta",side=2,line=3, cex = 0.9)
boxplot(PromigAltura1Plot ~ Tractament, voc)
mtext("Mitjana Altura",side=2,line=3, cex = 0.9)

mtext("Tractament",side=1,line=1,outer = TRUE,cex = 0.9)

mtext("SECANS OCCIDENTALS: Efecte del tractament en el recobriment de la vegetació",side=3,line=1,outer = TRUE, cex = 1.1)

#SECANOS OCCIDENTALES: POR SECTORES

#Alfés
af <- vtra[vtra$Secano == "OCCIDENTAL" & vtra$Sector1 == "AF", ]

par(mfrow = c(3,1),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,0) + 0.1)
boxplot(Recob_plotViu ~ Tractament, af)
mtext("% Veg viva",side=2,line=3, cex = 0.9)
boxplot(Recob_plotMort ~ Tractament, af) 
mtext("% Veg morta",side=2,line=3, cex = 0.9)
boxplot(PromigAltura1Plot ~ Tractament, af)
mtext("Mitjana Altura",side=2,line=3, cex = 0.9)

mtext("Tractament",side=1,line=1,outer = TRUE,cex = 0.9)
mtext("SECTOR ALFÉS: Efecte del tractament en recobriment de vegetació",side=3,line=1,outer = TRUE, cex = 1.1)

length(which(af$Tractament == "Curronar"))
length(which(af$Tractament == "Control"))
#No sé si merece la pena hacerlo por sectores, porque hay mucha diferencia en el número de obs. por tratamiento
#(por ejemplo, 1 curronar y 18 control)

#Utxesa
ut <- vtra[vtra$Secano == "OCCIDENTAL" & vtra$Sector1 == "UT", ]
par(mfrow = c(3,1),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,0) + 0.1)
boxplot(Recob_plotViu ~ Tractament, ut)
mtext("% Veg viva",side=2,line=3, cex = 0.9)
boxplot(Recob_plotMort ~ Tractament, ut) 
mtext("% Veg morta",side=2,line=3, cex = 0.9)
boxplot(PromigAltura1Plot ~ Tractament, ut)
mtext("Mitjana Altura",side=2,line=3, cex = 0.9)

mtext("Tractament",side=1,line=1,outer = TRUE,cex = 0.9)
mtext("SECTOR UTXESA: Efecte del tractament en recobriment de vegetació",side=3,line=1,outer = TRUE, cex = 1.1)

#Granja
gr <- vtra[vtra$Secano == "OCCIDENTAL" & vtra$Sector1 == "GR", ]
par(mfrow = c(3,1),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,0) + 0.1)
boxplot(Recob_plotViu ~ Tractament, gr)
mtext("% Veg viva",side=2,line=3, cex = 0.9)
boxplot(Recob_plotMort ~ Tractament, gr) 
mtext("% Veg morta",side=2,line=3, cex = 0.9)
boxplot(PromigAltura1Plot ~ Tractament, gr)
mtext("Mitjana Altura",side=2,line=3, cex = 0.9)

mtext("Tractament",side=1,line=1,outer = TRUE,cex = 0.9)
mtext("SECTOR GRANJA D'ESCARP: Efecte del tractament en recobriment de vegetació",side=3,line=1,outer = TRUE, cex = 1.1)

#SECTORES OCCIDENTALES: TODOS SECTORES EN UNO

par(mfrow = c(3,3),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,2) + 0.1)
#Viva
boxplot(Recob_plotViu ~ Tractament,af,names = c(" "," "," "))
mtext("% Veg viva",side=2,line=3, cex = 1)
mtext("Alfés",side=3,line=1,cex = 1)
boxplot(Recob_plotViu ~ Tractament, ut,names = c(" "))
mtext("Utxesa",side=3,line=1,cex = 1)
boxplot(Recob_plotViu ~ Tractament, gr,names = c(" "," "))
mtext("Granja d'Escarp",side=3,line=1,cex = 1)
#Muerta
boxplot(Recob_plotMort ~ Tractament, af,names = c(" "," "," "))
mtext("% Veg morta",side=2,line=3, cex = 1)
boxplot(Recob_plotMort ~ Tractament, ut, names = c(" "))
boxplot(Recob_plotMort ~ Tractament, gr, names = c(" "," "))
#Altura
boxplot(PromigAltura1Plot ~ Tractament,af)
mtext("Mitjana altura",side=2,line=3, cex = 1)
boxplot(PromigAltura1Plot ~ Tractament,ut)
mtext("Control",side=1,line=1, cex = 0.7)
boxplot(PromigAltura1Plot ~ Tractament,gr)

mtext("Tractament",side=1,line=1,outer = TRUE,cex = 1)
mtext ("SECTORS OCCIDENTALS: Efecte del tractament en el recobriment de la vegetació",side=3,line=2,outer = TRUE, cex = 1.1)

#SECANOS ORIENTALES: TODOS

vor <- vtra[vtra$Secano == "ORIENTAL", ]
par(mfrow = c(3,1),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,0) + 0.1)
boxplot(Recob_plotViu ~ Tractament, vor)
mtext("% Veg viva",side=2,line=3, cex = 0.9)
boxplot(Recob_plotMort ~ Tractament, vor) 
mtext("% Veg morta",side=2,line=3, cex = 0.9)
boxplot(PromigAltura1Plot ~ Tractament, vor)
mtext("Mitjana Altura",side=2,line=3, cex = 0.9)

mtext("Tractament",side=1,line=1,outer = TRUE,cex = 0.9)

mtext("SECANS ORIENTALS: Efecte del tractament en el recobriment de la vegetació",side=3,line=1,outer = TRUE, cex = 1.1)

#SECANOS ORIENTALES: POR SECTORES

#Belianes
be <- vtra[vtra$Secano == "ORIENTAL" & vtra$Sector1 == "BE", ]

par(mfrow = c(3,1),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,0) + 0.1)
boxplot(Recob_plotViu ~ Tractament, be)
mtext("% Veg viva",side=2,line=3, cex = 0.9)
boxplot(Recob_plotMort ~ Tractament, be) 
mtext("% Veg morta",side=2,line=3, cex = 0.9)
boxplot(PromigAltura1Plot ~ Tractament, be)
mtext("Mitjana Altura",side=2,line=3, cex = 0.9)

mtext("Tractament",side=1,line=1,outer = TRUE,cex = 0.9)
mtext("SECTOR BELIANES: Efecte del tractament en recobriment de vegetació",side=3,line=1,outer = TRUE, cex = 1.1)

#Bellmunt
bel <- vtra[vtra$Secano == "ORIENTAL" & vtra$Sector1 == "BM", ]

par(mfrow = c(3,1),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,0) + 0.1)
boxplot(Recob_plotViu ~ Tractament, bel)
mtext("% Veg viva",side=2,line=3, cex = 0.9)
boxplot(Recob_plotMort ~ Tractament, bel) 
mtext("% Veg morta",side=2,line=3, cex = 0.9)
boxplot(PromigAltura1Plot ~ Tractament, bel)
mtext("Mitjana Altura",side=2,line=3, cex = 0.9)

mtext("Tractament",side=1,line=1,outer = TRUE,cex = 0.9)
mtext("SECTOR BELLMUNT: Efecte del tractament en recobriment de vegetació",side=3,line=1,outer = TRUE, cex = 1.1)

#Sió
si <- vtra[vtra$Secano == "ORIENTAL" & vtra$Sector1 == "SI", ]

par(mfrow = c(3,1),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,0) + 0.1)
boxplot(Recob_plotViu ~ Tractament, si)
mtext("% Veg viva",side=2,line=3, cex = 0.9)
boxplot(Recob_plotMort ~ Tractament, si) 
mtext("% Veg morta",side=2,line=3, cex = 0.9)
boxplot(PromigAltura1Plot ~ Tractament, si)
mtext("Mitjana Altura",side=2,line=3, cex = 0.9)

mtext("Tractament",side=1,line=1,outer = TRUE,cex = 0.9)
mtext("SECTOR SIÓ: Efecte del tractament en recobriment de vegetació",side=3,line=1,outer = TRUE, cex = 1.1)

#SECTORES OCCIDENTALES: TODOS SECTORES EN UNO

par(mfrow = c(3,3),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,2) + 0.1)
#Viva
boxplot(Recob_plotViu ~ Tractament,be,names = c(" "," "," "," "," "))
mtext("% Veg viva",side=2,line=3, cex = 1)
mtext("Belianes",side=3,line=1,cex = 1)
boxplot(Recob_plotViu ~ Tractament, bel,names = c(" "," "))
mtext("Bellmunt",side=3,line=1,cex = 1)
boxplot(Recob_plotViu ~ Tractament, si,names = c(" "," "," "," "))
mtext("Sió",side=3,line=1,cex = 1)
#Muerta
boxplot(Recob_plotMort ~ Tractament, be,names = c(" "," "," "," "," "))
mtext("% Veg morta",side=2,line=3, cex = 1)
boxplot(Recob_plotMort ~ Tractament, bel, names = c(" "," "))
boxplot(Recob_plotMort ~ Tractament, si, names = c(" "," "," "," "))
#Altura
boxplot(PromigAltura1Plot ~ Tractament,be)
mtext("Mitjana altura",side=2,line=3, cex = 1)
boxplot(PromigAltura1Plot ~ Tractament,bel)
boxplot(PromigAltura1Plot ~ Tractament,si)

mtext("Tractament",side=1,line=1,outer = TRUE,cex = 1)
mtext ("SECTORS ORIENTALS: Efecte del tractament en el recobriment de la vegetació",side=3,line=2,outer = TRUE, cex = 1.1)






