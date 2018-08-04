
#COMPROBAR DATOS CON BASE DE DATOS

rm(list=ls())
setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Fincas sin sp 3 años")

fin<-read.csv("Fincas 3 años.csv",sep = ",",
              header=TRUE,fill = TRUE)

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")
veg<-read.csv("EvolucioOcells_20170503.csv",sep = ";",
              header=TRUE,fill = TRUE)
veg2<-read.csv("Plots_2014-2016.csv",sep = ";",
              header=TRUE,fill = TRUE)

vfin2<-veg2[which(veg2$Codi_Finca %in% fin$Codi_Finca), ]
vrec<-vfin2

#Tendencia temporal secanos occidentales
voc <- vrec[vrec$Seca == "OC", ]
voc$Alt_1<-as.integer(voc$Alt_1)

par(mfrow = c(1,3),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,0) + 0.1)
boxplot(Cobert_1_viu ~ Any, voc, main = "% Veg viva")
boxplot(Cobert_1_mort ~ Any, voc, main = "% Veg morta") #No homogenea. Desciende con el año
boxplot(Alt_1 ~ Any, voc, main = "Mitjana Altura")

mtext("Any",side=1,line=1,outer = TRUE,cex = 0.9)
mtext(" ",side=2,line=1,outer = TRUE, cex = 0.9)
mtext("SECANS OCCIDENTALS: Tendència temporal de la vegetació",side=3,line=1,outer = TRUE, cex = 1.1)

#Cambiar nombres para que me coincida
names(vrec)[names(vrec) == "Cobert_1_viu"]<-"Recob_plotViu"
names(vrec)[names(vrec) == "Cobert_1_mort"]<-"Recob_plotMort"
names(vrec)[names(vrec) == "Alt_1"]<-"PromigAltura1Plot"




#POR SECTORES
af <- vrec[vrec$Seca == "OC" & vrec$Sector == "AF", ]
ut <- vrec[vrec$Seca == "OC" & vrec$Sector == "UT", ]
gr <- vrec[vrec$Seca == "OC" & vrec$Sector == "GR", ]


par(mfrow = c(3,3),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,4) + 0.1)
#Viva
boxplot(Recob_plotViu ~ Any, af,names = c(" "," "," "))
mtext("% Veg viva",side=2,line=3, cex = 1)
mtext("AF",side=3,line=1,cex = 1)
boxplot(Recob_plotViu ~ Any, ut,names = c(" "," "," "))
mtext("UT",side=3,line=1,cex = 1)
boxplot(Recob_plotViu ~ Any, gr,names = c(" "," "," "))
mtext("GR",side=3,line=1,cex = 1)
#Muerta
boxplot(Recob_plotMort ~ Any, af,names = c(" "," "," "))
mtext("% Veg morta",side=2,line=3, cex = 1)
boxplot(Recob_plotMort ~ Any, ut, names = c(" "," "," "))
boxplot(Recob_plotMort ~ Any, gr, names = c(" "," "," "))
#Altura
boxplot(PromigAltura1Plot ~ Any,af)
mtext("Mitjana altura",side=2,line=3, cex = 1)
boxplot(PromigAltura1Plot ~ Any,ut)
boxplot(PromigAltura1Plot ~ Any,gr)

mtext("Any",side=1,line=1,outer = TRUE,cex = 1)
mtext ("SECTORS OCCIDENTALS: Tendència temporal de la vegetació",side=3,line=2,outer = TRUE, cex = 1.1)
