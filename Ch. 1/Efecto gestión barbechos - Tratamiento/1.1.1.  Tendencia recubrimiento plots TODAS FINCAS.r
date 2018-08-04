

#1.1. TENDENCIAS RECUBRIMIENTO DE VEGETACIÓN VIVA, MUERTA Y ALTURA
#PARA TODAS LAS FINCAS (MUESTREADAS 1,2 o los 3 AÑOS)


###############################Primero, cargar las fincas de 3 años unidas al recubrimiento
rm(list=ls())

library(dplyr)


setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")
tra<-read.csv("Fincas-tratamientos.csv",sep = ",",
              header=TRUE,fill = TRUE)
veg<-read.csv("EvolucioOcells_consulta.csv",sep = ";",
              header=TRUE,fill = TRUE)

vfin<-veg[which(veg$Codi_Finca %in% tra$Codi_Finca), ]

#Escoger variables recubrimiento plots
#Como no voy a ver el efecto de las especies, restringir los datos para 1 observación por año y por parcela

vfin<-vfin[vfin$especieObjectiu == "BUOED", ]
vrec<-vfin[ ,c(2,3,5,11,12,13)]
vrec$Recob_plotViu<-sub(",",".", vrec$Recob_plotViu)
vrec$Recob_plotMort<-sub(",",".", vrec$Recob_plotMort)
vrec$PromigAltura1Plot <-sub(",",".", vrec$PromigAltura1Plot)

vrec$Recob_plotViu<-as.numeric(vrec$Recob_plotViu)
vrec$Recob_plotMort<-as.numeric(vrec$Recob_plotMort)
vrec$PromigAltura1Plot <-as.numeric(vrec$PromigAltura1Plot)

#Unir columna de muestreo verdadero

vrec$Most<-"VERDADERO"
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

#Cargar todos los recubrimientos de todas las finas (1,2,3 años)

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")

vtod<-read.csv("Fincas-tratamientos TODOS prov.csv",sep = ",",
               header=TRUE,fill = TRUE)

#Quitar las fincas muestreadas los 3 años porque el recubrimiento no es válido (NA)

vtod<-vtod[-which(vtod$Codi_Finca %in% vrec$Codi_Finca),] 

#vtod Contiene fincas muestreadas 1 y 2 años con datos de recubrimiento válido
#Unir a fincas muestreadas los 3 años con datos de recubrimiento válido

v<-full_join(vtod,vrec)
names(vtod)
names(vrec)

vrec<-arrange(v,Codi_Finca,Any)

#EXPLORACIÓN DE LA TENDENCIA TEMPORAL DEL RECUBRIMIENTO POR SECANOS

#TODOS
voc <- vrec[vrec$Secano == "OCCIDENTAL", ]

par(mfrow = c(1,3),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,0) + 0.1)
boxplot(Recob_plotViu ~ Any, voc, main = "% Veg viva")
boxplot(Recob_plotMort ~ Any, voc, main = "% Veg morta") #No homogenea. Desciende con el año
boxplot(PromigAltura1Plot ~ Any, voc, main = "Mitjana Altura")

mtext("Any",side=1,line=1,outer = TRUE,cex = 0.9)
mtext(" ",side=2,line=1,outer = TRUE, cex = 0.9)
mtext("SECANS OCCIDENTALS: Tendència temporal de la vegetació",side=3,line=1,outer = TRUE, cex = 1.1)

unique(voc$Sector1)

#POR SECTORES
af <- vrec[vrec$Secano == "OCCIDENTAL" & vrec$Sector1 == "AF", ]
ut <- vrec[vrec$Secano == "OCCIDENTAL" & vrec$Sector1 == "UT", ]
gr <- vrec[vrec$Secano == "OCCIDENTAL" & vrec$Sector1 == "GR", ]


p<-gr[which(gr$Any == 2015),]

par(mfrow = c(3,3),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,4) + 0.1)
#Viva
boxplot(Recob_plotViu ~ Any, af,names = c(" "," "," "))
mtext("% Veg viva",side=2,line=3, cex = 1)
mtext("Alfés",side=3,line=1,cex = 1)
boxplot(Recob_plotViu ~ Any, ut,names = c(" "," "," "))
mtext("Utxesa",side=3,line=1,cex = 1)
boxplot(Recob_plotViu ~ Any, gr,names = c(" "," "," "))
mtext("Granja d'Escarp",side=3,line=1,cex = 1)
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


#ORIENTALES

vor <- vrec[vrec$Secano == "ORIENTAL", ]

par(mfrow = c(1,3),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,0) + 0.1)
boxplot(Recob_plotViu ~ Any, vor, main = "% Veg viva")
boxplot(Recob_plotMort ~ Any, vor, main = "% Veg morta") #No homogenea. Desciende con el año
boxplot(PromigAltura1Plot ~ Any, vor, main = "Mitjana Altura")

mtext("Año",side=1,line=1,outer = TRUE,cex = 0.9)
mtext(" ",side=2,line=1,outer = TRUE, cex = 0.9)
mtext("SECANOS ORIENTALES: Tendencia temporal de la vegetación",side=3,line=1,outer = TRUE, cex = 1.1)

vor <- vrec[vrec$Secano == "ORIENTAL" & vrec$Sector1 == "BE", ]





#USO 
##Separar Barbechos de Alfalfa para los secanos orientales (Sisón)

#1. Unir al ID finca
setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")
fin<-read.csv("Finca.csv",sep = ";",
              header=TRUE,fill = TRUE)

fin3<-fin[,c(2,5)] #Todas las fincas-ID 

#Uno el ID a las fincas  para asociarlo después con el tratamiento y uso

library(dplyr)

fid<-left_join(vrec,fin3,by = "Codi_Finca")

#Me cargo ID.x y US porque los voy a unir todos ahora

fid<-fid[,-c(9,10)]
colnames(fid)[colnames(fid) == "ID.y"]<-"ID"

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
gus<-gus[,-c(14,16)] #Elimina Us y Any.y (se queda con US.x y US.y)

gus$usos <- gus[,"US.x"]
gus[is.na(gus[, "usos"]), "usos"] <- gus[is.na(gus[, "usos"]), "US.y"]

#Alfalfa
a<-e[grep("Alfals",e$Us),]
a$US <- 2
a$usa<-a$US
a<-a[,-c(3)]
a$Any <- 2016
aus<-left_join(gus,a,by = c("ID" = "ID"))
aus<-aus[,-c(16,18)] #Quita Us y Any

aus$usos2 <- aus[, "usos"]
aus[is.na(aus[, "usos2"]), "usos2"] <- aus[is.na(aus[, "usos2"]), "usa"]

aus<-aus[,-c(13:16)] #Quita todos los usos menos usos2
aus$US<-aus$usos2
aus<-aus[,-c(13)] #quita usos 2



#En los secanos orientales, separar la evolución de la vegetación en Barbecho/Alfalfa
#TENDENCIA TEMPORAL BARBECHO
sor <- aus[aus$Secano == "ORIENTAL", ]
barb <- sor[sor$US == "4",]

par(mfrow = c(1,3),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,0) + 0.1)
boxplot(Recob_plotViu ~ Any.x, barb, main = "% Veg viva")
boxplot(Recob_plotMort ~ Any.x, barb, main = "% Veg muerta") #No homogenea. Desciende con el año
boxplot(PromigAltura1Plot ~ Any.x, barb, main = "Promedio Altura")

mtext("Año",side=1,line=1,outer = TRUE,cex = 0.9)
mtext(" ",side=2,line=1,outer = TRUE, cex = 0.9)
mtext("BARBECHO ORIENTAL: Tendencia temporal de la vegetación",side=3,line=1,outer = TRUE, cex = 1.1)

#TENDENCIA TEMPORAL ALFALFA
sor <- aus[aus$Secano == "ORIENTAL", ]
alf <- sor[sor$US == "2",]

par(mfrow = c(1,3),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,0) + 0.1)
boxplot(Recob_plotViu ~ Any.x, alf, main = "% Veg viva")
boxplot(Recob_plotMort ~ Any.x, alf, main = "% Veg muerta") #No homogenea. Desciende con el año
boxplot(PromigAltura1Plot ~ Any.x, alf, main = "Promedio Altura")

mtext("Año",side=1,line=1,outer = TRUE,cex = 0.9)
mtext(" ",side=2,line=1,outer = TRUE, cex = 0.9)
mtext ("ALFALFA ORIENTAL: Tendencia temporal de la vegetación",side=3,line=1,outer = TRUE, cex = 1.1)

#Gráfica por Variable:

par(mfrow = c(3,2),
    oma = c(3,3,3,1) + 0.1,
    mar = c(1,2,3,4) + 0.1)
boxplot(Recob_plotViu ~ Any.x, barb,names = c(" "," "," "))
mtext("% Veg viva",side=2,line=3, cex = 0.9)
mtext("Guaret",side=3,line=1,cex = 1)
boxplot(Recob_plotViu ~ Any.x, alf,names = c(" "," "," "))
mtext("Alfals",side=3,line=1,cex = 1)
boxplot(Recob_plotMort ~ Any.x, barb,names = c(" "," "," "))
mtext("% Veg morta",side=2,line=3, cex = 0.9)
boxplot(Recob_plotMort ~ Any.x, alf,names = c(" "," "," "))
boxplot(PromigAltura1Plot ~ Any.x, barb)
mtext("Mitjana altura",side=2,line=3, cex = 0.9)
boxplot(PromigAltura1Plot ~ Any.x, alf)

mtext("Any",side=1,line=1,outer = TRUE,cex = 1)
mtext ("SECANS ORIENTALS: Tendència temporal de la vegetació",side=3,line=1,outer = TRUE, cex = 1.1)

#Gráfica de barbecho/alfalfa oriental POR SECTOR

#BELIANES
beb <- barb[barb$Sector1 == "BE", ]
bea <- alf[alf$Sector1 == "BE", ]

par(mfrow = c(3,2),
    oma = c(3,3,3,1) + 0.1,
    mar = c(1,2,3,4) + 0.1)
boxplot(Recob_plotViu ~ Any.x, beb,names = c(" "," "," "))
mtext("% Veg viva",side=2,line=3, cex = 0.9)
mtext("Guaret",side=3,line=1,cex = 1)
boxplot(Recob_plotViu ~ Any.x, bea,names = c(" "," "," "))
mtext("Alfals",side=3,line=1,cex = 1)
boxplot(Recob_plotMort ~ Any.x, beb,names = c(" "," "," "))
mtext("% Veg morta",side=2,line=3, cex = 0.9)
boxplot(Recob_plotMort ~ Any.x, bea,names = c(" "," "," "))
boxplot(PromigAltura1Plot ~ Any.x, beb)
mtext("Mitjana altura",side=2,line=3, cex = 0.9)
boxplot(PromigAltura1Plot ~ Any.x, bea)

mtext("Any",side=1,line=1,outer = TRUE,cex = 1)
mtext ("SECTOR BELIANES: Tendència temporal de la vegetació",side=3,line=1,outer = TRUE, cex = 1.1)

#BELLMUNT
belb <- barb[barb$Sector1 == "BM", ]
bela <- alf[alf$Sector1 == "BM", ]

par(mfrow = c(3,2),
    oma = c(3,3,3,1) + 0.1,
    mar = c(1,2,3,4) + 0.1)
boxplot(Recob_plotViu ~ Any.x, belb,names = c(" "," "," "))
mtext("% Veg viva",side=2,line=3, cex = 0.9)
mtext("Guaret",side=3,line=1,cex = 1)
boxplot(Recob_plotViu ~ Any.x, bela,names = c(" "," "," "))
mtext("Alfals",side=3,line=1,cex = 1)
boxplot(Recob_plotMort ~ Any.x, belb,names = c(" "," "," "))
mtext("% Veg morta",side=2,line=3, cex = 0.9)
boxplot(Recob_plotMort ~ Any.x, bela,names = c(" "," "," "))
boxplot(PromigAltura1Plot ~ Any.x, belb)
mtext("Mitjana altura",side=2,line=3, cex = 0.9)
boxplot(PromigAltura1Plot ~ Any.x, bela)

mtext("Any",side=1,line=1,outer = TRUE,cex = 1)
mtext ("SECTOR BELLMUNT: Tendència temporal de la vegetació",side=3,line=1,outer = TRUE, cex = 1.1)

#SIÓ
sib <- barb[barb$Sector1 == "SI", ]
sia <- alf[alf$Sector1 == "SI", ]

par(mfrow = c(3,2),
    oma = c(3,3,3,1) + 0.1,
    mar = c(1,2,3,4) + 0.1)
boxplot(Recob_plotViu ~ Any.x, sib,names = c(" "," "," "))
mtext("% Veg viva",side=2,line=3, cex = 0.9)
mtext("Guaret",side=3,line=1,cex = 1)
boxplot(Recob_plotViu ~ Any.x, sia,names = c(" "," "," "))
mtext("Alfals",side=3,line=1,cex = 1)
boxplot(Recob_plotMort ~ Any.x, sib,names = c(" "," "," "))
mtext("% Veg morta",side=2,line=3, cex = 0.9)
boxplot(Recob_plotMort ~ Any.x, sia,names = c(" "," "," "))
boxplot(PromigAltura1Plot ~ Any.x, sib)
mtext("Mitjana altura",side=2,line=3, cex = 0.9)
boxplot(PromigAltura1Plot ~ Any.x, sia)

mtext("Any",side=1,line=1,outer = TRUE,cex = 1)
mtext ("SECTOR SIÓ: Tendència temporal de la vegetació",side=3,line=1,outer = TRUE, cex = 1.1)











