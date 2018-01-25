
#1.1. TENDENCIAS RECUBRIMIENTO DE VEGETACIÓN VIVA, MUERTA Y ALTURA
      #PARA FINCAS MUESTREADAS LOS 3 AÑOS

rm(list=ls())
setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Fincas sin sp 3 años")

fin<-read.csv("Fincas 3 años.csv",sep = ",",
              header=TRUE,fill = TRUE)

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")
veg<-read.csv("EvolucioOcells_consulta.csv",sep = ";",
              header=TRUE,fill = TRUE)

#Sólo datos de fincas muestreadas los 3 años

vfin<-veg[which(veg$Codi_Finca %in% fin$Codi_Finca), ]

#RECUBRIMIENTO VEGETACIÓN - AÑO
#Como no voy a ver el efecto de las especies, restringir los datos para 1 observación por año y por parcela

vfin<-vfin[vfin$especieObjectiu == "BUOED", ]
vrec<-vfin[ ,c(2,3,5,11,12,13)]
vrec$Recob_plotViu<-sub(",",".", vrec$Recob_plotViu)
vrec$Recob_plotMort<-sub(",",".", vrec$Recob_plotMort)
vrec$PromigAltura1Plot <-sub(",",".", vrec$PromigAltura1Plot)

vrec$Recob_plotViu<-as.numeric(vrec$Recob_plotViu)
vrec$Recob_plotMort<-as.numeric(vrec$Recob_plotMort)
vrec$PromigAltura1Plot <-as.numeric(vrec$PromigAltura1Plot)

vrec$Sector1<-vrec$Sector
vrec$Sector1[vrec$Sector1 == 4] <- "AF"
vrec$Sector1[vrec$Sector1 == 2] <- "BE"
vrec$Sector1[vrec$Sector1 == 3] <- "BM"
vrec$Sector1[vrec$Sector1 == 5] <- "GR"
vrec$Sector1[vrec$Sector1 == 1] <- "SI"
vrec$Sector1[vrec$Sector1 == 10] <- "UT"

#Data exploration

#1. Outliers: No, many zeros

par(mfrow= c (1,2), mar = c(5,4,2,1))
boxplot(vrec$Recob_plotViu,  ylab = "Recubrimiento Viva (%)")

dotchart(vrec$Recob_plotViu, xlab = "Recubrimiento Viva (%)",
         ylab = "Order of the data")


par(mfrow= c (1,2), mar = c(5,4,2,1))
boxplot(vrec$Recob_plotMort,  ylab = "Recubrimiento Muerta (%)")

dotchart(vrec$Recob_plotMort, xlab = "Recubrimiento Muerta (%)",
         ylab = "Order of the data")



#2. Homogeneity of variance. How the % of recobrimiento varia con el año

#RECUBRIMIENTO - AÑO

par(mfrow = c(1,3),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,0) + 0.1)
boxplot(Recob_plotViu ~ Any, vrec, main = "% Veg viva")
boxplot(Recob_plotMort ~ Any, vrec, main = "% Veg muerta") #No homogenea. Desciende con el año
boxplot(PromigAltura1Plot ~ Any, vrec, main = "Promedio Altura")

mtext("Año",side=1,line=1,outer = TRUE,cex = 0.9)
mtext(" ",side=2,line=1,outer = TRUE, cex = 0.9)
mtext("Tendencia temporal de la vegetación",side=3,line=1,outer = TRUE, cex = 1.1)

#RECUBRIMIENTO - SECTOR
par(mfrow = c(1,3),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,0) + 0.1)
boxplot(Recob_plotViu ~ Sector1, vrec, main = "% Veg viva")
boxplot(Recob_plotMort ~ Sector1, vrec, main = "% Veg muerta") #No homogenea. Desciende con el año
boxplot(PromigAltura1Plot ~ Sector1, vrec, main = "Promedio Altura")

mtext("Sector",side=1,line=1,outer = TRUE,cex = 0.9)
mtext(" ",side=2,line=1,outer = TRUE, cex = 0.9)
mtext("Tendencia de la vegetación por sectores",side=3,line=1,outer = TRUE, cex = 1.1)

#RECUBRIMIENTO - SECANO
vrec$Secano<-vrec$Sector
vrec$Secano[vrec$Sector == 4] <- "OCCIDENTAL"
vrec$Secano[vrec$Sector == 2] <- "ORIENTAL"
vrec$Secano[vrec$Sector == 3] <- "ORIENTAL"
vrec$Secano[vrec$Sector == 5] <- "OCCIDENTAL"
vrec$Secano[vrec$Sector == 1] <- "ORIENTAL"
vrec$Secano[vrec$Sector == 10] <- "OCCIDENTAL"

par(mfrow = c(1,3),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,0) + 0.1)
boxplot(Recob_plotViu ~ Secano, vrec, main = "% Veg viva")
boxplot(Recob_plotMort ~ Secano, vrec, main = "% Veg muerta") #No homogenea. Desciende con el año
boxplot(PromigAltura1Plot ~ Secano, vrec, main = "Promedio Altura")

mtext("Sector",side=1,line=1,outer = TRUE,cex = 0.9)
mtext(" ",side=2,line=1,outer = TRUE, cex = 0.9)
mtext("Tendencia de la vegetación por secanos",side=3,line=1,outer = TRUE, cex = 1.1)

#TENDENCIA TEMPORAL POR SECANOS

#OCCIDENTALES

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

 #Selecciono las filas que contengan los Codi_finca de fincas muestreadas 3 años
fin3<- fin[which(fin$Codi_Finca %in% vrec$Codi_Finca), ]
fin3<-fin3[,c(2,5)]
#Uno el ID a las fincas(3 años) para asociarlo después con el tratamiento y uso
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



#En los secanos orientales, separar la evolución de la vegetación en Barbecho/Alfalfa
#TENDENCIA TEMPORAL BARBECHO
sor <- aus[aus$Secano == "ORIENTAL", ]
barb <- sor[sor$US == "4",]
barb <- barb[complete.cases(barb),]

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
alf <- alf[complete.cases(alf),]

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

##########################################################################


#3. Normality: No normal distribution, many 0 in all years

hist(vrec$Recob_plotViu)
hist(vrec$Recob_plotMort)

library(lattice)
histogram( ~ Recob_plotViu | Any, type = "count",
           xlab = "Recub viva (%)",
           ylab = "Frequency",
           nint=30,layout=c(1,3),
           strip.left = strip.custom(bg = 'white'),
           strip = F,
           col.line = "black", col = "white",
           scales = list(x = list(relation = "same"),
                         y = list(relation = "same"),
                         draw = TRUE),
           subset = Any =="2014" | Any == "2015" |Any == "2016",
           data = vrec)


#There is a high number of 0, and the response is a percentage (Denominator = 100). 

#Distribution of the response: It is a continuous percentage. In this case, it doesn't follow a binomial distribution
# because this assumes that the probability of success of an experiment has 2 outcomes: Success and failure. Since it is a
#continuous probability, the best is a beta regression.




