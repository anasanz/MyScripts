

#EFECTO DE EL TRATAMIENTO EN EL RECUBRIMIENTO DE LA VEGETACIÓN (VIVA, MUERTA , ALTURA)

rm(list=ls())

library(dplyr)

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



#Importar tratamientos de primavera

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")
con<-read.csv("Consulta_tratamientos.csv",sep = ";",
              header=TRUE,fill = TRUE)
con<-arrange(con,Codi_Finca)
#Sólo tratamientos en primavera de fincas muestreadas los 3 años
tra<-con[which(con$Codi_Finca %in% vrec$Codi_Finca), ] 
tra<-arrange(tra,Codi_Finca)
#Hay 73 fincas con tratamientos en primavera (de 91 fincas muestreadas los 3 años)

f<-anti_join(aus,con) #Hay 18 fincas para las q no aparece tratamiento en primavera
#Ver si tienen tratamientos en otras fechas (importar full excel con todos los tratamients)

tardor<-read.csv("Consulta_tratamientos TARDOR.csv",sep = ";",
                 header=TRUE,fill = TRUE)
i<-tardor[which(tardor$ID %in% f$ID),] 
unique(i$ID) 
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

#FINCAS CON DOBLE OBSERVACIÓN TRATAMIENTOS!(Doc DATOS BARBECHOS ARRENDADOS)

#Unificar tratamientos (añadir categorías)

 ##En duplicados que son Herbicidar + Alfalfa, quedarse con Alfalfa
    #(el primer año de alfalfa la herbicidan para que compita mejor)
vtra<-vtra[-which(vtra$US == 2 & vtra$Tractament == 5),]

    ##En los duplicados de 2 usos en mismo año y finca, coger el uso q proviene
      #de la información aportada por el CTFC. 
rownames(vtra) <- seq(length=nrow(vtra)) #Reestablecer los nombres de las columnas
                                        #para hacer subsets of the data
vtra<-vtra[-c(44,82,98),]

    ##Selección de alfalfa y último tratamiento relevante de ciertas fincas
rownames(vtra) <- seq(length=nrow(vtra)) 
vtra<-vtra[-c(80,128,154,156,178)]
rownames(vtra) <- seq(length=nrow(vtra)) 
vtra<-vtra[- which(vtra$US == 2 & vtra$Tractament == 1),]

  #???Crear categoría para "Picar y luego herbicidar" (Categoría 20)

vtra<-distinct(vtra,Codi_Finca,Any,Recob_plotViu,Recob_plotMort,PromigAltura1Plot,.keep_all = TRUE)

rownames(vtra) <- seq(length=nrow(vtra)) 

vtra$Tractament<-as.factor(vtra$Tractament)
levels(vtra$Tractament)<-c(levels(vtra$Tractament),"20") #Añadir el nivel 20

#Sustituir el tratamiento que haya en las de picar y herbicidar por el número 20 (categoria)
vtra$Tractament<-as.integer(vtra$Tractament)

vtra$Tractament[vtra$Codi_Finca == "GR10A" & vtra$Any == "2014"]<- 20
vtra$Tractament[vtra$Codi_Finca == "GR10A" & vtra$Any == "2015"]<- 20

vtra$Tractament[vtra$Codi_Finca == "GR11A" & vtra$Any == "2014"]<- 20
vtra$Tractament[vtra$Codi_Finca == "GR11A" & vtra$Any == "2015"]<- 20

vtra$Tractament[vtra$Codi_Finca == "GR12A" & vtra$Any == "2014"]<- 20
vtra$Tractament[vtra$Codi_Finca == "GR12A" & vtra$Any == "2015"]<- 20

vtra$Tractament[vtra$Codi_Finca == "GR13A" & vtra$Any == "2014"]<- 20
vtra$Tractament[vtra$Codi_Finca == "GR13A" & vtra$Any == "2015"]<- 20

vtra$Tractament[vtra$Codi_Finca == "GR14A" & vtra$Any == "2014"]<- 20
vtra$Tractament[vtra$Codi_Finca == "GR14A" & vtra$Any == "2015"]<- 20

vtra$Tractament[vtra$Codi_Finca == "GR15A" & vtra$Any == "2014"]<- 20
vtra$Tractament[vtra$Codi_Finca == "GR15A" & vtra$Any == "2015"]<- 20

vtra$Tractament[vtra$Codi_Finca == "GR16A" & vtra$Any == "2015"]<- 20
vtra$Tractament[vtra$Codi_Finca == "GR17A" & vtra$Any == "2015"]<- 20
vtra$Tractament[vtra$Codi_Finca == "GR19A" & vtra$Any == "2015"]<- 20

vtra$Tractament[vtra$Codi_Finca == "GR2A" & vtra$Any == "2014"]<- 20
vtra$Tractament[vtra$Codi_Finca == "GR2A" & vtra$Any == "2015"]<- 20

vtra$Tractament[vtra$Codi_Finca == "GR3A" & vtra$Any == "2014"]<- 20
vtra$Tractament[vtra$Codi_Finca == "GR3A" & vtra$Any == "2015"]<- 20

vtra$Tractament[vtra$Codi_Finca == "GR3C" & vtra$Any == "2014"]<- 20
vtra$Tractament[vtra$Codi_Finca == "GR3C" & vtra$Any == "2015"]<- 20

vtra$Tractament[vtra$Codi_Finca == "GR5A" & vtra$Any == "2014"]<- 20
vtra$Tractament[vtra$Codi_Finca == "GR5A" & vtra$Any == "2015"]<- 20

vtra$Tractament[vtra$Codi_Finca == "GR8A" & vtra$Any == "2014"]<- 20
vtra$Tractament[vtra$Codi_Finca == "GR8A" & vtra$Any == "2015"]<- 20

vtra$Tractament[vtra$Codi_Finca == "GR9A" & vtra$Any == "2014"]<- 20
vtra$Tractament[vtra$Codi_Finca == "GR9A" & vtra$Any == "2015"]<- 20


#vtra contiene todos los usos y tratamientos en primavera de parcelas muestreadas 3 años.
#De momento en las parcelas de alfalfa con algún tratamiento (e.g. labrar) se consideran como
 #tratamiento "alfalfa"
#Añadir tratamiento "alfalfa" y tratamiento "control" donde no hay tratamiento

vtra$Tractament[vtra$US == "2"]<-"11" #Tratamiento "alfalfa"

#Añadir tratamiento 21 "Control" para los que no tienen tratamiento en barbecho (CR) 
levels(vtra$Tractament)<-c(levels(vtra$Tractament),"21")

vtra$Tractament[is.na(vtra$Tractament)]<- "21"

#Make the data nice and save file
vtra<-vtra[,-c(10:13)]
vtra<-vtra[,c(2,9,7,8,3:6,10)]
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

vtra$Tractament[vtra$Tractament == "Herbicidar"] <- "Picar i herbicidar"
vtra$Tractament<-as.factor(vtra$Tractament)
levels(vtra$Tractament) #9 tratamientos

#Guardar relación tratamiento-año-finca
vtra<-vtra[,-c(6:8)]
write.csv(vtra,file = "Fincas-tratamientos.csv")
