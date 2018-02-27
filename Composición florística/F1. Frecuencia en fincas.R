
#Check frequent species with 2017 INCLUDED

#SCRIPT: Frecuencia de presencia de plantas en fincas
#1. Todos los años
#2. Cada año (2.1 tendencia especies al final del todo)
#3. Especies menos frecuentes

rm(list=ls())
setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Consultas")
fl<-read.csv("C_especies_flora_FINAL.csv",sep = ";",header=TRUE,fill = TRUE,na.strings="")

#Combinar Codi_finca
fl$Campanya_detall.Codi_finca<-as.character(fl$Campanya_detall_Codi_finca)
fl$Dades_especies_plot.Codi_finca<-as.character(fl$Dades_especies_plot_Codi_finca)
fl$Codi_Finca<-ifelse(!is.na(fl$Campanya_detall.Codi_finca),fl$Campanya_detall.Codi_finca,fl$Dades_especies_plot.Codi_finca)
fl<-fl[,-c(2,3,7,8)]

library(dplyr)
fl$Sector <- substr(fl$Codi_Finca, 0, 2)
fl$Secano<-fl$Sector
fl$Secano[fl$Sector == "AF"] <- "OCCIDENTAL"
fl$Secano[fl$Sector == "BE"] <- "ORIENTAL"
fl$Secano[fl$Sector == "BM"] <- "ORIENTAL"
fl$Secano[fl$Sector == "GR"] <- "OCCIDENTAL"
fl$Secano[fl$Sector == "SI"] <- "ORIENTAL"
fl$Secano[fl$Sector == "UT"] <- "OCCIDENTAL"
fl$Secano[fl$Sector == "AN"] <- "ORIENTAL"



#1. DF de frecuencia TODOS LOS AÑOS

#################OCCIDENTALES#############################################

fl<-fl[which(fl$Secano == "OCCIDENTAL"),]

#Alternativa 1 (Funciona, pero mejor la 2 porque te la mete en data frame)
#library(dplyr)

#ID <- unique(fl$Codi_Finca)# Object with the IDs
#fl$Codi_Finca <- as.character(fl$Codi_Finca)
#h<-list()

#for (i in 1:length(ID)){

#  tmp <- fl[fl$Codi_Finca==ID[i], ]
#  d<-distinct(tmp,Especie)
#  h[i]<-d
#}

#Alternativa 2:
#Example of what it does:
tmp <- data.frame(species= c("A","A","B"))
tab <-table(tmp$species)
dimnames(tab)[[1]][which(tab<2)]
############################################

ID <- unique(fl$Codi_Finca)# Object with the IDs
fl$Codi_Finca <- as.character(fl$Codi_Finca)
h<-list()

for (i in 1:length(ID)) {
  
  tmp <- fl[fl$Codi_Finca==ID[i], ] #Select rows with the same ID
  tab <-table(tmp$Especie)          # Frequency of species of the same finca
  d <- dimnames(tab)[[1]][which(tab > 0)] #Select species with freq > 1 
  #(then I get only the name of the ones present )
  h[[i]]<-data.frame( Especie=d, IDFINCA= rep(ID[i], length(d)))
}

f<-do.call(rbind, h )


#Especies más frecuentes en fincas (Para Joan Estrada) POR BARBECHOS
f$Presence<-1
p<-aggregate (Presence~Especie, FUN = sum , data = f)

#Nº fincas muestreadas en total = ID (364 fincas todos los años)
p$Prop<-(p$Presence/364)*100
library(dplyr)
p_oc<-arrange(p, desc(Prop))

#Select 10 with higher occurrence and barplot
p<-p[c(1:10),]
p<-as.vector(p$Prop)

barplot(p,names.arg = c("A.clavatus","E.vesicaria",
                        "P.rhoeas","B.rubens","L.rigidum",
                        "P.lagopus", "H.murinum", "S.oleraceus",
                        "B.diandrus","C.arvensis"),
        main = "OCCIDENTALES",ylim = c(0,30),ylab="% Fincas")


#################ORIENTALES#############################################


fl<-read.csv("C_Especies_flora.csv",sep = ";",header=TRUE,fill = TRUE,na.strings="")

#Combinar Codi_finca
fl$Campanya_detall.Codi_finca<-as.character(fl$Campanya_detall.Codi_finca)
fl$Dades_especies_plot.Codi_finca<-as.character(fl$Dades_especies_plot.Codi_finca)
fl$Codi_Finca<-ifelse(!is.na(fl$Campanya_detall.Codi_finca),fl$Campanya_detall.Codi_finca,fl$Dades_especies_plot.Codi_finca)
fl<-fl[,-c(2,3)]

library(dplyr)

fl$Sector <- substr(fl$Codi_Finca, 0, 2)

fl$Secano<-fl$Sector
fl$Secano[fl$Sector == "AF"] <- "OCCIDENTAL"
fl$Secano[fl$Sector == "BE"] <- "ORIENTAL"
fl$Secano[fl$Sector == "BM"] <- "ORIENTAL"
fl$Secano[fl$Sector == "GR"] <- "OCCIDENTAL"
fl$Secano[fl$Sector == "SI"] <- "ORIENTAL"
fl$Secano[fl$Sector == "UT"] <- "OCCIDENTAL"
fl$Secano[fl$Sector == "AN"] <- "ORIENTAL"
fl<-fl[which(fl$Secano == "ORIENTAL"),]



ID <- unique(fl$Codi_Finca)# Object with the IDs
fl$Codi_Finca <- as.character(fl$Codi_Finca)
h<-list()

for (i in 1:length(ID)) {
  
  tmp <- fl[fl$Codi_Finca==ID[i], ] #Select rows with the same ID
  tab <-table(tmp$Especie)          # Frequency of species of the same finca
  d <- dimnames(tab)[[1]][which(tab > 0)] #Select species with freq > 1 
  #(then I get only the name of the ones present )
  h[[i]]<-data.frame( Especie=d, IDFINCA= rep(ID[i], length(d)))
}

f<-do.call(rbind, h )


#Especies mas frecuentes en fincas (Para Joan Estrada) POR BARBECHOS

f$Presence<-1
p<-aggregate (Presence~Especie, FUN = sum , data = f)

#Nº fincas muestreadas en total = ID (364 fincas todos los a?os)
p$Prop<-(p$Presence/364)*100
library(dplyr)
p_or<-arrange(p, desc(Prop))

#Select 5 with higher occurrence and barplot
p<-p[c(1:10),]
p<-as.vector(p$Prop)
barplot(p,names.arg = c("P.rhoeas","A.clavatus","L.rigidum",
                        "M.sativa", "E.nasturtiifolium", "D.erucoides",
                        "B.rubens","Sonchus sp.","B.madritensis","L.serriola"),
        main = "ORIENTALES",ylim = c(0,50),ylab="% Fincas")



#############################################################################################


#2. Especies más frecuentes en fincas POR AÑO

#############TODOS LOS SECTORES##############

#Frecuencia de presencia de plantas en fincas
rm(list=ls())
setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Consultas")
fl<-read.csv("C_especies_flora_FINAL.csv",sep = ";",header=TRUE,fill = TRUE,na.strings="")

#Combinar Codi_finca
fl$Campanya_detall.Codi_finca<-as.character(fl$Campanya_detall_Codi_finca)
fl$Dades_especies_plot.Codi_finca<-as.character(fl$Dades_especies_plot_Codi_finca)
fl$Codi_Finca<-ifelse(!is.na(fl$Campanya_detall.Codi_finca),fl$Campanya_detall.Codi_finca,fl$Dades_especies_plot.Codi_finca)
fl<-fl[,-c(2,3,7,8)]

library(dplyr)

fl$Sector <- substr(fl$Codi_Finca, 0, 2)

fl$Secano<-fl$Sector
fl$Secano[fl$Sector == "AF"] <- "OCCIDENTAL"
fl$Secano[fl$Sector == "BE"] <- "ORIENTAL"
fl$Secano[fl$Sector == "BM"] <- "ORIENTAL"
fl$Secano[fl$Sector == "GR"] <- "OCCIDENTAL"
fl$Secano[fl$Sector == "SI"] <- "ORIENTAL"
fl$Secano[fl$Sector == "UT"] <- "OCCIDENTAL"
fl$Secano[fl$Sector == "AN"] <- "ORIENTAL"

#Crear columna para combinación Finca-Año
library(tidyr)
library(dplyr)


fl<-arrange(fl,Codi_Finca,Any)
fl<-unite(fl, Finca_Año, Codi_Finca, Any, sep = "_", remove = FALSE)


ID <- unique(fl$Finca_Año)# Object with the IDs
fl$Finca_Año <- as.character(fl$Finca_Año)
h<-list()

for (i in 1:length(ID)) {
  
  tmp <- fl[fl$Finca_Año==ID[i], ] #Select rows with the same ID
  tab <-table(tmp$Especie)          # Frequency of species of the same finca
  d <- dimnames(tab)[[1]][which(tab > 0)] #Select species with freq > 1 
  #(then I get only the name of the ones present )
  h[[i]]<-data.frame( Especie=d, IDFINCA= rep(ID[i], length(d)))
}

f<-do.call(rbind, h )


#Especies más frecuentes en fincas cada año
f$Presence<-1
f<-separate (f,IDFINCA,c("Codi_Finca","Any"))

#Más frecuentes en 2014
f14<-f[which(f$Any == "2014"),]
#Nº fincas muestreadas en 2014 
length(unique(f14$Codi_Finca))# 43
p<-aggregate (Presence~Especie, FUN = sum , data = f14)
p$Prop<-(p$Presence/144)*100

library(dplyr)
p14<-arrange(p, desc(Prop))

#Más frecuentes en 2015
f15<-f[which(f$Any == "2015"),]
#N? fincas muestreadas en 2015 
length(unique(f15$Codi_Finca))# 242
p<-aggregate (Presence~Especie, FUN = sum , data = f15)
p$Prop<-(p$Presence/242)*100

library(dplyr)
p15<-arrange(p, desc(Prop))


#Más frecuentes en 2016
f16<-f[which(f$Any == "2016"),]
#Nº fincas muestreadas en 2016 
length(unique(f16$Codi_Finca))# 248
p<-aggregate (Presence~Especie, FUN = sum , data = f16)
p$Prop<-(p$Presence/248)*100

library(dplyr)
p16<-arrange(p, desc(Prop))

#Más frecuentes en 2017
f17<-f[which(f$Any == "2017"),]
#Nº fincas muestreadas en 2016 
length(unique(f17$Codi_Finca))# 285
p<-aggregate (Presence~Especie, FUN = sum , data = f17)
p$Prop<-(p$Presence/285)*100

library(dplyr)
p17<-arrange(p, desc(Prop))



#############OCCIDENTALES POR AÑO##############

#Frecuencia de presencia de plantas en fincas

fl<-read.csv("C_especies_flora_FINAL.csv",sep = ";",header=TRUE,fill = TRUE,na.strings="")

#Combinar Codi_finca
fl$Campanya_detall.Codi_finca<-as.character(fl$Campanya_detall_Codi_finca)
fl$Dades_especies_plot.Codi_finca<-as.character(fl$Dades_especies_plot_Codi_finca)
fl$Codi_Finca<-ifelse(!is.na(fl$Campanya_detall.Codi_finca),fl$Campanya_detall.Codi_finca,fl$Dades_especies_plot.Codi_finca)
fl<-fl[,-c(2,3,7,8)]
library(dplyr)

fl$Sector <- substr(fl$Codi_Finca, 0, 2)

fl$Secano<-fl$Sector
fl$Secano[fl$Sector == "AF"] <- "OCCIDENTAL"
fl$Secano[fl$Sector == "BE"] <- "ORIENTAL"
fl$Secano[fl$Sector == "BM"] <- "ORIENTAL"
fl$Secano[fl$Sector == "GR"] <- "OCCIDENTAL"
fl$Secano[fl$Sector == "SI"] <- "ORIENTAL"
fl$Secano[fl$Sector == "UT"] <- "OCCIDENTAL"
fl$Secano[fl$Sector == "AN"] <- "ORIENTAL"
fl<-fl[which(fl$Secano == "OCCIDENTAL"),]

#Crear columna para combinaci?n Finca-A?o
library(tidyr)
library(dplyr)


fl<-arrange(fl,Codi_Finca,Any)
fl<-unite(fl, Finca_Año, Codi_Finca, Any, sep = "_", remove = FALSE)


ID <- unique(fl$Finca_Año)# Object with the IDs
fl$Finca_Año <- as.character(fl$Finca_Año)
h<-list()

for (i in 1:length(ID)) {
  
  tmp <- fl[fl$Finca_Año==ID[i], ] #Select rows with the same ID
  tab <-table(tmp$Especie)          # Frequency of species of the same finca
  d <- dimnames(tab)[[1]][which(tab > 0)] #Select species with freq > 1 
  #(then I get only the name of the ones present )
  h[[i]]<-data.frame( Especie=d, IDFINCA= rep(ID[i], length(d)))
}

f<-do.call(rbind, h )


#Especies más frecuentes en fincas cada año
f$Presence<-1
f<-separate (f,IDFINCA,c("Codi_Finca","Any"))

#Más frecuentes en 2014
f14<-f[which(f$Any == "2014"),]
#N? fincas muestreadas en 2014 
length(unique(f14$Codi_Finca))# 43
p<-aggregate (Presence~Especie, FUN = sum , data = f14)
p$Prop<-(p$Presence/43)*100

library(dplyr)
p14_oc<-arrange(p, desc(Prop))


#Más frecuentes en 2015
f15<-f[which(f$Any == "2015"),]
#Nº fincas muestreadas en 2015 
length(unique(f15$Codi_Finca))# 94
p<-aggregate (Presence~Especie, FUN = sum , data = f15)
p$Prop<-(p$Presence/94)*100

p15_oc<-arrange(p, desc(Prop))


#Más frecuentes en 2016
f16<-f[which(f$Any == "2016"),]
#Nº fincas muestreadas en 2016 
length(unique(f16$Codi_Finca))# 86
p<-aggregate (Presence~Especie, FUN = sum , data = f16)
p$Prop<-(p$Presence/86)*100

p16_oc<-arrange(p, desc(Prop))

#Más frecuentes en 2017
f17<-f[which(f$Any == "2017"),]
#Nº fincas muestreadas en 2017
length(unique(f17$Codi_Finca))# 125
p<-aggregate (Presence~Especie, FUN = sum , data = f17)
p$Prop<-(p$Presence/125)*100

p17_oc<-arrange(p, desc(Prop))




#############ORIENTALES POR AÑO##############

#Frecuencia de presencia de plantas en fincas

fl<-read.csv("C_especies_flora_FINAL.csv",sep = ";",header=TRUE,fill = TRUE,na.strings="")

#Combinar Codi_finca
fl$Campanya_detall.Codi_finca<-as.character(fl$Campanya_detall_Codi_finca)
fl$Dades_especies_plot.Codi_finca<-as.character(fl$Dades_especies_plot_Codi_finca)
fl$Codi_Finca<-ifelse(!is.na(fl$Campanya_detall.Codi_finca),fl$Campanya_detall.Codi_finca,fl$Dades_especies_plot.Codi_finca)
fl<-fl[,-c(2,3,7,8)]


fl$Sector <- substr(fl$Codi_Finca, 0, 2)

fl$Secano<-fl$Sector
fl$Secano[fl$Sector == "AF"] <- "OCCIDENTAL"
fl$Secano[fl$Sector == "BE"] <- "ORIENTAL"
fl$Secano[fl$Sector == "BM"] <- "ORIENTAL"
fl$Secano[fl$Sector == "GR"] <- "OCCIDENTAL"
fl$Secano[fl$Sector == "SI"] <- "ORIENTAL"
fl$Secano[fl$Sector == "UT"] <- "OCCIDENTAL"
fl$Secano[fl$Sector == "AN"] <- "ORIENTAL"
fl<-fl[which(fl$Secano == "ORIENTAL"),]

#Crear columna para combinación Finca-Año

fl<-arrange(fl,Codi_Finca,Any)
fl<-unite(fl, Finca_Año, Codi_Finca, Any, sep = "_", remove = FALSE)


ID <- unique(fl$Finca_Año)# Object with the IDs
fl$Finca_Año <- as.character(fl$Finca_Año)
h<-list()

for (i in 1:length(ID)) {
  
  tmp <- fl[fl$Finca_Año==ID[i], ] #Select rows with the same ID
  tab <-table(tmp$Especie)          # Frequency of species of the same finca
  d <- dimnames(tab)[[1]][which(tab > 0)] #Select species with freq > 1 
  #(then I get only the name of the ones present )
  h[[i]]<-data.frame( Especie=d, IDFINCA= rep(ID[i], length(d)))
}

f<-do.call(rbind, h )


#Especies más frecuentes en fincas cada año
f$Presence<-1
f<-separate (f,IDFINCA,c("Codi_Finca","Any"))

#Más frecuentes en 2014
f14<-f[which(f$Any == "2014"),]
#Nº fincas muestreadas en 2014 
length(unique(f14$Codi_Finca))# 101
p<-aggregate (Presence~Especie, FUN = sum , data = f14)
p$Prop<-(p$Presence/101)*100

p14_or<-arrange(p, desc(Prop))


#Más frecuentes en 2015
f15<-f[which(f$Any == "2015"),]
#Nº fincas muestreadas en 2015 
length(unique(f15$Codi_Finca))# 148
p<-aggregate (Presence~Especie, FUN = sum , data = f15)
p$Prop<-(p$Presence/148)*100

p15_or<-arrange(p, desc(Prop))


#Más frecuentes en 2016
f16<-f[which(f$Any == "2016"),]
#Nº fincas muestreadas en 2016 
length(unique(f16$Codi_Finca))# 162
p<-aggregate (Presence~Especie, FUN = sum , data = f16)
p$Prop<-(p$Presence/162)*100

p16_or<-arrange(p, desc(Prop))


#Más frecuentes en 2017
f17<-f[which(f$Any == "2017"),]
#Nº fincas muestreadas en 2017 
length(unique(f17$Codi_Finca))# 160
p<-aggregate (Presence~Especie, FUN = sum , data = f16)
p$Prop<-(p$Presence/160)*100

p17_or<-arrange(p, desc(Prop))



#3. Ver qué especies son las menos frecuentes

#Frecuencia de especies por año
#2014
p14_L<-p14[which(p14$Prop > 10.00),]

p14_oc_L<-p14_oc[which(p14_oc$Prop > 10.00),]
p14_or_L<-p14_or[which(p14_or$Prop > 10.00),]

p14_comb<-full_join(p14_oc_L,p14_or_L )
p14_comb_u<-unique(p14_comb$Especie)

which(p14_L$Especie %in% p14_comb_u) #Todas las especies generales están incluidas en el vector 
                                    #que combina ambas especies comunes a los dos sectores.
#BUENO: VECTOR p14_comb_u
#2015
p15_L<-p15[which(p15$Prop > 10.00),]

p15_oc_L<-p15_oc[which(p15_oc$Prop > 10.00),]
p15_or_L<-p15_or[which(p15_or$Prop > 10.00),]

p15_comb<-full_join(p15_oc_L,p15_or_L )
p15_comb_u<-unique(p15_comb$Especie)

which(p15_L$Especie %in% p15_comb_u)# Todas incluidas. Me quedo con VECTOR p15_comb_u

#2016
p16_L<-p16[which(p16$Prop > 10.00),]

p16_oc_L<-p16_oc[which(p16_oc$Prop > 10.00),]
p16_or_L<-p16_or[which(p16_or$Prop > 10.00),]

p16_comb<-full_join(p16_oc_L,p16_or_L )
p16_comb_u<-unique(p16_comb$Especie)

which(p16_L$Especie %in% p16_comb_u) # Todas incluidas. Me quedo con VECTOR p15_comb_u

#2017
p17_L<-p17[which(p17$Prop > 10.00),]

p17_oc_L<-p17_oc[which(p17_oc$Prop > 10.00),]
p17_or_L<-p17_or[which(p17_or$Prop > 10.00),]

p17_comb<-full_join(p17_oc_L,p17_or_L )
p17_comb_u<-unique(p17_comb$Especie)

which(p17_L$Especie %in% p17_comb_u) 

# Frecuencia de especies más frecuentes (>10% de fincas) combinación AÑOS 
x<-c(p14_comb_u,p15_comb_u,p16_comb_u,p17_comb_u)
u<-unique(x)
save(u,file = "Frequent_species_FINAL.RData")

#####################
#Especies más frecuentes combinando todos los años y separando occident/orient (2014-2016)
p_oc_L<-p_oc[which(p_oc$Prop > 10.00),]
p_or_L<-p_or[which(p_or$Prop > 10.00),]

p_comb<-full_join(p_oc_L,p_or_L )
p_comb_u<-unique(p_comb$Especie)

save(p_comb_u,file = "Frequent_species_allyears.RData")

#####################################################################################
#2.1***Tendencia temporal de las especies más abundantes en los tres años OCCIDENTALES (2014-2016)

#Anacyclus clavatus   
an<-f[f$Especie == "Anacyclus clavatus",]
tf<-table(an$Codi_Finca,an$Any)
tf<-as.data.frame(tf)
colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/43*100
tf$Prop[2] <- tf$Freq[2]/94*100
tf$Prop[3] <- tf$Freq[3]/86*100
freq_tf<-as.vector(tf$Prop)
a2<-as.data.frame(t(tf))
colnames(a2)<-c("2014","2015","2016")
a2<-a2[-c(1,2),]
a2$sp<-"Anacyclus clavatus"

#Eruca vesicaria
er<-f[f$Especie == "Eruca vesicaria",]
tf<-table(er$Codi_Finca,er$Any)
tf<-as.data.frame(tf)
colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/43*100
tf$Prop[2] <- tf$Freq[2]/94*100
tf$Prop[3] <- tf$Freq[3]/86*100
freq_tf<-as.vector(tf$Prop)
b2<-as.data.frame(t(tf))
colnames(b2)<-c("2014","2015","2016")
b2<-b2[-c(1,2),]
b2$sp<-"Eruca vesicaria"

#Papaver rhoeas
pap<-f[f$Especie == "Papaver rhoeas",]
tf<-table(pap$Codi_Finca,pap$Any)
tf<-as.data.frame(tf)
colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/43*100
tf$Prop[2] <- tf$Freq[2]/94*100
tf$Prop[3] <- tf$Freq[3]/86*100
freq_tf<-as.vector(tf$Prop)

c2<-as.data.frame(t(tf))
colnames(c2)<-c("2014","2015","2016")
c2<-c2[-c(1,2),]
c2$sp<-"Papaver rhoeas"

#Bromus gr. rubens 
bro<-f[f$Especie == "Bromus gr. rubens",]
tf<-table(bro$Codi_Finca,bro$Any)
tf<-as.data.frame(tf)
colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/43*100
tf$Prop[2] <- tf$Freq[2]/94*100
tf$Prop[3] <- tf$Freq[3]/86*100
freq_tf<-as.vector(tf$Prop)
d2<-as.data.frame(t(tf))
colnames(d2)<-c("2014","2015","2016")
d2<-d2[-c(1,2),]
d2$sp<-"Bromus gr. rubens"

#Lolium rigidum    
lol<-f[f$Especie == "Lolium rigidum",]
tf<-table(lol$Codi_Finca,lol$Any)
tf<-as.data.frame(tf)
colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/43*100
tf$Prop[2] <- tf$Freq[2]/94*100
tf$Prop[3] <- tf$Freq[3]/86*100
freq_tf<-as.vector(tf$Prop)
e2<-as.data.frame(t(tf))
colnames(e2)<-c("2014","2015","2016")
e2<-e2[-c(1,2),]
e2$sp<-"Lolium rigidum"

#Plantago lagopus   
pl<-f[f$Especie == "Plantago lagopus",]
tf<-table(pl$Codi_Finca,pl$Any)
tf<-as.data.frame(tf)
colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/43*100
tf$Prop[2] <- tf$Freq[2]/94*100
tf$Prop[3] <- tf$Freq[3]/86*100
freq_tf<-as.vector(tf$Prop)
f2<-as.data.frame(t(tf))
colnames(f2)<-c("2014","2015","2016")
f2<-f2[-c(1,2),]
f2$sp<-"Plantago lagopus"

#Hordeum murinum subsp. leporinum  
ho<-f[f$Especie == "Hordeum murinum subsp. leporinum",]
tf<-table(ho$Codi_Finca,ho$Any)
tf<-as.data.frame(tf)
colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/43*100
tf$Prop[2] <- tf$Freq[2]/94*100
tf$Prop[3] <- tf$Freq[3]/86*100
freq_tf<-as.vector(tf$Prop)
g2<-as.data.frame(t(tf))
colnames(g2)<-c("2014","2015","2016")
g2<-g2[-c(1,2),]
g2$sp<-"Hordeum murinum subsp. leporinum "

#Sonchus oleraceus 
so<-f[f$Especie == "Sonchus oleraceus",]
tf<-table(so$Codi_Finca,so$Any)
tf<-as.data.frame(tf)
colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/43*100
tf$Prop[2] <- tf$Freq[2]/94*100
tf$Prop[3] <- tf$Freq[3]/86*100
freq_tf<-as.vector(tf$Prop)
h2<-as.data.frame(t(tf))
h2$V0<-as.factor(0)
h2<-h2[,c(3,1,2)]
colnames(h2)<-c("2014","2015","2016")
h2<-h2[-c(1,2),]
h2$sp<-"Sonchus oleraceus"

#Bromus diandrus 
bd<-f[f$Especie == "Bromus diandrus",]
tf<-table(bd$Codi_Finca,bd$Any)
tf<-as.data.frame(tf)
colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/43*100
tf$Prop[2] <- tf$Freq[2]/94*100
tf$Prop[3] <- tf$Freq[3]/86*100
freq_tf<-as.vector(tf$Prop)
i2<-as.data.frame(t(tf))
i2$V0<-as.factor(0)
i2<-i2[,c(3,1,2)]
colnames(i2)<-c("2014","2015","2016")
i2<-i2[-c(1,2),]
i2$sp<-"Bromus diandrus"

#Convolvulus arvensis 
co<-f[f$Especie == "Convolvulus arvensis",]
tf<-table(co$Codi_Finca,co$Any)
tf<-as.data.frame(tf)
colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/43*100
tf$Prop[2] <- tf$Freq[2]/94*100
tf$Prop[3] <- tf$Freq[3]/86*100
freq_tf<-as.vector(tf$Prop)
j2<-as.data.frame(t(tf))
colnames(j2)<-c("2014","2015","2016")
j2<-j2[-c(1,2),]
j2$sp<-"Convolvulus arvensis"

s<-bind_rows(a2,b2,c2,d2,e2,f2,g2,h2,i2,j2)
s$`2014`<-as.numeric(s$`2014`)
s$`2015`<-as.numeric(s$`2015`)
s$`2016`<-as.numeric(s$`2016`)
s<-s[,-c(4)]
s<-t(s)
s<-as.matrix(s)

mm <- barplot(s, beside = T,
              space = c(0.3,1.5),
              col = c("gray66","gray34","gray86"),
              axisnames = FALSE, xlab = "Especies", ylab = "% Fincas",
              main = "Tendencia temporal OCCIDENTALES",
              ylim = c(0,80))


# Get the midpoints of each sequential pair of bars
# within each of the four groups
at <- t(sapply(seq(1, nrow(s), by = 3),
               function(x) colMeans(mm[c(x, x+1), ])))

# Add the color labels for each group
mtext(1, at = colMeans(mm), text = c("A. clavatus", "E.vesicaria","P. rhoeas","B.rubens","L.rigidum",
                                     "P.lagopus","H.murinum","S.oleraceus","B.diandrus",
                                     "C.arvensis"), line = 1,cex = 0.9)
legend ( "topright",
         c("2014","2015","2016"),
         fill=c("gray66","gray34","gray86"),
         border = c("black", "black", "black"), bty = "n", cex = 0.7)


#####################################################################################
#Tendencia temporal de las especies m?s abundantes en los tres años ORIENTALES (2014-2016)
f$Presence<-1
f<-separate (f,IDFINCA,c("Codi_Finca","Any"))

#N?mero de fincas por a?o.
f14<-f[which(f$Any == "2014"),]
unique(f14$Codi_Finca)#???101

#N?mero de fincas por a?o.
f15<-f[which(f$Any == "2015"),]
unique(f15$Codi_Finca)#148

#N?mero de fincas por a?o.
f16<-f[which(f$Any == "2016"),]
#N? fincas muestreadas en 2016 
unique(f16$Codi_Finca)#162

#Papaver rhoeas
pap<-f[f$Especie == "Papaver rhoeas",]
tf<-table(pap$Codi_Finca,pap$Any)
tf<-as.data.frame(tf)
colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/101*100
tf$Prop[2] <- tf$Freq[2]/148*100
tf$Prop[3] <- tf$Freq[3]/162*100
freq_tf<-as.vector(tf$Prop)

a2<-as.data.frame(t(tf))
colnames(a2)<-c("2014","2015","2016")
a2<-a2[-c(1,2),]
a2$sp<-"Papaver rhoeas"

#Anacyclus clavatus   
an<-f[f$Especie == "Anacyclus clavatus",]
tf<-table(an$Codi_Finca,an$Any)
tf<-as.data.frame(tf)
colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/101*100
tf$Prop[2] <- tf$Freq[2]/148*100
tf$Prop[3] <- tf$Freq[3]/162*100
freq_tf<-as.vector(tf$Prop)
b2<-as.data.frame(t(tf))
colnames(b2)<-c("2014","2015","2016")
b2<-b2[-c(1,2),]
b2$sp<-"Anacyclus clavatus"

#Lolium rigidum    
lol<-f[f$Especie == "Lolium rigidum",]
tf<-table(lol$Codi_Finca,lol$Any)
tf<-as.data.frame(tf)
colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/101*100
tf$Prop[2] <- tf$Freq[2]/148*100
tf$Prop[3] <- tf$Freq[3]/162*100
freq_tf<-as.vector(tf$Prop)
c2<-as.data.frame(t(tf))
c2$V0<-as.factor(0)
c2<-c2[,c(3,1,2)]
colnames(c2)<-c("2014","2015","2016")
c2<-c2[-c(1,2),]
c2$sp<-"Lolium rigidum"

#Medicago sativa
er<-f[f$Especie == "Medicago sativa",]
tf<-table(er$Codi_Finca,er$Any)
tf<-as.data.frame(tf)
colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/101*100
tf$Prop[2] <- tf$Freq[2]/148*100
tf$Prop[3] <- tf$Freq[3]/162*100
freq_tf<-as.vector(tf$Prop)
d2<-as.data.frame(t(tf))
colnames(d2)<-c("2014","2015","2016")
d2<-d2[-c(1,2),]
d2$sp<-"Medicago sativa"

#Erucastrum nasturtiifolium   
pl<-f[f$Especie == "Erucastrum nasturtiifolium",]
tf<-table(pl$Codi_Finca,pl$Any)
tf<-as.data.frame(tf)
colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/101*100
tf$Prop[2] <- tf$Freq[2]/148*100
tf$Prop[3] <- tf$Freq[3]/162*100
freq_tf<-as.vector(tf$Prop)
e2<-as.data.frame(t(tf))
colnames(e2)<-c("2014","2015","2016")
e2<-e2[-c(1,2),]
e2$sp<-"E.nasturtiifoliums"

#D.erucoides
ho<-f[f$Especie == "Diplotaxis erucoides",]
tf<-table(ho$Codi_Finca,ho$Any)
tf<-as.data.frame(tf)
colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/101*100
tf$Prop[2] <- tf$Freq[2]/148*100
tf$Prop[3] <- tf$Freq[3]/162*100
freq_tf<-as.vector(tf$Prop)
f2<-as.data.frame(t(tf))
colnames(f2)<-c("2014","2015","2016")
f2<-f2[-c(1,2),]
f2$sp<-"Diplotaxis erucoides"

#Bromus gr. rubens 
bro<-f[f$Especie == "Bromus gr. rubens",]
tf<-table(bro$Codi_Finca,bro$Any)
tf<-as.data.frame(tf)
colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/101*100
tf$Prop[2] <- tf$Freq[2]/148*100
tf$Prop[3] <- tf$Freq[3]/162*100
freq_tf<-as.vector(tf$Prop)
g2<-as.data.frame(t(tf))
colnames(g2)<-c("2014","2015","2016")
g2<-g2[-c(1,2),]
g2$sp<-"Bromus gr. rubens"


#Sonchus sp.
so<-f[f$Especie == "Sonchus sp.",]
tf<-table(so$Codi_Finca,so$Any)
tf<-as.data.frame(tf)
colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/101*100
tf$Prop[2] <- tf$Freq[2]/148*100
tf$Prop[3] <- tf$Freq[3]/162*100
freq_tf<-as.vector(tf$Prop)
h2<-as.data.frame(t(tf))
colnames(h2)<-c("2014","2015","2016")
h2<-h2[-c(1,2),]
h2$sp<-"Sonchus sp."

#Bromus madritensis
bd<-f[f$Especie == "Bromus madritensis",]
tf<-table(bd$Codi_Finca,bd$Any)
tf<-as.data.frame(tf)
colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/101*100
tf$Prop[2] <- tf$Freq[2]/148*100
tf$Prop[3] <- tf$Freq[3]/162*100
freq_tf<-as.vector(tf$Prop)
i2<-as.data.frame(t(tf))
colnames(i2)<-c("2014","2015","2016")
i2<-i2[-c(1,2),]
i2$sp<-"Bromus madritensis"

#Lactuca serriola
co<-f[f$Especie == "Convolvulus arvensis",]
tf<-table(co$Codi_Finca,co$Any)
tf<-as.data.frame(tf)
colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/101*100
tf$Prop[2] <- tf$Freq[2]/148*100
tf$Prop[3] <- tf$Freq[3]/162*100
freq_tf<-as.vector(tf$Prop)
j2<-as.data.frame(t(tf))
colnames(j2)<-c("2014","2015","2016")
j2<-j2[-c(1,2),]
j2$sp<-"Lactuca serriola"

s<-bind_rows(a2,b2,c2,d2,e2,f2,g2,h2,i2,j2)
s$`2014`<-as.numeric(s$`2014`)
s$`2015`<-as.numeric(s$`2015`)
s$`2016`<-as.numeric(s$`2016`)
s<-s[,-c(4)]
s<-t(s)
s<-as.matrix(s)

mm <- barplot(s, beside = T,
              space = c(0.3,1.5),
              col = c("gray66","gray34","gray86"),
              axisnames = FALSE, xlab = "Especies", ylab = "% Fincas",
              main = "Tendencia temporal ORIENTALES",
              ylim = c(0,80))

# Get the midpoints of each sequential pair of bars
# within each of the four groups
at <- t(sapply(seq(1, nrow(s), by = 3),
               function(x) colMeans(mm[c(x, x+1), ])))

# Add the color labels for each group
mtext(1, at = colMeans(mm), text = c( "P. rhoeas","A. clavatus","L. rigidum","M.sativa",
                                      "E.nasturtiifolium","D.erucoides","B. rubens",
                                      "Sonchus sp.","B.madritensis","L.serriola"), line = 1,cex = 0.9)
legend ( "topright",
         c("2014","2015","2016"),
         fill=c("gray66","gray34","gray86"),
         border = c("black", "black", "black"), bty = "n", cex = 0.7)

