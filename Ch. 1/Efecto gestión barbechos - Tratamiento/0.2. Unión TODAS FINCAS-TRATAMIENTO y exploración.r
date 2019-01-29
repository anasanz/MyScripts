
#En este script:
#1. Unificar tratamientos
#2. Sustitución tratamientos 2016 por tratamientos buenos (FINCAS ARIZA) -> Picar i Herbicidar
#3. Añadir tratamientos 2017 (Añadir PASTURADAS)
#4. Modificar tratamiento ALFALFA (Catagorizar por la abundancia de Medicago Sativa en plots)


##Data exploration treatments
rm(list=ls())
setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")
fin3<-read.csv("Fincas-tratamientos TODOS prov.csv",sep = ",",
               header=TRUE,fill = TRUE) 

fin<-fin3[which(fin3$Most == "VERDADERO"),c(2,3,7,8,9,10,11,12)]

fin$Tractament <- as.character(fin$Tractament)

fin<-fin[-which(fin$Tractament == "Pasturar" | fin$Tractament == "Sembra directa civada i veça" | 
                 fin$Tractament == "Retirada de soques" | fin$Tractament == "Retirar arbres arrancats i acopiar arrels" |
                 fin$Tractament == "Triturat de restes vegetals"),]
                  #Elimino 13 observaciones con distintos tratamientos con poca muestra 
fin$Tractament<-as.factor(fin$Tractament)

##Hay fincas el 2016 que tenían de tratamiento herbicidar y picar y no se ha añadido (ARIZA)
tra16<-read.csv("Tratamientos_buenos_2016.csv",sep = ",",
              header=TRUE,fill = TRUE) #Herb = Picar i herbicidar

tra16<-tra16[,-c(1,2,5:7)]

##Sustituir los tratamientos de 2016 por los buenos (incluidos ariza, tra16)
fin<-fin[-which(fin$Any == "2016"),]#Elimino 2016 malo

b<-full_join(fin,tra16)  #Meto 2016 bueno con ariza

b<-b[-c(790:796),]#Eliminar las 6 últimas observaciones porque son fincas que no se muestrearon en 2016

#Eliminar los usos que no sean barbecho, alfalfa o NA (Control)

b$US[is.na(b$US)]<-"1" #Convertir NA en 1

class(b$US)

b$US<-as.integer(b$US)
b <- b[b$US %in% c(1,2,4), ] #Seleccionar usos

#UNIFICAR TRATAMIENTOS
b$Tractament<-as.character(b$Tractament)
#Eliminar civ_ve
b<-b[-which(b$Tractament == "Civ_ve"),]
#Triturar -> Picar
b$Tractament[b$Tractament == "Trit"]<-"Picar"
#Herb (es picar i herbicidar de Ariza)-> Picar i herbicidar
b$Tractament[b$Tractament == "Herb"]<-"Picar i herbicidar"
#Herbicidar->Unificar con picar y herbicidar
b$Tractament[b$Tractament == "Herbicidar"]<-"Picar i herbicidar"

b<-arrange(b,Codi_Finca)

#SAVE (GOOD FILE TREATMENTS UNTIL 2016)
write.csv(b,file = "TODOS_TRATAMIENTOS_456.csv")

f<-table(b$Tractament,b$Any)

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Gráficas/Frecuencia en fincas")
x11()
pdf("Tratamientos.pdf", width=7,height = 5)
rainbowcols<-rainbow(6)
barplot(f,beside = T, ylim = c(0,200), col = rainbowcols)
legend ( "topright",
         c("Alfals","Control","Curronar","Llaurar","Picar","Picar i herbicidar"),
         fill= rainbowcols, bty = "n", cex = 0.7)

dev.off()

#ADD TREATMENTS OF 2017
library(rgdal)
g<-readOGR("C:/Users/ana.sanz/OneDrive/PhD/GIS Ana/Finques_ASG_2017 SELECCIÓN","Finques_ASG_2017_seleccio_def_OK")
g@data

setwd("C:/Users/ana.sanz/OneDrive/PhD/First chapter/Datos/Datos barbechos arrendados/Consultas")
write.table(g@data,"Treatments_2017.csv") 

d<-read.csv("Treatments_2017.csv",sep = " ",
               header=TRUE,fill = TRUE) 
d<-d[,c(2,9,12,37,38)]
colnames(d)[4]<-"Tractament"
colnames(d)[1]<-"ID"

#Cambiar tratamientos y hacerlos coincidir con nombres categorías
d$Tractament<-as.character(d$Tractament)

d$Tractament[d$Tractament == "CHISSEL + CURRO"]<-"Curronar"
d$Tractament[d$Tractament == "CHISSEL + GRADA"]<-"Llaurar"
d$Tractament[d$Tractament == "CHISSEL"]<-"Llaurar"
d$Tractament[d$Tractament == "PICAR"]<-"Picar"
d$Tractament[d$Tractament == "GRADA DISCOS"]<-"Llaurar"
d$Tractament[d$Tractament == "GRADA DE DISCOS + CURRO"]<-"Curronar"
d$Tractament[d$Tractament == "PICAR + HERB"]<-"Picar i herbicidar"
d$Tractament[d$Descr == "Alfals"]<-"Alfals"
d$Tractament[is.na(d$Tractament)]<-"Control"

possible_past <- d[which(d$Tractament == "Control" & d$Descr == "Guaret pasturable"), ]

length(which(d$Tractament == "Control" & d$Descr == "Guaret pasturable")) #Hay 50 Guaret pasturable sin tratamiento. Se han pastoreado?

#Comprobar pasturadas
p<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/Finques_ASG_2017 SELECCIÓN","Pasturades_Vinfaro")
f<-readOGR("C:/Users/ana.sanz/Documents/GIS Ana/Capes GIS","pasturades")
c<-as.data.frame(p@data)
c$pasturar<- "Pasturar"
c<-c[ ,c(3,41)]
e<-as.data.frame(f@data)
e$pasturar2<-"Pasturar"
e<-e[,c(1,5)]

#Mirar las que coinciden
library(dplyr)
join<-left_join(d,c)
join2<-left_join(join,e)

dat <- transform(join2, Past = pmax(pasturar, pasturar2, na.rm = TRUE))
dat<-dat[,-c(6,7)]
pas<-dat[which(dat$Descr_1 == "Guaret pasturable"),]
length(which(pas$Tractament == "Control" & pas$Past == "Pasturar")) 
#Hay 32 controles pastorables que se han pastoreado, asi que lo añado como tratamiento
dat$Tractament<-as.character(dat$Tractament)
dat$Past<-as.character(dat$Past)
dat$Past[dat$Tractament == "Control" & dat$Descr == "Guaret pasturable" & dat$Past == "Pasturar"]<- "Past" 
dat$Tractament[dat$Past == "Past"]<-"Pastoreada"
dat<-dat[,-c(6)]

#Unir
b<-read.csv("TODOS_TRATAMIENTOS_456.csv",sep = ",",
            header=TRUE,fill = TRUE) 
class(b$Any)
dat$Any<-2017
h<-full_join(b,dat)
h<-h[,c(2:6,8)]

#SAVE (GOOD FILE ALL TREATMENTS)
write.csv(h,file = "TODOS_TRATAMIENTOS.csv")

f<-table(h$Tractament,h$Any)

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Gráficas/Frecuencia en fincas")
x11()
pdf("Tratamientos.pdf", width=7,height = 5)
rainbowcols<-rainbow(7)
barplot(f,beside = T, ylim = c(0,200), col = rainbowcols)
legend ( "topleft",
         c("Alfals","Control","Curronar","Llaurar","Pastoreada","Picar","Picar i herbicidar"),
         fill= rainbowcols, bty = "n", cex = 0.7)

dev.off()


# Modificar ALFALFA
c<-read.csv("TODOS_TRATAMIENTOS.csv",sep = ",",
            header=TRUE,fill = TRUE)
fl<-read.csv("Cobertura_media_flora.csv",sep = ",",
             header=TRUE,fill = TRUE)

med<-as.data.frame(fl[,which(names(fl) %in% c("Codi_Finca", "Any", "Medicago.sativa"))])

dat<-left_join(c,med)

a<-dat[which(dat$Tractament == "Alfals"),]

length(which(is.na(a$Medicago.sativa)))

#Los campos con NA son campos pequeños.
#Existe correlacion entre el recubrimiento de alfalfa y el tamaño del campo? -> NO
ar<-read.csv("Finca.csv",sep = ";",
            header=TRUE,fill = TRUE)
ar<-ar[,c("Codi_Finca","area")]
d<-left_join(dat,ar)
cor<-d[which(complete.cases(d)),]
cor<-cor[which(cor$Tractament == "Alfals"),]
cor<-cor[which(cor$area < 6),]

plot(cor$area,cor$Medicago.sativa)
abline(lm(cor$Medicago.sativa~cor$area))
x<-lm(cor$Medicago.sativa~cor$area)
summary(x)

hist(dat$Medicago.sativa)
hist(dat$area)

#No hay correlación. En los campos pequeños no tiene por qué haber más o menos recubrimiento de alfalfa.
#Por lo tanto relleno los NA con el tratamiento que figura inicialmente y donde no hay NA, sustituyo
#por control si el recubrimiento de Med.sativa < 30%

hist(a$Medicago.sativa)


#También es importante la ocurrencia de la alfalfa en los plots. Crear la variable de "Ocurrencia
#en plots y comparar" (las que son de <30 pero están en más del 50% de los plots las meto?)
pr<-read.csv("Proporción_plots_flora.csv",sep = ",",
             header=TRUE,fill = TRUE)
pmed<-pr[,c("Codi_Finca","Any","Medicago.sativa")]
colnames(pmed)[3]<-"Medicago.sativa.prop"
s<-left_join(dat,pmed,by = c("Codi_Finca","Any"))
#Campos con 100% de plots ocupados y 0% de cobertura. No se consideran alfalfa si no ocupan
#más del 66.66666 % de los plots y tienen cobertura mayor al 30%. Si tienen cobertura mayor
#al 20% pero están en baja proporción de plots, el tratamiento no es alfalfa si no control.


for (i in 1:1220){
  
  if (s$Tractament [i] == "Alfals" & !is.na(s$Medicago.sativa[i]) & !is.na(s$Medicago.sativa.prop[i])){
    
   if (s$Medicago.sativa[i] < 30.00 | s$Medicago.sativa.prop[i] < 66.6666) {
      s$Tractament[i] <-"Control"
       } else {
    s$Tractament[i] 
       }
    }
}

#Los que son control y tienen alto recubrimiento de alfalfa, los cambio a alfalfa
which(s$Tractament != "Alfals" & s$Medicago.sativa > 30 & s$Medicago.sativa.prop > 66.6666) 

for (i in 1:1220){
  
  if (s$Tractament [i] == "Control" & !is.na(s$Medicago.sativa[i]) & !is.na(s$Medicago.sativa.prop[i])){
    
    if (s$Tractament[i] != "Alfals" & s$Medicago.sativa[i] > 30 & s$Medicago.sativa.prop[i] > 66.6666) {
      s$Tractament[i] <-"Alfals"
    } else {
      s$Tractament[i] 
    }
  }
}

#write.csv(s, file = "TODOS_TRATAMIENTOS.csv")

