

#???Comparación vegetación para tratamientos en primavera e invierno

#Tratamientos de invierno

fin<-read.csv("Fincas-tratamientos TODOS prov.csv",sep = ",",
              header=TRUE,fill = TRUE) 

hiv<-read.csv("C_Tratamientos_Fincas.csv",sep = ";",
               header=TRUE,fill = TRUE) 
hiv<-hiv[which(hiv$Estacio == "Hivern"),]

#Unir tratamientos primavera
hiv$Temporada<-as.character(hiv$Temporada)
hiv$Temporada[hiv$Temporada == "2013-2014"]<-"2014"
hiv$Temporada[hiv$Temporada == "2014-2015"]<-"2015"
hiv$Temporada[hiv$Temporada == "2015-2016"]<-"2016"
names(hiv)[names(hiv) == 'Temporada'] <- 'Any'

#Mirar qué tratamientos son
hiv$Tractament[hiv$Tractament == "6"] <- "Llaurar"
hiv$Tractament[hiv$Tractament == "4"] <- "Triturat de restes vegetals"
hiv$Tractament[hiv$Tractament == "2"] <- "Retirada de soques"
hiv$Tractament[hiv$Tractament == "5"] <- "Herbicidar"
hiv$Tractament[hiv$Tractament == "12"] <- "Llaurar"
hiv$Tractament[hiv$Tractament == "1"] <- "Curronar"
hiv$Tractament[hiv$Tractament == "7"] <- "Llaurar"
colnames(hiv)[1]<-"codi_finca"

#Unir a recubrimientos
fin<-read.csv("C_plot_recobriment2.csv",sep = ";",
              header=TRUE,fill = TRUE) 

fin$Any<-as.character(fin$Any)
fin$codi_finca<-as.character(fin$codi_finca)
vtra<-left_join(fin,hiv,by = c("codi_finca","Any"))

vtra<-vtra[which(vtra$Estacio == "Hivern"),] #Tratamientos-recubrimiento invierno

#Cargar tratamientos primavera
t<-read.csv("TODOS_TRATAMIENTOS.csv",sep = ",",
              header=TRUE,fill = TRUE) 
g<-t[which(t$Codi_Finca %in% vtra$codi_finca),]
g$Any<-as.character(g$Any)
colnames(vtra)[2]<-"Codi_Finca"
y<-bind_rows(vtra,g)
y<-arrange(y,Codi_Finca) #Fincas con tratamientos en invierno y verano (de las que tienen tratamientos en invierno)

#Miro cuáles están duplicadas. Las que están duplicadas esque tienen tratamiento en invierno y en verano, luego el 
#de invierno no vale. Si hay muchas en esta situación no merece la pena hacer el análisis
y<-y[,c(2,3)]
length(which(duplicated(y))) #De 90 observaciones de invierno, hay sólo 27 fincas repartidas entre 2015 y 2016 que no 
                            #tienen tratamientos de verano
        