
#SCRIPT: 
    #1. Media de cobertura de cada especie en los plots para cada campo (Average of species 
        #composition in each field)
    #2. Proportion of plots occupied per species
library(tidyr)
library(dplyr)

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")
fl<-read.csv("C_Especies_flora.csv",sep = ";",header=TRUE,fill = TRUE,na.strings="")
#Combinar Codi_finca
fl$Campanya_detall.Codi_finca<-as.character(fl$Campanya_detall.Codi_finca)
fl$Dades_especies_plot.Codi_finca<-as.character(fl$Dades_especies_plot.Codi_finca)
fl$Codi_Finca<-ifelse(!is.na(fl$Campanya_detall.Codi_finca),fl$Campanya_detall.Codi_finca,fl$Dades_especies_plot.Codi_finca)
fl<-fl[,-c(2,3)]
fl<-fl[,c(5,1:4)]

#Average of each species/finca

fl$CF_A<-paste(fl$Codi_Finca,fl$Any,sep = "-")
ID <- unique(fl$CF_A)
fl$CF_A <- as.character(fl$CF_A)
fl<-droplevels(fl)
fl$Cobertura[is.na(fl$Cobertura)]<-0
#Create an empty matrix with the length of the species (wide) and fincas(long)
m <- matrix(0, ncol=length(unique(fl$Especie)), nrow=length(unique(fl$CF_A)) )
m <-data.frame(m)
colnames(m) <- unique(fl$Especie)
rownames(m) <- unique(fl$CF_A) 
#Loop
for( i in 1:length(ID)){
  
  tmp <- fl[fl$CF_A==ID[i], ]
  ag<-aggregate(Cobertura ~ Especie, data = tmp, mean)
  m[i, as.character(ag$Especie)] <- ag$Cobertura
}

m$CF_A<-rownames(m)
m<-separate(data = m, col = CF_A, into = c("Codi_Finca", "Any"), sep = "\\-")

#write.csv(m,file = "Cobertura_media_flora.csv")


#2. Proportion in plots of each species/finca

m <- matrix(0, ncol=length(unique(fl$Especie)), nrow=length(unique(fl$CF_A)) )
m <-data.frame(m)
colnames(m) <- unique(fl$Especie)
rownames(m) <- unique(fl$CF_A) 

for( i in 1:length(ID)){
  
  tmp <- fl[fl$CF_A==ID[i], ]
  ag<-aggregate( CF_A ~ Especie, data = tmp, table)
  
    for (j in 1:length(ag$CF_A)){
      
      ag$pr[j]<-ag$CF_A[j]*100/length(unique(tmp$Plot))
      
     }
  
  m[i, as.character(ag$Especie)] <- ag$pr
}

m$CF_A<-rownames(m)
m<-separate(data = m, col = CF_A, into = c("Codi_Finca", "Any"), sep = "\\-")

#???write.csv(m,file = "Proporción_plots_flora.csv")
