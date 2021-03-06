
library(dplyr)

#SPECIES PLANTS: VARIABLES FOR LEAF/SEED AVAILABILITY INDEX
setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Consultas")
fl<-read.csv("Cobertura_media_flora_FINAL.csv",sep = ",",header=TRUE,fill = TRUE,na.strings="")
cov<-fl[,c(1,232,233,2:231)]

#Delete species with low occurrence (Script F1)
#Species with high occurrence in fincas (>10%) calculated for 2 sectors and each year

load("Frequent_species_FINAL.RData")
u <- data.frame(u)
colnames(u)[1] <- "Especie"
# Change codes to known ones plants
cod<-read.csv("Especies_flora_C�digos.csv",sep = ";",header=TRUE,fill = TRUE,na.strings="")
cod <- cod[, c(1,3)]
colnames(cod)[1] <- "Especie"

j <- left_join(u,cod)

load("Frequent_species.RData")
#Add Codi_Finca and Any
d<-c("Codi_Finca","Any","X")
j<-sub(" ",".",j$nomCientific)
j[2]<-"Bromus.gr..rubens"
j[9]<-"Crepis.vesicaria.subsp..taraxacifolia"
j[20]<-"Hordeum.murinum.subsp..leporinum"
j[29]<-"Chenopodium.sp..pl."
j[54]<-"Capsella.bursa.pastoris"
j<-c(j,d)


#DE LAS ESPECIES MENOS COMUNES, MIRAR CU�LES TIENEN COBERTURA DE M�S DE 20 EN ALGUNA PARCELA
no<-cov[,which(!names(cov) %in% j)]
no<-t(no)

h<-list()
for (i in 1:150){
  o<-ifelse (no[i,] > 20, 1, 0)
  s<-sum(o)
  h[[i]]<-s
}

h<-do.call(rbind,h) #N�mero de parcelas en las que tiene cada especie una cobertura mayor a 20
x<-as.data.frame(no[which(h>2), ]) #Las especies que tienen m�s de 20 en m�s de dos fincas son 21 de 150 especies

x<-rownames(x) #Juntar con la lista de especies frecuentes
j<-c(j,x)

f<-cov[,which(names(cov) %in% j)]

#Equivalencias especies
sp<-read.csv("Equivalencias especies.csv",sep = ";",header=FALSE,fill = TRUE,na.strings="") #Name species
sp$V1<-sub(" ",".",sp$V1) # Name species equal to my data
sp$V2<-sub("(.{3})(.*)","\\1 \\2",sp$V2) #Abbreviations equal to irene's data: 
#sub(un grupo con los 3 primeros separado de el resto, la separacion de espacio entre el 1� y 2� grupo)
colnames(sp)[2]<-"species"

#Traits especies
tr<-read.csv("spp_traits.csv",sep = ";",header=TRUE,fill = TRUE,na.strings="") #Traits irene 

#Join equivalencias-traits irene
g<-left_join (tr,sp)

#See match with frequent species dataset
v<-names(f)[which(names(f) %in% g$V1)] #Vector with the columns of the species that have information

data_av<-g[which(g$V1 %in% v), ] #SPECIES WITH DATA AVAILABLE

#Species left???
l<-names(f)[!names(f) %in% v] 


#See if the species left match with the missing names of irene
#Change the match -> as Irene (f=COVER ESPECIES MAS FRECUENTES)
names(f)[names(f) == "Bromus.gr..rubens"] <- "Bromus.rubens"
names(f)[names(f) == "Palenis.spinosa"] <- "Pallenis.spinosa"
names(f)[names(f) == "Galium.parisiense"] <- "Galium.parisiense"
names(f)[names(f) == "Anagallis.arvensis"] <- "Anagallis.arvensis"
names(f)[names(f) == "Capsella.bursa.pastoris"] <- "Capsella.bursa-pastoris"
names(f)[names(f) == "Sisimbrium.irio"] <- "Sisymbrium.irio"
names(f)[names(f) == "Heliotropum.europaeum"] <- "Heliotropium.europaeum"

#Fill missing names in Irene (g = IRENE)
g[61,6]<- "Erucastrum.nasturtiifolium"
g[84,6]<- "Hordeum.murinum.subsp..leporinum"
g[107,6]<- "Moricandia.arvensis"
g[142,6]<- "Sonchus.sp."
g[16,6]<- "Avena.sp."
g[70,6]<- "Fumaria.sp."
g[42,6]<- "Chenopodium.sp..pl."
g[73,6]<- "Galium.sp."
g[9,6]<- "Asphodelus.fistulosus"
g[47,6]<- "Composta.sp."
g[68,6]<- "Frankenia.pulverulenta"
g[75,6]<- "Gram�nia.sp."
g[81,6]<- "Herniaria.cinerea"
g[109,6]<- "Onopordum.corymbosum"
g[24,6]<- "Bromus.sp."
g[5,6]<-"Anagallis.arvensis"
g[72,6]<-"Galium.parisiense"
g[106,6]<-"Medicago.truncatula"
g[7,6]<-"Artemisia.herba.alba"
g[147,6]<-"Stipa.parviflora"
g[136,6]<-"Sisymbrium.sp."
g[20,6]<-"Brachypodium.distachyon"
g[149,6]<-"Suaeda..vera"
g[101,6]<-"Matricaria.recutita"
g[95,6]<-"Lygeum.spartum"
g[145,6]<-"Spergularia.rubra"
g[15,6]<-"Avena.sativa"
g[87,6]<-"Hypericum.perforatum"
g[50,6]<-"Crepis.vesicaria.subsp..taraxacifolia"
g[29,6]<-"Camphorosma.mospeliaca"
g[105,6]<-"Medicago.sp."
g[102,6]<-"Medicago.minima"
g[151,6]<-"Thymus.vulgaris"
g[34,6]<-"Carduus.sp."
g[60,6]<-"Erodium.ciconium"


v<-names(f)[which(names(f) %in% g$V1)] #Vector with the columns of the species that have information
data_av<-g[which(g$V1 %in% v), ] #SPECIES WITH DATA AVAILABLE
l<-names(f)[!names(f) %in% v] #Species left. Only 6 out of 98, Im not gonna ask for them.




#write.csv(g, file = "Species_traits_full_FINAL.csv")
#write.csv(f,file = "Cover_traits_FINAL.csv")

