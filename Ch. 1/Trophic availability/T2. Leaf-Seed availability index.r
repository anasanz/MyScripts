
library(tidyr)
library(data.table)
library (dplyr)

#LEAF AVAILABILITY INDEX

setwd("C:/Users/ana.sanz/OneDrive/PhD/First chapter/Datos/Datos barbechos arrendados/Consultas")

#Each plant species will be given a value (INDEX) per FIELD: Cover x Height x SLA

#1. COVER: I take the average cover per field (average of the three plots)
fl<-read.csv("Cover_traits_FINAL.csv",sep = ",",header=TRUE,fill = TRUE,na.strings="")
fl<-fl[which(fl$Any != "2014"),] 
fl$X<-paste(fl$Codi_Finca, fl$Any, sep = "_")

#Selected species that I will use to build the index 
#(described in T0: Mas frecuentes, y de las menos, aquellas que tienen mas de un 20% de cobertura en alguna en alguna finca)


#2. HEIGHT
#Dato campo:
ab<-read.csv("Birddata_abundance_FINAL.csv",sep = ";",header=TRUE,fill = TRUE) #birddata_abundance es el archivo mas reciente de abundancia (9/10)
ab<-ab[which(ab$especieObjectiu %in% c("BUOED")),c(1,2,3,5,6,7,11,12,13)]
ab<-ab[which(ab$Any != "2014"),] 

ab <- ab[-c(which(rownames(ab) == 10329), which(rownames(ab) == 12162), which(rownames(ab) == 12201)), ] #Delete duplicates malos 2017

hei<-ab[,c(3,4,9)]
hei$X<-paste(hei$Codi_Finca,hei$Any, sep = "_")
hei<-hei[,c(4,3)]
hei$PromigAltura1Plot[hei$PromigAltura1Plot == 99]<-NA
hei<-hei[complete.cases(hei),]

#Unir la altura al cover y eliminar fincas que tienen NA en alguno de los dos
un<-full_join(hei, fl)
un<-un[complete.cases(un),]
un<-un[-which(duplicated(un)),]
cov<-un[,-c(2:5)] #COVER OF ALL SPECIES



#Dato bibliogr?fico para cada especie
g<-read.csv("Species_traits_full_FINAL.csv")
v<-names(fl)[which(names(fl) %in% g$V1)] #Vector with the columns of the species that have information
data_av<-g[which(g$V1 %in% v), ] #SPECIES WITH DATA AVAILABLE
dat<-data_av[,c(7,4)]
#Select the height of the plant (if the height of the plant is higher than the plot, keep the plot)
alt<-data.frame(matrix(nrow = 775, ncol = 85))
colnames(alt)<-dat$V1

h<-list()

for (i in 1:775){
  
  for (j in 1:85){
    if (un$PromigAltura1Plot[i] > dat$Ave_height[j]) {h[j]<-dat$Ave_height[j]}
    else {h[j]<-un$PromigAltura1Plot[i]}
  }
  alt[i,]<-do.call(rbind,h)
}

alt$X<-un$X
alt<-alt[,c(86,1:85)] #HEIGHT OF ALL PLANTS


#LEAF AVAILABILITY INDEZ
#3. SLA
sla<-data_av[,c(7,3)]

#Same columns
cov<-cov[,which(colnames(cov) %in% colnames(alt))]
setcolorder(cov, colnames(alt)) 
colnames(alt) == colnames(cov)


#Same rows
row.names(alt) <- alt$X 
row.names(cov) <- cov$X
rownames(alt) == rownames(cov)

cov<-cov[,-c(1)]
alt<-alt[,-c(1)]
#INDEX = fl (cover) x df(alt) x sla. 
mul<-cov*alt
y <-sweep(mul,MARGIN = 2,sla$SLA,"*") #Multiplies by column
y$LAI<-rowSums(y)


#SEED AVAILABILITY INDEX
#3. SEED
seed<-data_av[,c(7,6,5)]

u <-sweep(mul,MARGIN = 2,seed$Seed_mass,"*") #Multiplies by column seed mas
i<-sweep(u,MARGIN = 2,seed$Months_Bloom_AES, "*") #Weighted by flowering period
y$SAI<-rowSums(i[,1:76])

y$LAI_sd<-scale(y$LAI)
y$SAI_sd<-scale(y$SAI)

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")
write.csv(y, file = "av_index_FINAL.csv")







