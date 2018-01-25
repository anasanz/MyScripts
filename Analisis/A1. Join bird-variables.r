
#Join data with variables

rm(list=ls())

library(dplyr)

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")
ab<-read.csv("Birddata_abundance.csv",sep = ";",header=TRUE,fill = TRUE) #birddata_abundance es el archivo más reciente de abundancia (9/10)

#FIELD SCALE#

#Select target species and vegetation cover variables 2015 and 2016
ab<-ab[which(ab$EspecieObj %in% c("CABRA","MECAL","TERAX","TERAX_F","TERAX_M","BUOED","PTORI","PTALC","ALRUF","COGAR")),c(1,2,3,5,6,7,11,12,13)]
ab<-ab[which(ab$Any != "2014"),] 

#Join variables floristic composition
setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")
tip<-read.csv("Cluster_tipo_barbecho.csv",sep = ",",header=TRUE,fill = TRUE)
tip<-tip[,c(3,4,5)]
ab<-left_join(ab,tip)

sim<-read.csv("Simpson_plant_diversity.csv",sep = ",",header=TRUE,fill = TRUE)
sim<-sim[,c(2,3,4)]
ab<-left_join(ab,sim)

#Join heterogeneity index
het<-read.csv("Fallow_heterogeneity.csv",sep = ",",header=TRUE,fill = TRUE)
het<-het[,c(4,6,17)]
ab<-left_join(ab,het)

#Field size and shape
ar<-read.csv("Allfincas_area_shape.csv",sep = ",",header=TRUE,fill = TRUE)
ar<-ar[,c(3,4,5,6)]
ab<-left_join(ab,ar)


#TBL
a<-read.csv("TBL_2015_b200.csv",sep = ",",header=TRUE,fill = TRUE)
colnames(a)[4]<-"TBL_200"
a<-a[,c(2,3,4)]
b<-read.csv("TBL_2016_b200.csv",sep = ",",header=TRUE,fill = TRUE)
colnames(b)[4]<-"TBL_200"
b$Any<-"2016"
b<-b[,c(2,3,4)]
c<-rbind(a,b)
c$Any<-as.integer(c$Any)

ab<-left_join(ab,c)

a<-read.csv("TBL_2015_b500.csv",sep = ",",header=TRUE,fill = TRUE)
colnames(a)[4]<-"TBL_500"
a<-a[,c(2,3,4)]
b<-read.csv("TBL_2016_5200.csv",sep = ",",header=TRUE,fill = TRUE)
colnames(b)[4]<-"TBL_500"
b<-b[,c(2,3,4)]
c<-rbind(a,b)
c$Any<-as.integer(c$Any)

ab<-left_join(ab,c)

#PAR
a<-read.csv("PAR_2015_b200.csv",sep = ",",header=TRUE,fill = TRUE)
colnames(a)[4]<-"PAR_200"
a<-a[,c(2,3,4)]
b<-read.csv("PAR_2016_b200.csv",sep = ",",header=TRUE,fill = TRUE)
colnames(b)[4]<-"PAR_200"
b<-b[,c(2,3,4)]
c<-rbind(a,b)
c$Any<-as.integer(c$Any)

ab<-left_join(ab,c)

a<-read.csv("PAR_2015_b500.csv",sep = ",",header=TRUE,fill = TRUE)
colnames(a)[4]<-"PAR_500"
a<-a[,c(2,3,4)]
b<-read.csv("PAR_2016_b500.csv",sep = ",",header=TRUE,fill = TRUE)
colnames(b)[4]<-"PAR_500"
b<-b[,c(2,3,4)]
c<-rbind(a,b)
c$Any<-as.integer(c$Any)

ab<-left_join(ab,c)

#Join PTALC and PTORI = SAND

ab$CF_A<-paste(ab$Codi_Finca,ab$Any,sep = "_")
ID <- unique(ab$CF_A)
h <- list()

for( i in 1:length(ID)){
  tmp <- ab[ab$CF_A==ID[i], ]
  pt <- tmp[which(tmp$EspecieObj %in% c("PTALC","PTORI")),]
  s<-sum (pt$Contatge)
  h[i]<-s
}
df<-do.call(rbind.data.frame, h)
df<-data.frame(ID,df$c.0L..198L..0L..198L..2L..0L..198L..198L..0L..0L..0L..0L..198L..)
colnames(df)[2]<-"Cont_pt"
colnames(df)[1]<-"CF_A"

ab<-left_join(ab,df)
ab<-ab[,c(1:4,19,5,6,20,7:18)] #ªVariable Cont_pt es suma de PTALC y PTORI

#TREATMENTS
tr<-read.csv("TODOS_TRATAMIENTOS.csv",sep = ",",header=TRUE,fill = TRUE)
tr$CF_A<-paste(tr$Codi_Finca,tr$Any,sep = "_")
tr<-tr[,which(colnames(tr) %in% c("Tractament","CF_A"))]

ab<-left_join(ab,tr)

#FALLOW AGE: Expressed as the number of consecutive years a field has been kept as fallow
f<-read.csv("Fallow_age.csv",sep = ",",header=TRUE,fill = TRUE)

 #Join 2015
 f$Any<-2015
 g<-f[,c(2,10,7)]
 colnames(g)[1]<-"IDfinca"
 a<-left_join(ab,g)
 a$Age_1415[which(is.na(a$Age_1415))]<-0
 #Join 2016
 f$Any<-2016
 g<-f[,c(2,10,9)]
 colnames(g)[1]<-"IDfinca"
 a<-left_join(a,g)
 
 a$age<-a$Age_1516
 for (i in 1:13620){
   if (is.na(a$age)[i]) {
     a$age [i]<-a$Age_1415 [i]
   } else {a$age [i]}
 }
 
 a<-a[,c(1,6,4,24)]
 a[which(a$age == 5),]
 ab<-left_join(ab,a)

#COMPOSITION

#Buffer 200
a<-read.csv("comp15_200.csv",sep = ",",header=TRUE,fill = TRUE)
colnames(a)[6]<-"Fallow_200"
colnames(a)[7]<-"Tree_200"
colnames(a)[8]<-"Irri_200"
colnames(a)[2]<-"IDfinca"

b<-read.csv("comp16_200.csv",sep = ",",header=TRUE,fill = TRUE)
colnames(b)[6]<-"Fallow_200"
colnames(b)[7]<-"Tree_200"
colnames(b)[8]<-"Irri_200"
colnames(b)[2]<-"IDfinca"

c<-rbind(a,b)
c<-c[,-c(1,5)]
c$Any<-as.integer(c$Any)

ab<-left_join(ab,c)

#Buffer 500
a<-read.csv("comp15_500.csv",sep = ",",header=TRUE,fill = TRUE)
colnames(a)[6]<-"Fallow_500"
colnames(a)[7]<-"Tree_500"
colnames(a)[8]<-"Irri_500"
colnames(a)[2]<-"IDfinca"

b<-read.csv("comp16_500.csv",sep = ",",header=TRUE,fill = TRUE)
colnames(b)[6]<-"Fallow_500"
colnames(b)[7]<-"Tree_500"
colnames(b)[8]<-"Irri_500"
colnames(b)[2]<-"IDfinca"

c<-rbind(a,b)
c<-c[,-c(1,5)]
c$Any<-as.integer(c$Any)

ab<-left_join(ab,c)

# Include zone and sub-zone

#Subzone
ab$Subzone<-ab$Sector
ab$Subzone[ab$Subzone == 4] <- "AF"
ab$Subzone[ab$Subzone == 2] <- "BE"
ab$Subzone[ab$Subzone == 3] <- "BM"
ab$Subzone[ab$Subzone == 5] <- "GR"
ab$Subzone[ab$Subzone == 1] <- "SI"
ab$Subzone[ab$Subzone == 10] <- "UT"

#Zone
ab$Zone<-ab$Sector
ab$Zone[ab$Sector == 4] <- "OCCIDENTAL"
ab$Zone[ab$Sector == 2] <- "ORIENTAL"
ab$Zone[ab$Sector == 3] <- "ORIENTAL"
ab$Zone[ab$Sector == 5] <- "OCCIDENTAL"
ab$Zone[ab$Sector == 1] <- "ORIENTAL"
ab$Zone[ab$Sector == 10] <- "OCCIDENTAL"

#TROPHIC AVAILABILITY

#Ortopter
or<-read.csv(file = "or_biomass.csv", header = TRUE, sep = ",")
or$CF_A<-paste(or$Codi_Finca,or$Any, sep = "_")
or<-or[,which(colnames(or) %in% c("CF_A", "biom"))]

ab<-left_join(ab,or)

#LAI & SAI
av<-read.csv(file = "av_index.csv",header = TRUE,sep = ",")
av$CF_A<-av$X
av<-av[,which(colnames(av) %in% c("CF_A","LAI_sd","SAI_sd"))]

ab<-left_join(ab,av)

write.csv(ab,"Variables.csv")


