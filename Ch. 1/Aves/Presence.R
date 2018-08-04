
rm(list=ls())
setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")
ab<-read.csv("EvolucioOcells_presence.csv",sep = ";",header=TRUE,fill = TRUE)

p<-ab[which(ab$Any == "2014" & ab$Contatge != "99"),] #151 fincas 2014
unique(p$Codi_Finca)
w<-ab[which(ab$Any == "2015" & ab$Contatge != "99"),] #332 fincas 2015
unique(w$Codi_Finca)
t<-ab[which(ab$Any == "2016" & ab$Contatge != "99"),] #341 fincas 2016
unique(t$Codi_Finca)

#Juntar estorninos (STSSP, STUNI, STVUL)

#All with the same name
ab$EspecieObj[ab$EspecieObj == "STUNI"]<-"STSSP"
ab$EspecieObj[ab$EspecieObj == "STVUL"]<-"STSSP"
#Merge them and sum the contages to detect the fincas where there is any sp
library(plyr)
names(ab)
s<-ddply(ab, .(Codi_Finca, Any, EspecieObj), summarize, Contatge = sum(Contatge))

#Join with the rest of the data
dat<-read.csv("EvolucioOcells_presence.csv",sep = ";",header=TRUE,fill = TRUE)
library(dplyr)
dat<-dat[-which(dat$EspecieObj == "STUNI" | dat$EspecieObj == "STVUL"),]
g<-dat[,c(1,2,3,5,6,8:29)]
ab<-left_join(s,g)


#Change dataset to species presence (With only 1/0)


ab$presence<-0

ab<-ab[ ,c(1:4,29,5:28)]

for (i in 1:nrow(ab)){
  if ((ab$Contatge[i] > 0) & (ab$Contatge[i] < 99)) {ab$presence[i] <- 1}
  if (ab$Contatge[i] >= 99) {ab$presence[i] <- 99}
}

ab$presence[is.na(ab$presence)]<-0
ab<-ab[ ,-c(4)]

#Save definitive bird-presence data file

write.csv(ab,file="Ocells_presence.csv")

#Lista con especies * de secanos orientales
#Seleccionar secanos orientales para comprobar qué actividad hacen las especies
#detectadas en ellos

rm(list=ls())
setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")
ab<-read.csv("Ocells_presence.csv",sep = ",",header=TRUE,fill = TRUE)

ab$Sector1<-ab$Sector
ab$Sector1[ab$Sector1 == 4] <- "AF"
ab$Sector1[ab$Sector1 == 2] <- "BE"
ab$Sector1[ab$Sector1 == 3] <- "BM"
ab$Sector1[ab$Sector1 == 5] <- "GR"
ab$Sector1[ab$Sector1 == 1] <- "SI"
ab$Sector1[ab$Sector1 == 10] <- "UT"

ab$Secano<-ab$Sector
ab$Secano[ab$Sector == 4] <- "OCCIDENTAL"
ab$Secano[ab$Sector == 2] <- "ORIENTAL"
ab$Secano[ab$Sector == 3] <- "ORIENTAL"
ab$Secano[ab$Sector == 5] <- "OCCIDENTAL"
ab$Secano[ab$Sector == 1] <- "ORIENTAL"
ab$Secano[ab$Sector == 10] <- "OCCIDENTAL"

or<-ab[ab$Secano == "ORIENTAL",]

#Seleccionar especies para las que es interesante saber si se han detectado
#volando o utilizando la finca activamente.

sp<-or[which(or$EspecieObj == "BUIBI" | or$EspecieObj == "COCOR" | or$EspecieObj == "COLIV" 
             | or$EspecieObj == "COMON" | or$EspecieObj == "COOEN" | or$EspecieObj == "COPAL"
             | or$EspecieObj == "PIPIC" | or$EspecieObj == "PYRAX" | or$EspecieObj == "STDEC"
             | or$EspecieObj == "STTUR"), ]

#Seleccionar fincas con presencia y comprobarlas en datos de campo para filtrar datos

rownames(sp) <- seq(length=nrow(sp))

check<-sp[which(sp$presence > 0 & sp$presence < 99),]

library(dplyr)
check1<-arrange(check, Any, Codi_Finca,Sector1)
datos_2016<-check1[which(check1$Any == "2016"),]
write.csv(datos_2016,file="Datos_2016.csv")


#Frecuencia de aparición de especies en fincas por año#

names(ab)
colnames(ab)[5]<-"Contatge"

#ALARV

#ALRUF

#ANCAM
ancam<-ab[ab$EspecieObj == "ANCAM",c(1:7)]
n<-which(ancam$Contatge == 99) # Delete the years not sampled
ancam<-ancam[- n,]
ancam<-ancam[which(ancam$Contatge > 0),] #Keep only the fincas where it is detected

tf<-table(ancam$Codi_Finca,ancam$Any)
tf<-as.data.frame(tf)
colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/151*100
tf$Prop[2] <- tf$Freq[2]/332*100
tf$Prop[3] <- tf$Freq[3]/341*100
freq_tf<-as.vector(tf$Prop)

a2<-as.data.frame(t(tf))
colnames(a2)<-c("2014","2015","2016")
a2<-a2[-c(1,2),]
a2$sp<-"Bisbita camp"

#ATNOC
atnoc<-ab[ab$EspecieObj == "ATNOC",c(1:7)]
n<-which(atnoc$Contatge == 99) # Delete the years not sampled
atnoc<-atnoc[- n,]
atnoc<-atnoc[which(atnoc$Contatge > 0),] #Keep only the fincas where it is detected

tf<-table(atnoc$Codi_Finca,atnoc$Any)
tf<-as.data.frame(tf)
colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/151*100
tf$Prop[2] <- tf$Freq[2]/332*100
tf$Prop[3] <- tf$Freq[3]/341*100
freq_tf<-as.vector(tf$Prop)

b2<-as.data.frame(t(tf))
b2$V0<-as.factor(0)
b2<-b2[,c(3,1,2)]
colnames(b2)<-c("2014","2015","2016")
b2<-b2[-c(1,2),]
b2$sp<-"Mochuelo"

#BUIBI
buibi<-ab[ab$EspecieObj == "BUIBI",c(1:7)]
n<-which(buibi$Contatge == 99) # Delete the years not sampled
buibi<-buibi[- n,]
buibi<-buibi[which(buibi$Contatge > 0),] #Keep only the fincas where it is detected

tf<-table(buibi$Codi_Finca,buibi$Any)
tf<-as.data.frame(tf)
colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/151*100
tf$Prop[2] <- tf$Freq[2]/332*100
tf$Prop[3] <- tf$Freq[3]/341*100
freq_tf<-as.vector(tf$Prop)

c2<-as.data.frame(t(tf))
colnames(c2)<-c("2014","2015","2016")
c2<-c2[-c(1,2),]
c2$sp<-"Garcilla"

#BUOED

#CABRA

#CACAR
cacar<-ab[ab$EspecieObj == "CACAR",c(1:7)]
n<-which(cacar$Contatge == 99) # Delete the years not sampled
cacar<-cacar[- n,]
cacar<-cacar[which(cacar$Contatge > 0),] #Keep only the fincas where it is detected

tf<-table(cacar$Codi_Finca,cacar$Any)
tf<-as.data.frame(tf)
colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/151*100
tf$Prop[2] <- tf$Freq[2]/332*100
tf$Prop[3] <- tf$Freq[3]/341*100
freq_tf<-as.vector(tf$Prop)

d2<-as.data.frame(t(tf))
colnames(d2)<-c("2014","2015","2016")
d2<-d2[-c(1,2),]
d2$sp<-"Jilguero"

#CACHL
cachl<-ab[ab$EspecieObj == "CACHL",c(1:7)]
n<-which(cachl$Contatge == 99) # Delete the years not sampled
cachl<-cachl[- n,]
cachl<-cachl[which(cachl$Contatge > 0),] #Keep only the fincas where it is detected

tf<-table(cachl$Codi_Finca,cachl$Any)
tf<-as.data.frame(tf)
colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/151*100
tf$Prop[2] <- tf$Freq[2]/332*100
tf$Prop[3] <- tf$Freq[3]/341*100
freq_tf<-as.vector(tf$Prop)

e2<-as.data.frame(t(tf))
colnames(e2)<-c("2014","2015","2016")
e2<-e2[-c(1,2),]
e2$sp<-"Verderón"

#CAINA
caina<-ab[ab$EspecieObj == "CAINA",c(1:7)]
n<-which(caina$Contatge == 99) # Delete the years not sampled
caina<-caina[- n,]
caina<-caina[which(caina$Contatge > 0),] #Keep only the fincas where it is detected

tf<-table(caina$Codi_Finca,caina$Any)
tf<-as.data.frame(tf)

colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/151*100
tf$Prop[2] <- tf$Freq[2]/332*100
tf$Prop[3] <- tf$Freq[3]/341*100
freq_tf<-as.vector(tf$Prop)

f2<-as.data.frame(t(tf))
f2$V0<-as.factor(0)
f2$V<-as.factor(0)
f2<-f2[,c(3,2,1)]
colnames(f2)<-c("2014","2015","2016")
f2<-f2[-c(1,2),]
f2$sp<-"Pardillo"

#CIJUN
cijun<-ab[ab$EspecieObj == "CIJUN",c(1:7)]
n<-which(cijun$Contatge == 99) # Delete the years not sampled
cijun<-cijun[- n,]
cijun<-cijun[which(cijun$Contatge > 0),] #Keep only the fincas where it is detected

tf<-table(cijun$Codi_Finca,cijun$Any)
tf<-as.data.frame(tf)

colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/151*100
tf$Prop[2] <- tf$Freq[2]/332*100
tf$Prop[3] <- tf$Freq[3]/341*100
freq_tf<-as.vector(tf$Prop)

g2<-as.data.frame(t(tf))
colnames(g2)<-c("2014","2015","2016")
g2<-g2[-c(1,2),]
g2$sp<-"Buitrón"

#COCOR

cocor<-ab[ab$EspecieObj == "COCOR",c(1:7)]
n<-which(cocor$Contatge == 99) # Delete the years not sampled
cocor<-cocor[- n,]
cocor<-cocor[which(cocor$Contatge > 0),] #Keep only the fincas where it is detected

tf<-table(cocor$Codi_Finca,cocor$Any)
tf<-as.data.frame(tf)

colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/151*100
tf$Prop[2] <- tf$Freq[2]/332*100
tf$Prop[3] <- tf$Freq[3]/341*100
freq_tf<-as.vector(tf$Prop)

h2<-as.data.frame(t(tf))
h2$V0<-as.factor(0)
h2<-h2[,c(3,1,2)]
colnames(h2)<-c("2014","2015","2016")
h2<-h2[-c(1,2),]
h2$sp<-"Corneja"

#COCOT

cocot<-ab[ab$EspecieObj == "COCOT",c(1:7)]
n<-which(cocot$Contatge == 99) # Delete the years not sampled
cocot<-cocot[- n,]
cocot<-cocot[which(cocot$Contatge > 0),] #Keep only the fincas where it is detected

tf<-table(cocot$Codi_Finca,cocot$Any)
tf<-as.data.frame(tf)

colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/151*100
tf$Prop[2] <- tf$Freq[2]/332*100
tf$Prop[3] <- tf$Freq[3]/341*100
freq_tf<-as.vector(tf$Prop)

i2<-as.data.frame(t(tf))
i2$V0<-as.factor(0)
i2$V<-as.factor(0)
i2<-i2[,c(3,2,1)]
colnames(i2)<-c("2014","2015","2016")
i2<-i2[-c(1,2),]
i2$sp<-"Codorniz"

#COGAR

#COLIV

coliv<-ab[ab$EspecieObj == "COLIV",c(1:7)]
n<-which(coliv$Contatge == 99) # Delete the years not sampled
coliv<-coliv[- n,]
coliv<-coliv[which(coliv$Contatge > 0),] #Keep only the fincas where it is detected

tf<-table(coliv$Codi_Finca,coliv$Any)
tf<-as.data.frame(tf)

colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/151*100
tf$Prop[2] <- tf$Freq[2]/332*100
tf$Prop[3] <- tf$Freq[3]/341*100
freq_tf<-as.vector(tf$Prop)

j2<-as.data.frame(t(tf))
colnames(j2)<-c("2014","2015","2016")
j2<-j2[-c(1,2),]
j2$sp<-"Paloma bravía"

#COMON

comon<-ab[ab$EspecieObj == "COMON",c(1:7)]
n<-which(comon$Contatge == 99) # Delete the years not sampled
comon<-comon[- n,]
comon<-comon[which(comon$Contatge > 0),] #Keep only the fincas where it is detected

tf<-table(comon$Codi_Finca,comon$Any)
tf<-as.data.frame(tf)

colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/151*100
tf$Prop[2] <- tf$Freq[2]/332*100
tf$Prop[3] <- tf$Freq[3]/341*100
freq_tf<-as.vector(tf$Prop)

k2<-as.data.frame(t(tf))
colnames(k2)<-c("2014","2015","2016")
k2<-k2[-c(1,2),]
k2$sp<-"Grajilla"

#COOEN

cooen<-ab[ab$EspecieObj == "COOEN",c(1:7)]
n<-which(cooen$Contatge == 99) # Delete the years not sampled
cooen<-cooen[- n,]
cooen<-cooen[which(cooen$Contatge > 0),] #Keep only the fincas where it is detected

tf<-table(cooen$Codi_Finca,cooen$Any)
tf<-as.data.frame(tf)

colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/151*100
tf$Prop[2] <- tf$Freq[2]/332*100
tf$Prop[3] <- tf$Freq[3]/341*100
freq_tf<-as.vector(tf$Prop)

l2<-as.data.frame(t(tf))
l2$V0<-as.factor(0)
l2<-l2[,c(3,1,2)]
colnames(l2)<-c("2014","2015","2016")
l2<-l2[-c(1,2),]
l2$sp<-"Paloma zurita"

#COPAL

copal<-ab[ab$EspecieObj == "COPAL",c(1:7)]
n<-which(copal$Contatge == 99) # Delete the years not sampled
copal<-copal[- n,]
copal<-copal[which(copal$Contatge > 0),] #Keep only the fincas where it is detected

tf<-table(copal$Codi_Finca,copal$Any)
tf<-as.data.frame(tf)

colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/151*100
tf$Prop[2] <- tf$Freq[2]/332*100
tf$Prop[3] <- tf$Freq[3]/341*100
freq_tf<-as.vector(tf$Prop)

m2<-as.data.frame(t(tf))
m2$V0<-as.factor(0)
m2<-m2[,c(3,1,2)]
colnames(m2)<-c("2014","2015","2016")
m2<-m2[-c(1,2),]
m2$sp<-"Paloma torcaz"

#GACRI

#GASSP

#GATHE

#LAMER

#LASEN

#MECAL

#MICAL

#OEHIS

oehis<-ab[ab$EspecieObj == "OEHIS",c(1:7)]
n<-which(oehis$Contatge == 99) # Delete the years not sampled
oehis<-oehis[- n,]
oehis<-oehis[which(oehis$Contatge > 0),] #Keep only the fincas where it is detected

tf<-table(oehis$Codi_Finca,oehis$Any)
tf<-as.data.frame(tf)

colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/151*100
tf$Prop[2] <- tf$Freq[2]/332*100
tf$Prop[3] <- tf$Freq[3]/341*100
freq_tf<-as.vector(tf$Prop)

n2<-as.data.frame(t(tf))
n2$V0<-as.factor(0)
n2<-n2[,c(3,1,2)]
colnames(n2)<-c("2014","2015","2016")
n2<-n2[-c(1,2),]
n2$sp<-"Collalba rubia"

#PADOM

padom<-ab[ab$EspecieObj == "PADOM",c(1:7)]
n<-which(padom$Contatge == 99) # Delete the years not sampled
padom<-padom[- n,]
padom<-padom[which(padom$Contatge > 0),] #Keep only the fincas where it is detected

tf<-table(padom$Codi_Finca,padom$Any)
tf<-as.data.frame(tf)

colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/151*100
tf$Prop[2] <- tf$Freq[2]/332*100
tf$Prop[3] <- tf$Freq[3]/341*100
freq_tf<-as.vector(tf$Prop)

o2<-as.data.frame(t(tf))
colnames(o2)<-c("2014","2015","2016")
o2<-o2[-c(1,2),]
o2$sp<-"Gorrión"

#PAMON

pamon<-ab[ab$EspecieObj == "PAMON",c(1:7)]
n<-which(pamon$Contatge == 99) # Delete the years not sampled
pamon<-pamon[- n,]
pamon<-pamon[which(pamon$Contatge > 0),] #Keep only the fincas where it is detected

tf<-table(pamon$Codi_Finca,pamon$Any)
tf<-as.data.frame(tf)

colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/151*100
tf$Prop[2] <- tf$Freq[2]/332*100
tf$Prop[3] <- tf$Freq[3]/341*100
freq_tf<-as.vector(tf$Prop)

p2<-as.data.frame(t(tf))
colnames(p2)<-c("2014","2015","2016")
p2<-p2[-c(1,2),]
p2$sp<-"Gor. molinero"

#PEPET

pepet<-ab[ab$EspecieObj == "PEPET",c(1:7)]
n<-which(pepet$Contatge == 99) # Delete the years not sampled
pepet<-pepet[- n,]
pepet<-pepet[which(pepet$Contatge > 0),] #Keep only the fincas where it is detected

tf<-table(pepet$Codi_Finca,pepet$Any)
tf<-as.data.frame(tf)

colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/151*100
tf$Prop[2] <- tf$Freq[2]/332*100
tf$Prop[3] <- tf$Freq[3]/341*100
freq_tf<-as.vector(tf$Prop)

q2<-as.data.frame(t(tf))
q2$V0<-as.factor(0)
q2<-q2[,c(1,2,3)]
colnames(q2)<-c("2014","2015","2016")
q2<-q2[-c(1,2),]
q2$sp<-"Gor. chillón"


#PIPIC

pipic<-ab[ab$EspecieObj == "PIPIC",c(1:7)]
n<-which(pipic$Contatge == 99) # Delete the years not sampled
pipic<-pipic[- n,]
pipic<-pipic[which(pipic$Contatge > 0),] #Keep only the fincas where it is detected

tf<-table(pipic$Codi_Finca,pipic$Any)
tf<-as.data.frame(tf)

colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/151*100
tf$Prop[2] <- tf$Freq[2]/332*100
tf$Prop[3] <- tf$Freq[3]/341*100
freq_tf<-as.vector(tf$Prop)

r2<-as.data.frame(t(tf))
colnames(r2)<-c("2014","2015","2016")
r2<-r2[-c(1,2),]
r2$sp<-"Urraca"

#PIVIR

pivir<-ab[ab$EspecieObj == "PIVIR",c(1:7)]
n<-which(pivir$Contatge == 99) # Delete the years not sampled
pivir<-pivir[- n,]
pivir<-pivir[which(pivir$Contatge > 0),] #Keep only the fincas where it is detected

tf<-table(pivir$Codi_Finca,pivir$Any)
tf<-as.data.frame(tf)

colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/151*100
tf$Prop[2] <- tf$Freq[2]/332*100
tf$Prop[3] <- tf$Freq[3]/341*100
freq_tf<-as.vector(tf$Prop)

s2<-as.data.frame(t(tf))
s2$V0<-as.factor(0)
s2<-s2[,c(3,1,2)]
colnames(s2)<-c("2014","2015","2016")
s2<-s2[-c(1,2),]
s2$sp<-"Pito real"

#PTALC

#PTORI

#PYRAX

pyrax<-ab[ab$EspecieObj == "PYRAX",c(1:7)]
n<-which(pyrax$Contatge == 99) # Delete the years not sampled
pyrax<-pyrax[- n,]
pyrax<-pyrax[which(pyrax$Contatge > 0),] #Keep only the fincas where it is detected

tf<-table(pivir$Codi_Finca,pivir$Any)
tf<-as.data.frame(tf)

colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/151*100
tf$Prop[2] <- tf$Freq[2]/332*100
tf$Prop[3] <- tf$Freq[3]/341*100
freq_tf<-as.vector(tf$Prop)

t2<-as.data.frame(t(tf))
t2$V0<-as.factor(0)
t2<-t2[,c(3,1,2)]
colnames(t2)<-c("2014","2015","2016")
t2<-t2[-c(1,2),]
t2$sp<-"Chova"

#	SATOR

sator<-ab[ab$EspecieObj == "SATOR",c(1:7)]
n<-which(sator$Contatge == 99) # Delete the years not sampled
sator<-sator[- n,]
sator<-sator[which(sator$Contatge > 0),] #Keep only the fincas where it is detected

tf<-table(sator$Codi_Finca,sator$Any)
tf<-as.data.frame(tf)

colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/151*100
tf$Prop[2] <- tf$Freq[2]/332*100
tf$Prop[3] <- tf$Freq[3]/341*100
freq_tf<-as.vector(tf$Prop)

u2<-as.data.frame(t(tf))
colnames(u2)<-c("2014","2015","2016")
u2<-u2[-c(1,2),]
u2$sp<-"Tarabilla"


#	SESER

seser<-ab[ab$EspecieObj == "SESER",c(1:7)]
n<-which(seser$Contatge == 99) # Delete the years not sampled
seser<-seser[- n,]
seser<-seser[which(seser$Contatge > 0),] #Keep only the fincas where it is detected

tf<-table(seser$Codi_Finca,seser$Any)
tf<-as.data.frame(tf)

colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/151*100
tf$Prop[2] <- tf$Freq[2]/332*100
tf$Prop[3] <- tf$Freq[3]/341*100
freq_tf<-as.vector(tf$Prop)

v2<-as.data.frame(t(tf))
colnames(v2)<-c("2014","2015","2016")
v2<-v2[-c(1,2),]
v2$sp<-"Verdecillo"


#	STDEC

stdec<-ab[ab$EspecieObj == "STDEC",c(1:7)]
n<-which(stdec$Contatge == 99) # Delete the years not sampled
stdec<-stdec[- n,]
stdec<-stdec[which(stdec$Contatge > 0),] #Keep only the fincas where it is detected

tf<-table(stdec$Codi_Finca,stdec$Any)
tf<-as.data.frame(tf)

colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/151*100
tf$Prop[2] <- tf$Freq[2]/332*100
tf$Prop[3] <- tf$Freq[3]/341*100
freq_tf<-as.vector(tf$Prop)

w2<-as.data.frame(t(tf))
w2$V0<-as.factor(0)
w2$V<-as.factor(0)
w2<-w2[,c(3,1,2)]
colnames(w2)<-c("2014","2015","2016")
w2<-w2[-c(1,2),]
w2$sp<-"Tórtola turca"

#STSSP

stssp<-ab[ab$EspecieObj == "STSSP",c(1:7)]
n<-which(stssp$Contatge == 99) # Delete the years not sampled
stssp<-stssp[- n,]
stssp<-stssp[which(stssp$Contatge > 0),] #Keep only the fincas where it is detected

tf<-table(stssp$Codi_Finca,stssp$Any)
tf<-as.data.frame(tf)

colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/151*100
tf$Prop[2] <- tf$Freq[2]/332*100
tf$Prop[3] <- tf$Freq[3]/341*100
freq_tf<-as.vector(tf$Prop)

x2<-as.data.frame(t(tf))
colnames(x2)<-c("2014","2015","2016")
x2<-x2[-c(1,2),]
x2$sp<-"Estornino sp."

#STTUR -> No hay! Delete

#TERAX_F

#TERAX_M

#TUMER

tumer<-ab[ab$EspecieObj == "TUMER",c(1:7)]
n<-which(tumer$Contatge == 99) # Delete the years not sampled
tumer<-tumer[- n,]
tumer<-tumer[which(tumer$Contatge > 0),] #Keep only the fincas where it is detected

tf<-table(tumer$Codi_Finca,tumer$Any)
tf<-as.data.frame(tf)

colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/151*100
tf$Prop[2] <- tf$Freq[2]/332*100
tf$Prop[3] <- tf$Freq[3]/341*100
freq_tf<-as.vector(tf$Prop)

y2<-as.data.frame(t(tf))
y2$V0<-as.factor(0)
y2<-y2[,c(3,1,2)]
colnames(y2)<-c("2014","2015","2016")
y2<-y2[-c(1,2),]
y2$sp<-"Mirlo"

#UPEPO

upepo<-ab[ab$EspecieObj == "TUMER",c(1:7)]
n<-which(upepo$Contatge == 99) # Delete the years not sampled
upepo<-upepo[- n,]
upepo<-upepo[which(upepo$Contatge > 0),] #Keep only the fincas where it is detected

tf<-table(upepo$Codi_Finca,upepo$Any)
tf<-as.data.frame(tf)

colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/151*100
tf$Prop[2] <- tf$Freq[2]/332*100
tf$Prop[3] <- tf$Freq[3]/341*100
freq_tf<-as.vector(tf$Prop)

z2<-as.data.frame(t(tf))
z2$V0<-as.factor(0)
z2<-z2[,c(3,1,2)]
colnames(z2)<-c("2014","2015","2016")
z2<-z2[-c(1,2),]
z2$sp<-"Abubilla"

s<-bind_rows(a2,b2,c2,d2,e2,f2,g2,h2,i2,
               j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2)

#ESPECIES EN MUCHAS FINCAS

high<-s[c(4,11,12,13,15,16,18,19,20,24),-c(4)]
high$`2014`<-as.numeric(high$`2014`)
high$`2015`<-as.numeric(high$`2015`)
high$`2016`<-as.numeric(high$`2016`)
high<-t(high)
high<-as.matrix(high)

mm <- barplot(high, beside = T,
              space = c(0.3,1.5),
              col = c("gray66","gray34","gray86"),
              axisnames = FALSE, xlab = "Especies", ylab = " ",
              main = "Frecuencia en fincas",
              ylim = c(0,40))

# Get the midpoints of each sequential pair of bars
# within each of the four groups
at <- t(sapply(seq(1, nrow(high), by = 3),
               function(x) colMeans(mm[c(x, x+1), ])))

# Add the color labels for each group
mtext(1, at = colMeans(mm), text = c("Jilguero", "Grajilla", "P.zurita", "P.Torcaz","Gorrión",
                                     "Gor.molinero","Urraca","Pito real","Chova","Estornino sp"), line = 1,cex = 0.9)
legend ( "topright",
         c("2014","2015","2016"),
         fill=c("gray66","gray34","gray86"),
         border = c("black", "black", "black"), bty = "n", cex = 0.7)

#ESPECIES EN POCAS FINCAS
s<-bind_rows(a2,b2,c2,d2,e2,f2,g2,h2,i2,
             j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2,u2,v2,w2,x2,y2,z2)
s<-s[-c(4,11,12,13,15,16,18,19,20,24),]

#First half
low<-s[c(1:8),-c(4)]
low$`2014`<-as.numeric(low$`2014`)
low$`2015`<-as.numeric(low$`2015`)
low$`2016`<-as.numeric(low$`2016`)
low<-t(low)
low<-as.matrix(low)

mp <- barplot(low, beside = T,
              space = c(0.3,1.5),
              col = c("gray66","gray34","gray86"),
              axisnames = FALSE, xlab = "Especies", ylab = " ",
              main = "Frecuencia en fincas",
              ylim = c(0,8))

# Get the midpoints of each sequential pair of bars
# within each of the four groups
at <- t(sapply(seq(1, nrow(low), by = 3),
               function(x) colMeans(mp[c(x, x+1), ])))

# Add the color labels for each group
mtext(1, at = colMeans(mp), text = c("Bisbita camp", "Mochuelo", "Garcilla", 
                                     "Verderón","Pardillo", "Buitrón",
                                     "Corneja","Codorniz"), line = 1,cex = 0.9)
legend ( "topright",
         c("2014","2015","2016"),
         fill=c("gray66","gray34","gray86"),
         border = c("black", "black", "black"), bty = "n", cex = 0.7)

#Second half
low<-s[c(9:16),-c(4)]
low$`2014`<-as.numeric(low$`2014`)
low$`2015`<-as.numeric(low$`2015`)
low$`2016`<-as.numeric(low$`2016`)
low<-t(low)
low<-as.matrix(low)

mp <- barplot(low, beside = T,
              space = c(0.3,1.5),
              col = c("gray66","gray34","gray86"),
              axisnames = FALSE, xlab = "Especies", ylab = " ",
              main = "Frecuencia en fincas",
              ylim = c(0,8))

# Get the midpoints of each sequential pair of bars
# within each of the four groups
at <- t(sapply(seq(1, nrow(low), by = 3),
               function(x) colMeans(mp[c(x, x+1), ])))

# Add the color labels for each group
mtext(1, at = colMeans(mp), text = c("P.bravía", "Collalba", "Gor.chillón", 
                                     "Tarabilla","Verdecillo", "Tórtula turc.",
                                     "Mirlo","Abubilla"), line = 1,cex = 0.9)
legend ( "topright",
         c("2014","2015","2016"),
         fill=c("gray66","gray34","gray86"),
         border = c("black", "black", "black"), bty = "n", cex = 0.7)