
#Frecuencia de aparición de especies en fincas por año#

rm(list=ls())
setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")
ab<-read.csv("EvolucioOcells_abundance.csv",sep = ";",header=TRUE,fill = TRUE)

#PROPORTION (Get number of fincas each year)
p<-ab[which(ab$Any == "2014" & ab$Contatge != "99"),] #151 fincas 2014
unique(p$Codi_Finca)
w<-ab[which(ab$Any == "2015" & ab$Contatge != "99"),] #332 fincas 2015
unique(w$Codi_Finca)
t<-ab[which(ab$Any == "2016" & ab$Contatge != "99"),] #341 fincas 2016
unique(t$Codi_Finca)


#TERAX_F
tef<-ab[ab$EspecieObj == "TERAX_F",c(1:7)]
n<-which(tef$Contatge == 99) # Delete the years not sampled
tef<-tef[- n,]
tef<-tef[which(tef$Contatge > 0),] #Keep only the fincas where it is detected

tf<-table(tef$Codi_Finca,tef$Any)
tf<-as.data.frame(tf)
colnames(tf)<-c("Codi_Finca","Any","Freq")
tf<-aggregate(Freq~Any, sum, data=tf)
tf$Prop[1] <- tf$Freq[1]/151*100
tf$Prop[2] <- tf$Freq[2]/332*100
tf$Prop[3] <- tf$Freq[3]/341*100
freq_tf<-as.vector(tf$Prop)

barplot(freq_tf,names.arg = c("2014","2015","2016"),main = "Frecuencia TERAX_F en fincas",ylim = c(0,25))

#TERAX_M
tem<-ab[ab$EspecieObj == "TERAX_M",c(1:7)]
n<-which(tem$Contatge == 99) # Delete the years not sampled
tem<-tem[- n,]
tem<-tem[which(tem$Contatge > 0),] #Keep only the fincas where it is detected

tm<-table(tem$Codi_Finca,tem$Any)
tm<-as.data.frame(tm)
colnames(tm)<-c("Codi_Finca","Any","Freq")
tm<-aggregate(Freq~Any, sum, data=tm)
tm$Prop[1] <- tm$Freq[1]/151*100
tm$Prop[2] <- tm$Freq[2]/332*100
tm$Prop[3] <- tm$Freq[3]/341*100
freq_tm<-as.vector(tm$Prop)

barplot(freq_tm,names.arg = c("2014","2015","2016"),main = "Frecuencia TERAX_M en fincas",ylim = c(0,90))

#CABRA
cab<-ab[ab$EspecieObj == "CABRA",c(1:7)]
n<-which(cab$Contatge == 99) # Delete the years not sampled
cab<-cab[- n,]
cab<-cab[which(cab$Contatge > 0),] #Keep only the fincas where it is detected

c<-table(cab$Codi_Finca,cab$Any)
c<-as.data.frame(c)
colnames(c)<-c("Codi_Finca","Any","Freq")
c<-aggregate(Freq~Any, sum, data=c)
c$Prop[1] <- c$Freq[1]/151*100
c$Prop[2] <- c$Freq[2]/332*100
c$Prop[3] <- c$Freq[3]/341*100
freq_c<-as.vector(c$Prop)

barplot(freq_c,names.arg = c("2014","2015","2016"),main = "Frecuencia CABRA en fincas",ylim = c(0,30))


#MECAL
mec<-ab[ab$EspecieObj == "MECAL",c(1:7)]
n<-which(mec$Contatge == 99) # Delete the years not sampled
mec<-mec[- n,]
mec<-mec[which(mec$Contatge > 0),] #Keep only the fincas where it is detected

m<-table(mec$Codi_Finca,mec$Any)
m<-as.data.frame(m)
colnames(m)<-c("Codi_Finca","Any","Freq")

m<-aggregate(Freq~Any, sum, data=m)
m$Prop[1] <- m$Freq[1]/151*100
m$Prop[2] <- m$Freq[2]/332*100
m$Prop[3] <- m$Freq[3]/341*100
freq_m<-as.vector(m$Prop)

barplot(freq_m,names.arg = c("2014","2015","2016"),main = "Frecuencia MECAL en fincas",ylim = c(0,120))

#PTALC
ptalc<-ab[ab$EspecieObj == "PTALC",c(1:7)]
n<-which(ptalc$Contatge == 99) # Delete the years not sampled
ptalc<-ptalc[- n,]
ptalc<-ptalc[which(ptalc$Contatge > 0),] #Keep only the fincas where it is detected

pa<-table(ptalc$Codi_Finca,ptalc$Any)
pa<-as.data.frame(pa)
colnames(pa)<-c("Codi_Finca","Any","Freq")

pa<-aggregate(Freq~Any, sum, data=pa)
pa$Prop[1] <- pa$Freq[1]/151*100
pa$Prop[2] <- pa$Freq[2]/332*100
pa$Prop[3] <- pa$Freq[3]/341*100
freq_pa<-as.vector(pa$Prop)

barplot(freq_pa,names.arg = c("2014","2015","2016"),main = "Frecuencia PTALC en fincas",ylim = c(0,20))

#PTORI
ptori<-ab[ab$EspecieObj == "PTORI",c(1:7)]
n<-which(ptori$Contatge == 99) # Delete the years not sampled
ptori<-ptori[- n,]
ptori<-ptori[which(ptori$Contatge > 0),] #Keep only the fincas where it is detected

po<-table(ptori$Codi_Finca,ptori$Any)
po<-as.data.frame(po)
colnames(po)<-c("Codi_Finca","Any","Freq")

po<-aggregate(Freq~Any, sum, data=po)

po$Prop[1] <- po$Freq[1]/151*100
po$Prop[2] <- po$Freq[2]/332*100
po$Prop[3] <- po$Freq[3]/341*100
freq_po<-as.vector(po$Prop)
freq_po<-union(c(0),freq_po)

barplot(freq_po,names.arg = c("2014","2015","2016"),main = "Frecuencia PTORI en fincas",ylim = c(0,10))

#BUOED
buo<-ab[ab$EspecieObj == "BUOED",c(1:7)]
n<-which(buo$Contatge == 99) # Delete the years not sampled
buo<-buo[- n,]
buo<-buo[which(buo$Contatge > 0),] #Keep only the fincas where it is detected

b<-table(buo$Codi_Finca,buo$Any)
b<-as.data.frame(b)
colnames(b)<-c("Codi_Finca","Any","Freq")

b<-aggregate(Freq~Any, sum, data=b)
b$Prop[1] <- b$Freq[1]/151*100
b$Prop[2] <- b$Freq[2]/332*100
b$Prop[3] <- b$Freq[3]/341*100
freq_b<-as.vector(b$Prop)

barplot(freq_b,names.arg = c("2014","2015","2016"),main = "Frecuencia BUOED en fincas",ylim = c(0,70))

#GASSP
gas<-ab[ab$EspecieObj == "GASSP",c(1:7)]
n<-which(gas$Contatge == 99) # Delete the years not sampled
gas<-gas[- n,]
gas<-gas[which(gas$Contatge > 0),] #Keep only the fincas where it is detected

gs<-table(gas$Codi_Finca,gas$Any)
gs<-as.data.frame(gs)
colnames(gs)<-c("Codi_Finca","Any","Freq")

gs<-aggregate(Freq~Any, sum, data=gs)
gs$Prop[1] <- gs$Freq[1]/151*100
gs$Prop[2] <- gs$Freq[2]/332*100
gs$Prop[3] <- gs$Freq[3]/341*100
freq_gs<-as.vector(gs$Prop)
freq_gs<-union(c(0),freq_gs)

barplot(freq_gs,names.arg = c("2014","2015","2016"),main = "Frecuencia GASSP en fincas",ylim = c(0,15))

#GACRI
gac<-ab[ab$EspecieObj == "GACRI",c(1:7)]
n<-which(gac$Contatge == 99) # Delete the years not sampled
gac<-gac[- n,]
gac<-gac[which(gac$Contatge > 0),] #Keep only the fincas where it is detected

gc<-table(gac$Codi_Finca,gac$Any)
gc<-as.data.frame(gc)
colnames(gc)<-c("Codi_Finca","Any","Freq")

gc<-aggregate(Freq~Any, sum, data=gc)
gc$Prop[1] <- gc$Freq[1]/151*100
gc$Prop[2] <- gc$Freq[2]/332*100
gc$Prop[3] <- gc$Freq[3]/341*100
freq_gc<-as.vector(gc$Prop)

barplot(freq_gc,names.arg = c("2014","2015","2016"),main = "Frecuencia GACRI en fincas",ylim = c(0,100))

#GATHE
gat<-ab[ab$EspecieObj == "GATHE",c(1:7)]
n<-which(gat$Contatge == 99) # Delete the years not sampled
gat<-gat[- n,]
gat<-gat[which(gat$Contatge > 0),] #Keep only the fincas where it is detected

gt<-table(gat$Codi_Finca,gat$Any)
gt<-as.data.frame(gt)
colnames(gt)<-c("Codi_Finca","Any","Freq")

gt<-aggregate(Freq~Any, sum, data=gt)
gt$Prop[1] <- gt$Freq[1]/151*100
gt$Prop[2] <- gt$Freq[2]/332*100
gt$Prop[3] <- gt$Freq[3]/341*100
freq_gt<-as.vector(gt$Prop)

barplot(freq_gt,names.arg = c("2014","2015","2016"),main = "Frecuencia GATHE en fincas",ylim = c(0,120))

#LUARB
lua<-ab[ab$EspecieObj == "LUARB",c(1:7)]
n<-which(lua$Contatge == 99) # Delete the years not sampled
lua<-lua[- n,]
lua<-lua[which(lua$Contatge > 0),] #Keep only the fincas where it is detected

l<-table(lua$Codi_Finca,lua$Any)
l<-as.data.frame(l)
colnames(l)<-c("Codi_Finca","Any","Freq")

l<-aggregate(Freq~Any, sum, data=l)
l$Prop[1] <- l$Freq[1]/151*100
l$Prop[2] <- l$Freq[2]/332*100
l$Prop[3] <- l$Freq[3]/341*100
freq_l<-as.vector(l$Prop)
freq_l<-union(c(0),freq_l)

barplot(freq_l,names.arg = c("2014","2015","2016"),main = "Frecuencia LUARB en fincas",ylim = c(0,10))

#ALRUF
alr<-ab[ab$EspecieObj == "ALRUF",c(1:7)]
n<-which(alr$Contatge == 99) # Delete the years not sampled
alr<-alr[- n,]
alr<-alr[which(alr$Contatge > 0),] #Keep only the fincas where it is detected

a<-table(alr$Codi_Finca,alr$Any)
a<-as.data.frame(a)
colnames(a)<-c("Codi_Finca","Any","Freq")

a<-aggregate(Freq~Any, sum, data=a)
a$Prop[1] <- a$Freq[1]/151*100
a$Prop[2] <- a$Freq[2]/332*100
a$Prop[3] <- a$Freq[3]/341*100
freq_a<-as.vector(a$Prop)

barplot(freq_a,names.arg = c("2014","2015","2016"),main = "Frecuencia ALRUF en fincas",ylim = c(0,25))

#COGAR
cog<-ab[ab$EspecieObj == "COGAR",c(1:7)]
n<-which(cog$Contatge == 99) # Delete the years not sampled
cog<-cog[- n,]
cog<-cog[which(cog$Contatge > 0),] #Keep only the fincas where it is detected

cg<-table(cog$Codi_Finca,cog$Any)
cg<-as.data.frame(cg)
colnames(cg)<-c("Codi_Finca","Any","Freq")

cg<-aggregate(Freq~Any, sum, data=cg)
cg$Prop[1] <- cg$Freq[1]/151*100
cg$Prop[2] <- cg$Freq[2]/332*100
cg$Prop[3] <- cg$Freq[3]/341*100
freq_cg<-as.vector(cg$Prop)

barplot(freq_cg,names.arg = c("2014","2015","2016"),main = "Frecuencia COGAR en fincas",ylim = c(0,15))

#MICAL
mic<-ab[ab$EspecieObj == "MICAL",c(1:7)]
n<-which(mic$Contatge == 99) # Delete the years not sampled
mic<-mic[- n,]
mic<-mic[which(mic$Contatge > 0),] #Keep only the fincas where it is detected

mi<-table(mic$Codi_Finca,mic$Any)
mi<-as.data.frame(mi)
colnames(mi)<-c("Codi_Finca","Any","Freq")

mi<-aggregate(Freq~Any, sum, data=mi)
mi$Prop[1] <- mi$Freq[1]/151*100
mi$Prop[2] <- mi$Freq[2]/332*100
mi$Prop[3] <- mi$Freq[3]/341*100
freq_mi<-as.vector(mi$Prop)

barplot(freq_mi,names.arg = c("2014","2015","2016"),main = "Frecuencia MICAL en fincas",ylim = c(0,100))

#ALARV
ala<-ab[ab$EspecieObj == "ALARV",c(1:7)]
n<-which(ala$Contatge == 99) # Delete the years not sampled
ala<-ala[- n,]
ala<-ala[which(ala$Contatge > 0),] #Keep only the fincas where it is detected

al<-table(ala$Codi_Finca,ala$Any)
al<-as.data.frame(al)
colnames(al)<-c("Codi_Finca","Any","Freq")

al<-aggregate(Freq~Any, sum, data=al)
al$Prop[1] <- al$Freq[1]/151*100
al$Prop[2] <- al$Freq[2]/332*100
al$Prop[3] <- al$Freq[3]/341*100
freq_al<-as.vector(al$Prop)

barplot(freq_al,names.arg = c("2014","2015","2016"),main = "Frecuencia ALARV en fincas",ylim = c(0,10))

#LAMER
lam<-ab[ab$EspecieObj == "LAMER",c(1:7)]
n<-which(lam$Contatge == 99) # Delete the years not sampled
lam<-lam[- n,]
lam<-lam[which(lam$Contatge > 0),] #Keep only the fincas where it is detected

lm<-table(lam$Codi_Finca,lam$Any)
lm<-as.data.frame(lm)
colnames(lm)<-c("Codi_Finca","Any","Freq")

lm<-aggregate(Freq~Any, sum, data=lm)
lm$Prop[1] <- lm$Freq[1]/151*100
lm$Prop[2] <- lm$Freq[2]/332*100
lm$Prop[3] <- lm$Freq[3]/341*100
freq_lm<-as.vector(lm$Prop)

barplot(freq_lm,names.arg = c("2014","2015","2016"),main = "Frecuencia LAMER en fincas",ylim = c(0,10))

#LASEN
las<-ab[ab$EspecieObj == "LASEN",c(1:7)]
n<-which(las$Contatge == 99) # Delete the years not sampled
las<-las[- n,]
las<-las[which(las$Contatge > 0),] #Keep only the fincas where it is detected

ls<-table(las$Codi_Finca,las$Any)
ls<-as.data.frame(ls)
colnames(ls)<-c("Codi_Finca","Any","Freq")

ls<-aggregate(Freq~Any, sum, data=ls)
ls$Prop[1] <- ls$Freq[1]/151*100
ls$Prop[2] <- ls$Freq[2]/332*100
ls$Prop[3] <- ls$Freq[3]/341*100
freq_ls<-as.vector(ls$Prop)

barplot(freq_ls,names.arg = c("2014","2015","2016"),main = "Frecuencia LASEN en fincas",ylim = c(0,15))


#PLOT SUMMARY
library(dplyr)

#Terax_f
a1<-as.data.frame(t(tf))
colnames(a1)<-c("2014","2015","2016")
a1<-a1[-c(1,2),]
a1$sp<-"Sisón_f"

#Terax_m
b1<-as.data.frame(t(tm))
colnames(b1)<-c("2014","2015","2016")
b1<-b1[-c(1,2),]
b1$sp<-"Sisón_m"

#Cabra
c1<-as.data.frame(t(c))
colnames(c1)<-c("2014","2015","2016")
c1<-c1[-c(1,2),]
c1$sp<-"Terrera"

#Mecal
d1<-as.data.frame(t(m))
colnames(d1)<-c("2014","2015","2016")
d1<-d1[-c(1,2),]
d1$sp<-"Calandria"

#Ptalc
e1<-as.data.frame(t(pa))
colnames(e1)<-c("2014","2015","2016")
e1<-e1[-c(1,2),]
e1$sp<-"Ganga"

#Ptori
f1<-as.data.frame(t(po))
f1$V0<-as.factor(0)
f1<-f1[,c(3,1,2)]
colnames(f1)<-c("2014","2015","2016")
f1<-f1[-c(1,2),]
f1$sp<-"Ortega"

#Buoed
g1<-as.data.frame(t(b))
colnames(g1)<-c("2014","2015","2016")
g1<-g1[-c(1,2),]
g1$sp<-"Alcaraván"

#Gassp
h1<-as.data.frame(t(gs))
h1$V0<-as.factor(0)
h1<-h1[,c(3,1,2)]
colnames(h1)<-c("2014","2015","2016")
h1<-h1[-c(1,2),]
h1$sp<-"Cogujada sp."

#Gacri
i1<-as.data.frame(t(gc))
colnames(i1)<-c("2014","2015","2016")
i1<-i1[-c(1,2),]
i1$sp<-"Cogujada com"

#Gathe
j1<-as.data.frame(t(gt))
colnames(j1)<-c("2014","2015","2016")
j1<-j1[-c(1,2),]
j1$sp<-"Cogujada mont."

#Luarb
k1<-as.data.frame(t(l))
k1$V0<-as.factor(0)
k1<-k1[,c(3,1,2)]
colnames(k1)<-c("2014","2015","2016")
k1<-k1[-c(1,2),]
k1$sp<-"Alondra totov."

#Alruf
l1<-as.data.frame(t(a))
colnames(l1)<-c("2014","2015","2016")
l1<-l1[-c(1,2),]
l1$sp<-"Perdiz"

#Cogar
m1<-as.data.frame(t(cg))
colnames(m1)<-c("2014","2015","2016")
m1<-m1[-c(1,2),]
m1$sp<-"Carraca"

#Mical
n1<-as.data.frame(t(mi))
colnames(n1)<-c("2014","2015","2016")
n1<-n1[-c(1,2),]
n1$sp<-"Triguero"

#Alarv
o1<-as.data.frame(t(al))
colnames(o1)<-c("2014","2015","2016")
o1<-o1[-c(1,2),]
o1$sp<-"Alondra com."

#Lamer
p1<-as.data.frame(t(lm))
colnames(p1)<-c("2014","2015","2016")
p1<-p1[-c(1,2),]
p1$sp<-"Alcaudón real"

#Lasen
q1<-as.data.frame(t(ls))
colnames(q1)<-c("2014","2015","2016")
q1<-q1[-c(1,2),]
q1$sp<-"Alcaudón com."

s<-bind_rows(a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1)

#ESPECIES EN POCAS FINCAS

low<-s[c(5,6,8,11,13,15,16,17),-c(4)]
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
mtext(1, at = colMeans(mp), text = c("Ganga", "Ortega", "Cog sp.", "Alondra tot","Carraca", "Alondra com.",
                                     "Alc real","Alc com"), line = 1,cex = 0.9)
legend ( "topright",
         c("2014","2015","2016"),
         fill=c("gray66","gray34","gray86"),
         border = c("black", "black", "black"), bty = "n", cex = 0.7)

#ESPECIES EN MUCHAS FINCAS

high<-s[c(1,2,3,4,7,9,10,12,14),-c(4)]
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
at <- t(sapply(seq(1, nrow(low), by = 3),
               function(x) colMeans(mm[c(x, x+1), ])))

# Add the color labels for each group
mtext(1, at = colMeans(mm), text = c("Sisón_f", "Sisón_m", "Terrera", "Calandria","Alcaraván",
                                     "Cog com.","Cog mont","Perdiz","Triguero"), line = 1,cex = 0.9)
legend ( "topright",
         c("2014","2015","2016"),
         fill=c("gray66","gray34","gray86"),
         border = c("black", "black", "black"), bty = "n", cex = 0.7)


