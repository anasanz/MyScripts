
rm(list=ls())

library(dplyr)
library(tidyr)
library(FactoMineR)
library(factoextra)
library(rgl)
library(RColorBrewer)
library(scales)
library(fpc)
library(cluster)
library(indicspecies)

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")
fl<-read.csv("Cobertura_media_flora.csv",sep = ",",header=TRUE,fill = TRUE,na.strings="")

#Delete species with low occurrence (Script F1)
#Species with high occurrence in fincas (>10%) calculated for 2 sectors and each year
load("Frequent_species.RData")
#Add Codi_Finca and Any
d<-c("Codi_Finca","Any","X")
j<-sub(" ",".",u)
j[2]<-"Bromus.gr..rubens"
j[9]<-"Crepis.vesicaria.subsp..taraxacifolia"
j[20]<-"Hordeum.murinum.subsp..leporinum"
j[29]<-"Chenopodium.sp..pl."
j[54]<-"Capsella.bursa.pastoris"
j<-c(j,d)

f<-fl[,which(names(fl) %in% j)]
f<-f[,c(1,65,66,2:64)]
p<-f[,4:66]

#1. PAM clustering method
pam1<-pamk(p,krange = 2:10,criterion = "asw",usepam = TRUE,scaling = FALSE) #↨Recommends k=7
fviz_cluster(pam$pamobject) # Cant see anything. Try with PCA

#Plot results PAM en PCA
pc <- prcomp(f[,4:66])
comp <- data.frame(pc$x[,1:4])
fviz_pca_biplot(pc, axes = c(3,4), label=" ",col.var = "black",habillage = as.factor(pam1$pamobject$clustering),
                addEllipses = TRUE) + ggtitle("") +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))
sort(table(pam1$pamobject$clustering))
#6   7   2   3   5   4   1 
#36  36  40  47  88  99 288 

f$Cluster<-pam1$pamobject$clustering
ag1<-aggregate(. ~ Cluster, f, function(x) c(mean = mean(x)))
ag2<-aggregate(. ~ Cluster, f, function(x) c(sd = sd(x)))
library(tidyr)
ag1<-t(ag1)
ag1<-ag1[-c(1:4),]
ag1<-round(ag1,digits = 3)
ag2<-t(ag2)
ag2<-ag2[-c(1:4),]
ag2<-round(ag2,digits = 3)

plot3d(comp$PC1, comp$PC2, comp$PC3, col=pam$pamobject$clustering)


#2. Cluster composition
comb<-multipatt(p, pam1$pamobject$clustering, func = "IndVal.g", duleg=FALSE, restcomb=NULL,
          min.order = 1, max.order = NULL, control=how(),
          print.perm=FALSE)
  
#Exclusive species of CL 1
one1<-data.frame(which(comb$sign$s.1 == 1 & rowSums(comb$sign[,c(1:7)]) == 1))
one2<-data.frame(which(comb$sign$s.1 == 1 & rowSums(comb$sign[,c(1:7)]) == 2))

one1$Species<-rownames(one1)
one1$Class<-"Primary"
one1<-one1[,-c(1)]
one2$Species<-rownames(one2)
one2$Class<-"Secondary"
one2<-one2[,-c(1)]

one<-bind_rows(one1,one2) #There are no primary or secondary

#Exclusive species of CL 2
two1<-data.frame(which(comb$sign$s.2 == 1 & rowSums(comb$sign[,c(1:7)]) == 1))
two2<-data.frame(which(comb$sign$s.2 == 1 & rowSums(comb$sign[,c(1:7)]) == 2))

two1$Species<-rownames(two1)
two1$Class<-"Primary"
two1<-two1[,-c(1)]
two2$Species<-rownames(two2)
two2$Class<-"Secondary"
two2<-two2[,-c(1)]

two<-bind_rows(two1,two2)
two$Cluster<-as.factor(2)


#Exclusive species of CL 3
three1<-data.frame(which(comb$sign$s.3 == 1 & rowSums(comb$sign[,c(1:7)]) == 1))
three2<-data.frame(which(comb$sign$s.3 == 1 & rowSums(comb$sign[,c(1:7)]) == 2))

three1$Species<-rownames(three1)
three1$Class<-"Primary"
three1<-three1[,-c(1)]
three2$Species<-rownames(three2)
three2$Class<-"Secondary"
three2<-three2[,-c(1)]

three<-bind_rows(three1,three2)
three$Cluster<-as.factor(3)


#Exclusive species of CL 4
four1<-data.frame(which(comb$sign$s.4 == 1 & rowSums(comb$sign[,c(1:7)]) == 1))
four2<-data.frame(which(comb$sign$s.4 == 1 & rowSums(comb$sign[,c(1:7)]) == 2))

four1$Species<-rownames(four1)
four1$Class<-"Primary"
four1<-four1[,-c(1)]
four2$Species<-rownames(four2)
four2$Class<-"Secondary"
four2<-four2[,-c(1)]

four<-bind_rows(four1,four2)
four$Cluster<-as.factor(4)

#Exclusive species of CL 5
five1<-data.frame(which(comb$sign$s.5 == 1 & rowSums(comb$sign[,c(1:7)]) == 1))
five2<-data.frame(which(comb$sign$s.5 == 1 & rowSums(comb$sign[,c(1:7)]) == 2))

five1$Species<-rownames(five1)
five1$Class<-"Primary"
five1<-five1[,-c(1)]
five2$Species<-rownames(five2)
five2$Class<-"Secondary"
five2<-five2[,-c(1)]

five<-bind_rows(five1,five2)
five$Cluster<-as.factor(5)

#Exclusive species of CL 6
six1<-data.frame(which(comb$sign$s.6 == 1 & rowSums(comb$sign[,c(1:7)]) == 1))
six2<-data.frame(which(comb$sign$s.6 == 1 & rowSums(comb$sign[,c(1:7)]) == 2))

six1$Species<-rownames(six1)
six1$Class<-"Primary"
six1<-six1[,-c(1)]
six2$Species<-rownames(six2)
six2$Class<-"Secondary"
six2<-six2[,-c(1)]

six<-bind_rows(six1,six2)
six$Cluster<-as.factor(6)

#Exclusive species of CL 7
seven1<-data.frame(which(comb$sign$s.7 == 1 & rowSums(comb$sign[,c(1:7)]) == 1))
seven2<-data.frame(which(comb$sign$s.7 == 1 & rowSums(comb$sign[,c(1:7)]) == 2))

seven1$Species<-rownames(seven1)
seven1$Class<-"Primary"
seven1<-seven1[,-c(1)]
seven2$Species<-rownames(seven2)
seven2$Class<-"Secondary"
seven2<-seven2[,-c(1)]

seven<-bind_rows(seven1,seven2)
seven$Cluster<-as.factor(7)

#Join clusters
clas<-bind_rows(two, three, four, five, six, seven)
clas<-clas[,c(3,2,1)]

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")
write.csv(clas,file = "Clasificación_7clusters.csv")

#25/63 species are primarily located in one cluster (characterizing one cluster). The rests are 
#not exclusive (i.e., are secondary characterizing the cluster)


###########Try with 6
pam2<-pamk(p,krange = 3:6,criterion = "asw",usepam = TRUE,scaling = FALSE) #↨Recommends k=6
sort(table(pam2$pamobject$clustering))
#6   2   3   5   4   1 
#36  40  48  88  99 323 
#From k=6 to k=7 it splits cluster 1 in two clusters: GOOD SOLUTION, but I still try to see
#if I manage to characterize k1

comb<-multipatt(p, pam2$pamobject$clustering, func = "IndVal.g", duleg=FALSE, restcomb=NULL,
                min.order = 1, max.order = NULL, control=how(),
                print.perm=FALSE)

#Exclusive species of CL 1
one1<-data.frame(which(comb$sign$s.1 == 1 & rowSums(comb$sign[,c(1:6)]) == 1))
one2<-data.frame(which(comb$sign$s.1 == 1 & rowSums(comb$sign[,c(1:6)]) == 2))

one1$Species<-rownames(one1)
one1$Class<-"Primary"
one1<-one1[,-c(1)]
one2$Species<-rownames(one2)
one2$Class<-"Secondary"
one2<-one2[,-c(1)]

one<-bind_rows(one1,one2) #There are no primary or secondary
one$Cluster<-as.factor(1)

#Exclusive species of CL 2
two1<-data.frame(which(comb$sign$s.2 == 1 & rowSums(comb$sign[,c(1:6)]) == 1))
two2<-data.frame(which(comb$sign$s.2 == 1 & rowSums(comb$sign[,c(1:6)]) == 2))

two1$Species<-rownames(two1)
two1$Class<-"Primary"
two1<-two1[,-c(1)]
two2$Species<-rownames(two2)
two2$Class<-"Secondary"
two2<-two2[,-c(1)]

two<-bind_rows(two1,two2)
two$Cluster<-as.factor(2)


#Exclusive species of CL 3
three1<-data.frame(which(comb$sign$s.3 == 1 & rowSums(comb$sign[,c(1:6)]) == 1))
three2<-data.frame(which(comb$sign$s.3 == 1 & rowSums(comb$sign[,c(1:6)]) == 2))

three1$Species<-rownames(three1)
three1$Class<-"Primary"
three1<-three1[,-c(1)]
three2$Species<-rownames(three2)
three2$Class<-"Secondary"
three2<-three2[,-c(1)]

three<-bind_rows(three1,three2)
three$Cluster<-as.factor(3)


#Exclusive species of CL 4
four1<-data.frame(which(comb$sign$s.4 == 1 & rowSums(comb$sign[,c(1:6)]) == 1))
four2<-data.frame(which(comb$sign$s.4 == 1 & rowSums(comb$sign[,c(1:6)]) == 2))

four1$Species<-rownames(four1)
four1$Class<-"Primary"
four1<-four1[,-c(1)]
four2$Species<-rownames(four2)
four2$Class<-"Secondary"
four2<-four2[,-c(1)]

four<-bind_rows(four1,four2)
four$Cluster<-as.factor(4)

#Exclusive species of CL 5
five1<-data.frame(which(comb$sign$s.5 == 1 & rowSums(comb$sign[,c(1:6)]) == 1))
five2<-data.frame(which(comb$sign$s.5 == 1 & rowSums(comb$sign[,c(1:6)]) == 2))

five1$Species<-rownames(five1)
five1$Class<-"Primary"
five1<-five1[,-c(1)]
five2$Species<-rownames(five2)
five2$Class<-"Secondary"
five2<-five2[,-c(1)]

five<-bind_rows(five1,five2)
five$Cluster<-as.factor(5)

#Exclusive species of CL 6
six1<-data.frame(which(comb$sign$s.6 == 1 & rowSums(comb$sign[,c(1:6)]) == 1))
six2<-data.frame(which(comb$sign$s.6 == 1 & rowSums(comb$sign[,c(1:6)]) == 2))

six1$Species<-rownames(six1)
six1$Class<-"Primary"
six1<-six1[,-c(1)]
six2$Species<-rownames(six2)
six2$Class<-"Secondary"
six2<-six2[,-c(1)]

six<-bind_rows(six1,six2)
six$Cluster<-as.factor(6)


#Join clusters
clas<-bind_rows(one, two, three, four, five, six)
clas<-clas[,c(3,2,1)]
write.csv(clas,file = "Clasificación_6clusters.csv")

#COMPROBAR CON 5

pam3<-pamk(p,krange = 3:5,criterion = "asw",usepam = TRUE,scaling = FALSE)
sort(table(pam3$pamobject$clustering))
comb<-multipatt(p, pam3$pamobject$clustering, func = "IndVal.g", duleg=FALSE, restcomb=NULL,
                min.order = 1, max.order = NULL, control=how(),
                print.perm=FALSE)

#Exclusive species of CL 1
one1<-data.frame(which(comb$sign$s.1 == 1 & rowSums(comb$sign[,c(1:5)]) == 1))
one2<-data.frame(which(comb$sign$s.1 == 1 & rowSums(comb$sign[,c(1:5)]) == 2))

one1$Species<-rownames(one1)
one1$Class<-"Primary"
one1<-one1[,-c(1)]
one2$Species<-rownames(one2)
one2$Class<-"Secondary"
one2<-one2[,-c(1)]

one<-bind_rows(one1,one2) #There are no primary or secondary
one$Cluster<-as.factor(1)

#Exclusive species of CL 2
two1<-data.frame(which(comb$sign$s.2 == 1 & rowSums(comb$sign[,c(1:5)]) == 1))
two2<-data.frame(which(comb$sign$s.2 == 1 & rowSums(comb$sign[,c(1:5)]) == 2))

two1$Species<-rownames(two1)
two1$Class<-"Primary"
two1<-two1[,-c(1)]
two2$Species<-rownames(two2)
two2$Class<-"Secondary"
two2<-two2[,-c(1)]

two<-bind_rows(two1,two2)
two$Cluster<-as.factor(2)


#Exclusive species of CL 3
three1<-data.frame(which(comb$sign$s.3 == 1 & rowSums(comb$sign[,c(1:5)]) == 1))
three2<-data.frame(which(comb$sign$s.3 == 1 & rowSums(comb$sign[,c(1:5)]) == 2))

three1$Species<-rownames(three1)
three1$Class<-"Primary"
three1<-three1[,-c(1)]
three2$Species<-rownames(three2)
three2$Class<-"Secondary"
three2<-three2[,-c(1)]

three<-bind_rows(three1,three2)
three$Cluster<-as.factor(3)


#Exclusive species of CL 4
four1<-data.frame(which(comb$sign$s.4 == 1 & rowSums(comb$sign[,c(1:5)]) == 1))
four2<-data.frame(which(comb$sign$s.4 == 1 & rowSums(comb$sign[,c(1:5)]) == 2))

four1$Species<-rownames(four1)
four1$Class<-"Primary"
four1<-four1[,-c(1)]
four2$Species<-rownames(four2)
four2$Class<-"Secondary"
four2<-four2[,-c(1)]

four<-bind_rows(four1,four2)
four$Cluster<-as.factor(4)

#Exclusive species of CL 5
five1<-data.frame(which(comb$sign$s.5 == 1 & rowSums(comb$sign[,c(1:5)]) == 1))
five2<-data.frame(which(comb$sign$s.5 == 1 & rowSums(comb$sign[,c(1:5)]) == 2))

five1$Species<-rownames(five1)
five1$Class<-"Primary"
five1<-five1[,-c(1)]
five2$Species<-rownames(five2)
five2$Class<-"Secondary"
five2<-five2[,-c(1)]

five<-bind_rows(five1,five2)
five$Cluster<-as.factor(5)



#Join clusters
clas<-bind_rows(one, two, three, four, five)
clas<-clas[,c(3,2,1)]
write.csv(clas,file = "Clasificación_5clusters.csv")

