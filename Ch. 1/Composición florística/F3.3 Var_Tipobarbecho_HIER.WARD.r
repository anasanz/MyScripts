

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

#Hierarchical clustering

########### 8 CLUSTERS ########################

d <- dist(p, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D") #With method "ward": equal size. Useful if it finds clusters with few observations
#It is this case, so better to use it.
plot(fit) # display dendogram
groups <- cutree(fit, k=8) # cut tree into  clusters

rect.hclust(fit, k=8, border="red")

table(groups)

#2. Cluster composition
comb<-multipatt(p, groups, func = "IndVal.g", duleg=FALSE, restcomb=NULL,
                min.order = 1, max.order = NULL, control=how(),
                print.perm=FALSE)

#Exclusive species of CL 1
one1<-data.frame(which(comb$sign$s.1 == 1 & rowSums(comb$sign[,c(1:8)]) == 1))
one2<-data.frame(which(comb$sign$s.1 == 1 & rowSums(comb$sign[,c(1:8)]) == 2))

one1$Species<-rownames(one1)
one1$Class<-"Primary"
one1<-one1[,-c(1)]
one2$Species<-rownames(one2)
one2$Class<-"Secondary"
one2<-one2[,-c(1)]

one<-bind_rows(one1,one2) 
one$Cluster<-as.factor(1)

#Exclusive species of CL 2
two1<-data.frame(which(comb$sign$s.2 == 1 & rowSums(comb$sign[,c(1:8)]) == 1))
two2<-data.frame(which(comb$sign$s.2 == 1 & rowSums(comb$sign[,c(1:8)]) == 2))

two1$Species<-rownames(two1)
two1$Class<-"Primary"
two1<-two1[,-c(1)]
two2$Species<-rownames(two2)
two2$Class<-"Secondary"
two2<-two2[,-c(1)]

two<-bind_rows(two1,two2)
two$Cluster<-as.factor(2)


#Exclusive species of CL 3
three1<-data.frame(which(comb$sign$s.3 == 1 & rowSums(comb$sign[,c(1:8)]) == 1))
three2<-data.frame(which(comb$sign$s.3 == 1 & rowSums(comb$sign[,c(1:8)]) == 2))

three1$Species<-rownames(three1)
three1$Class<-"Primary"
three1<-three1[,-c(1)]
three2$Species<-rownames(three2)
three2$Class<-"Secondary"
three2<-three2[,-c(1)]

three<-bind_rows(three1,three2)
three$Cluster<-as.factor(3)


#Exclusive species of CL 4
four1<-data.frame(which(comb$sign$s.4 == 1 & rowSums(comb$sign[,c(1:8)]) == 1))
four2<-data.frame(which(comb$sign$s.4 == 1 & rowSums(comb$sign[,c(1:8)]) == 2))

four1$Species<-rownames(four1)
four1$Class<-"Primary"
four1<-four1[,-c(1)]
four2$Species<-rownames(four2)
four2$Class<-"Secondary"
four2<-four2[,-c(1)]

four<-bind_rows(four1,four2)
four$Cluster<-as.factor(4)

#Exclusive species of CL 5
five1<-data.frame(which(comb$sign$s.5 == 1 & rowSums(comb$sign[,c(1:8)]) == 1))
five2<-data.frame(which(comb$sign$s.5 == 1 & rowSums(comb$sign[,c(1:8)]) == 2))

five1$Species<-rownames(five1)
five1$Class<-"Primary"
five1<-five1[,-c(1)]
five2$Species<-rownames(five2)
five2$Class<-"Secondary"
five2<-five2[,-c(1)]

five<-bind_rows(five1,five2)
five$Cluster<-as.factor(5)

#Exclusive species of CL 6
six1<-data.frame(which(comb$sign$s.6 == 1 & rowSums(comb$sign[,c(1:8)]) == 1))
six2<-data.frame(which(comb$sign$s.6 == 1 & rowSums(comb$sign[,c(1:8)]) == 2))

six1$Species<-rownames(six1)
six1$Class<-"Primary"
six1<-six1[,-c(1)]
six2$Species<-rownames(six2)
six2$Class<-"Secondary"
six2<-six2[,-c(1)]

six<-bind_rows(six1,six2)
six$Cluster<-as.factor(6)

#Exclusive species of CL 7
seven1<-data.frame(which(comb$sign$s.7 == 1 & rowSums(comb$sign[,c(1:8)]) == 1))
seven2<-data.frame(which(comb$sign$s.7 == 1 & rowSums(comb$sign[,c(1:8)]) == 2))

seven1$Species<-rownames(seven1)
seven1$Class<-"Primary"
seven1<-seven1[,-c(1)]
seven2$Species<-rownames(seven2)
seven2$Class<-"Secondary"
seven2<-seven2[,-c(1)]

seven<-bind_rows(seven1,seven2)
seven$Cluster<-as.factor(7)

#Exclusive species of CL 8
eight1<-data.frame(which(comb$sign$s.8 == 1 & rowSums(comb$sign[,c(1:8)]) == 1))
eight2<-data.frame(which(comb$sign$s.8 == 1 & rowSums(comb$sign[,c(1:8)]) == 2))

eight1$Species<-rownames(eight1)
eight1$Class<-"Primary"
eight1<-eight1[,-c(1)]
eight2$Species<-rownames(eight2)
eight2$Class<-"Secondary"
eight2<-eight2[,-c(1)]

eight<-bind_rows(eight1,eight2)
eight$Cluster<-as.factor(8)

#Join clusters
clas<-bind_rows(one,two, three, four, five, six, seven, eight)
clas<-clas[,c(3,2,1)]

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")
write.csv(clas,file = "Clasificaci贸n_8clusters_WARD.csv")

#Me quedo con 8 clusters porque es la divisi髇 m醩 l骻ica (mirar excel Comparaci髇_Clusters_Divisi髇_PC1)
#Crear variable

f$Cluster<-groups
f<-f[,c(1:3,67,4:66)]

#write.csv(f,file = "Cluster_tipo_barbecho.csv")



###################7 CLUSTERS##########################################################
d <- dist(p, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D") #With method "ward": equal size. Useful if it finds clusters with few observations
#It is this case, so better to use it.
plot(fit) # display dendogram
groups <- cutree(fit, k=7) # cut tree into  clusters

rect.hclust(fit, k=7, border="red")

table(groups)
#1   2   3   4   5   6   7 
#251  40  33  96  94  82  38
#

#2. Cluster composition
comb<-multipatt(p, groups, func = "IndVal.g", duleg=FALSE, restcomb=NULL,
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

one<-bind_rows(one1,one2) 
one$Cluster<-as.factor(1)

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
clas<-bind_rows(one,two, three, four, five, six, seven)
clas<-clas[,c(3,2,1)]

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")
write.csv(clas,file = "Clasificaci贸n_7clusters_WARD.csv")

#############################6 CLUSTERS#####################################################
#Hierarchical clustering
d <- dist(p, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D") #With method "ward": equal size. Useful if it finds clusters with few observations
#It is this case, so better to use it.
plot(fit) # display dendogram
groups <- cutree(fit, k=6) # cut tree into  clusters
rect.hclust(fit, k=6, border="red")
table(groups)
#1   2   3   4   5   6 
#284  40  96  94  82  38 


#2. Cluster composition
comb<-multipatt(p, groups, func = "IndVal.g", duleg=FALSE, restcomb=NULL,
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

one<-bind_rows(one1,one2) 
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
clas<-bind_rows(one,two, three, four, five, six)
clas<-clas[,c(3,2,1)]

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")
write.csv(clas,file = "Clasificaci贸n_6clusters_WARD.csv")


#5 CLUSTERS
d <- dist(p, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D") #With method "ward": equal size. Useful if it finds clusters with few observations
#It is this case, so better to use it.
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into  clusters
rect.hclust(fit, k=5, border="red")
table(groups)
#1   2   3   4   5   6 
#284  40  96  94  82  38 


#2. Cluster composition
comb<-multipatt(p, groups, func = "IndVal.g", duleg=FALSE, restcomb=NULL,
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

one<-bind_rows(one1,one2) 
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
clas<-bind_rows(one,two, three, four, five, six)
clas<-clas[,c(3,2,1)]

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")
write.csv(clas,file = "Clasificaci贸n_6clusters_WARD.csv")

