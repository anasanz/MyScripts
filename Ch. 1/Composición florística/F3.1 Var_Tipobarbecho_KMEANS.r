
rm(list=ls())

library(dplyr)
library(tidyr)
library(FactoMineR)
library(factoextra)
library(rgl)
library(RColorBrewer)
library(scales)

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

#PCA with most common species

pc <- prcomp(f[,4:66])


##################################EXPLORATION:VARIABLE CONTRIBUTION, EIGENVALUES...##############
#Variable contribution
e.pca <- PCA(f[,4:66], graph = FALSE)
print(e.pca)
#Eigenvalues: Amount of variation retained by each PC
eigenvalues <- e.pca$eig
head(eigenvalues[, 1:2])
fviz_screeplot(e.pca, ncp=20) #THE COMPONENTS EXPLAIN VERY FEW OF THE VARIANCE
pc <- princomp(p)
plot(pc)
plot(pc, type='l') #Coger los primeros 6 components segun the "Elbow rule" para el kmeans
summary(pc)

#Correlations/loadings of the variables with the components. The variables can be plotted as points in the component space using their loadings as coordinates.
head(e.pca$var$coord)
fviz_pca_var(e.pca)
#Quality of the representation of the variables(By the components)
#If a variable is perfectly represented by the two first comp: sum of cos2 = 1
#On the graph: Variables that are closer to the centre of the plot are
#Less important for the first components:
head(e.pca$var$cos2)
fviz_pca_var(e.pca, col.var="cos2") + scale_color_gradient2(low="white", mid="blue", high="red", midpoint=0.5) + theme_minimal()
#Red: Best representation by the two first components
#Check which variables contribute more to the components
#Scale plot:
fviz_pca_var(e.pca, col.var="contrib") +
  scale_color_gradient2(low="white", mid="blue",high="red", midpoint=50) + theme_minimal()
#Contributions of variables in accounting for the variability in a given 
#principal component are (in percentage)
fviz_pca_contrib(e.pca, choice = "var", axes = 1)
#Mirar contribuciÃ³n de las variables de los primeros ejes porque son los que explican
#mayor parte de la varianza
g1<-fviz_contrib(e.pca, choice="var", axes = 1:2, top = 50)

######################################################################


# Multi 3D plot
comp <- data.frame(pc$x[,1:6])
plot3d(comp$PC1, comp$PC2, comp$PC3)

#CLUSTER ANALYSIS
#K-means: Determine the number of clusters: look at the "elbow" of the within groups sum of squares 
# Determine number of clusters (CHANGES??????)
wss <- (nrow(p)-1)*sum(apply(p,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(p,
                                     centers=i)$withinss)
plot( 1:15, wss, type="b", xlab="Number of Clusters",
      ylab="Within groups sum of squares")
#♣Changing the nstart and iter.max doesnt change but I dont understand this
#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- p
wss <- sapply(1:k.max, 
              function(k){kmeans(p, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")



#ELBOW IN 5-6
#Apply k-means with k = 5
k5 <- kmeans(comp, 5, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k5$clust, pch=16)

plot3d(comp$PC1, comp$PC2, comp$PC3, col=k5$clust)
sort(table(k5$clust))



#Apply k-means with k = 6
k6 <- kmeans(comp, 6, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k6$clust, pch=16)
plot3d(comp$PC1, comp$PC2, comp$PC3, col=k6$clust)
sort(table(k6$clust))
clust <- names(sort(table(k6$clust)))

# Ward Hierarchical Clustering
d <- dist(p, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=6) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=6, border="red")
sort(table(groups))

#Cluster validation
library(clValid)
?clValid
val<-clValid(p, 3:10, clMethods = c("hierarchical","kmeans"), 
             validation = "stability", metric = "euclidean", method = "ward",
             verbose=FALSE)
summary(val)
optimalScores(val)

#De 4:7 clusters
#Optimal Scores:
#Internal:
#Score    Method       Clusters
#Connectivity 146.6056 hierarchical 4       
#Dunn           0.0317 hierarchical 4       
#Silhouette     0.1552 kmeans       7       

#Stability:
#Score   Method Clusters
#APN  0.0106 kmeans 4       
#AD  57.1718 kmeans 7       
#ADM  0.7668 kmeans 4       
#FOM  5.6694 kmeans 7 


#De 3:10 clusters
#Internal:
#Score   Method       Clusters
#Connectivity 89.4774 hierarchical 3       
#Dunn          0.0363 kmeans       3       
#Silhouette    0.1552 kmeans       7  

#Stability:
#Score   Method Clusters
#APN  0.0106 kmeans 4       
#AD  55.3398 kmeans 10      
#ADM  0.7668 kmeans 4       
#FOM  5.6598 kmeans 10

#5 CLUSTERS
#MIRAR QUÉ ESPECIES CORRESPONDEN A CADA CLUSTER COGIENDO LAS 65 ESPECIES
f$Clusters_5<-k5$cluster
f<-f[,c(1:3,67,4:66)]
#Ver con loadings de cada especie cómo se caracterizan los clusters
pc$rotation[,c(1,2)]
#Ver con la media de cada especie cómo se caracterizan los clusters
ag1<-aggregate(. ~ Clusters, f, function(x) c(mean = mean(x)))
ag2<-aggregate(. ~ Clusters, f, function(x) c(sd = sd(x)))
library(tidyr)
ag1<-t(ag1)
ag1<-ag1[-c(1:4),]
ag1<-round(ag1,digits = 3)
ag2<-t(ag2)
ag2<-ag2[-c(1:4),]
ag2<-round(ag2,digits = 3)

fviz_pca_biplot(pc, axes = c(5,6), label="var",col.var = "black",habillage = as.factor(k5$clust),
                addEllipses = TRUE) + ggtitle("") +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))

#6 CLUSTERS
#MIRAR QUÉ ESPECIES CORRESPONDEN A CADA CLUSTER COGIENDO LAS 65 ESPECIES
f$Clusters_6<-k6$cluster
f<-f[,c(1:3,67,4:66)]
#Ver con loadings de cada especie cómo se caracterizan los clusters
pc$rotation[,c(1,2)]
#Ver con la media de cada especie cómo se caracterizan los clusters
ag1_6<-aggregate(. ~ Clusters_6, f, function(x) c(mean = mean(x)))
ag2_6<-aggregate(. ~ Clusters_6, f, function(x) c(sd = sd(x)))
library(tidyr)
ag1_6<-t(ag1)
ag1_6<-ag1_6[-c(1:4),]
ag1_6<-round(ag1_6,digits = 3)
ag2_6<-t(ag2_6)
ag2_6<-ag2_6[-c(1:4),]
ag2_6<-round(ag2_6,digits = 3)

fviz_pca_biplot(pc, axes = c(1,2), label=" ",col.var = "black",habillage = as.factor(k6$clust),
                addEllipses = TRUE) + ggtitle("") +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))


#Reducir el número de especies raras para que se extiendan las observaciones del cluster 2
#2.1. No hacerlo por año, solo dividir por barbechos

rm(list=ls())
setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")
fl<-read.csv("Cobertura_media_flora.csv",sep = ",",header=TRUE,fill = TRUE,na.strings="")
#Delete species with low occurrence (Script F1)
#Species with high occurrence in fincas (>10%) calculated for 2 sectors and each year
load("Frequent_species_allyears.RData")
#Add Codi_Finca and Any
d<-c("Codi_Finca","Any","X")
j<-sub(" ",".",p_comb_u)
j[4]<-"Bromus.gr..rubens"
j[7]<-"Hordeum.murinum.subsp..leporinum"
j<-c(j,d)

f<-fl[,which(names(fl) %in% j)]
f<-f[,c(28,25,26,1:25)]
p<-f[,3:27]

#PCA with most common species
pc <- prcomp(f[,3:27])
##################################EXPLORATION:VARIABLE CONTRIBUTION, EIGENVALUES...##############
#Variable contribution
e.pca <- PCA(f[,3:27], graph = FALSE)
print(e.pca)
#Eigenvalues: Amount of variation retained by each PC
eigenvalues <- e.pca$eig
head(eigenvalues[, 1:2])
fviz_screeplot(e.pca, ncp=20)# ~ 60% of the variation retained by the first 2 comp
pc <- princomp(p)
plot(pc)
plot(pc, type='l') #Coger los primeros 6 components segun the "Elbow rule" para el kmeans
summary(pc)
#Correlations/loadings of the variables with the components. The variables can be plotted as points in the component space using their loadings as coordinates.
head(e.pca$var$coord)
fviz_pca_var(e.pca)
#Quality of the representation of the variables(By the components)
#If a variable is perfectly represented by the two first comp: sum of cos2 = 1
#On the graph: Variables that are closer to the centre of the plot are
#Less important for the first components:
head(e.pca$var$cos2)
fviz_pca_var(e.pca, col.var="cos2") + scale_color_gradient2(low="white", mid="blue", high="red", midpoint=0.5) + theme_minimal()
#Red: Best representation by the two first components
#Check which variables contribute more to the components
#Scale plot:
fviz_pca_var(e.pca, col.var="contrib") +
  scale_color_gradient2(low="white", mid="blue",high="red", midpoint=50) + theme_minimal()
#Contributions of variables in accounting for the variability in a given 
#principal component are (in percentage)
fviz_pca_contrib(e.pca, choice = "var", axes = 1)
#Mirar contribuciÃ³n de las variables de los primeros ejes porque son los que explican
#mayor parte de la varianza
g1<-fviz_contrib(e.pca, choice="var", axes = 1:2, top = 50)

######################################################################

#Nº of clusters
wss <- (nrow(p)-1)*sum(apply(p,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(p,
                                     centers=i)$withinss)
plot( 1:15, wss, type="b", xlab="Number of Clusters",
      ylab="Within groups sum of squares")


comp <- data.frame(pc$x[,1:4])

#Apply k-means with k = 5
k5 <- kmeans(comp, 5, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k5$clust, pch=16)
plot3d(comp$PC1, comp$PC2, comp$PC3, col=k5$clust)
sort(table(k5$clust))
clust <- names(sort(table(k5$clust)))

#Apply k-means with k = 6
k6 <- kmeans(comp, 6, nstart=25, iter.max=1000)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k6$clust, pch=16)
plot3d(comp$PC1, comp$PC2, comp$PC3, col=k6$clust)
sort(table(k6$clust))
clust <- names(sort(table(k5$clust)))

fviz_pca_biplot(pc, axes = c(1,2), label="var",col.var = "black",habillage = as.factor(k5$clust),
                addEllipses = TRUE) + ggtitle("") +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))

