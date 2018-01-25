
library(factoextra)
library(digest)
library(FactoMineR)

#DATA EXPLORATION

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")
ab<-read.csv(file = "variables.csv")

#BUOED

buo<-ab[which(ab$EspecieObj == "BUOED"),]
buo<-buo[complete.cases(buo),]
#Correlation variables
cor(buo[,c(8,10:12,14:17,19,21,23,27:29,32:34)],method = "spearman") 
# LAI - Cov. viva (0.6); LAI - altura (0.5); LAI - Simpson (0.5); Tree - TBL (-0.6), LAI - SAI (0.74)

buo_sd <- as.data.frame (scale ( buo[, c(8,10:12,14:17,19,21,23,27:29,32:34)]) )

pbuo <- prcomp (buo_sd)

fviz_pca_biplot(pbuo, axes = c(1,2), label="var",col.var = "black",habillage = as.factor(buo$Tractament),
                addEllipses = FALSE) + ggtitle("") +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))

fviz_pca_biplot(pbuo, axes = c(2,3), label="var",col.var = "black",habillage = as.factor(buo$Tractament),
                addEllipses = FALSE) + ggtitle("") +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))

#TERAX_M
ter<-ab[which(ab$EspecieObj == "TERAX_M"),]
ter<-ter[complete.cases(ter),]
ter_sd<-as.data.frame(scale(ter[,c(8,10:12,14:17,19,21,23,27:29,32:34)]))

pter <- prcomp(ter_sd)

fviz_pca_biplot(pter, axes = c(1,2), label="var",col.var = "black",habillage = as.factor(ter$Tractament),
                addEllipses = TRUE) + ggtitle("") +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))

fviz_pca_biplot(pbuo, axes = c(2,3), label="var",col.var = "black",habillage = as.factor(ter$Tractament),
                addEllipses = FALSE) + ggtitle("") +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))

#ALRUF
al<-ab[which(ab$EspecieObj == "ALRUF"),]
al<-al[complete.cases(al),]
al_sd<-as.data.frame(scale(al[,c(8,10:12,14:17,19,21,23,27:29,32:34)]))

pal <- prcomp(al_sd)

fviz_pca_biplot(pal, axes = c(1,2), label="var",col.var = "black",habillage = as.factor(al$Tractament),
                addEllipses = TRUE) + ggtitle("") +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))

fviz_pca_biplot(pal, axes = c(2,3), label="var",col.var = "black",habillage = as.factor(al$Tractament),
                addEllipses = TRUE) + ggtitle("") +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))

#PTEROCLIDOS
pter<-ab[which(ab$EspecieObj == "PTALC"),]
pter<-pter[complete.cases(pter),]
pter_sd<-as.data.frame(scale(pter[,c(8,10:12,14:17,19,21,23,27:29,32:34)]))

ppter <- prcomp(pter_sd)

fviz_pca_biplot(ppter, axes = c(1,2), label="var",col.var = "black",habillage = as.factor(pter$Tractament),
                addEllipses = TRUE) + ggtitle("") +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))

fviz_pca_biplot(ppter, axes = c(2,3), label="var",col.var = "black",habillage = as.factor(pter$Tractament),
                addEllipses = TRUE) + ggtitle("") +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))


#MECAL
mec<-ab[which(ab$EspecieObj == "MECAL"),]
mec<-mec[complete.cases(mec),]

#Correlation variables
cor(mec[,c(8,10:12,14:18,20,23:26,32:34)],method = "spearman") 

mec_sd<-as.data.frame(scale(mec[,c(8,10:12,14:18,20,23:26,32:34)]))

pmec <- prcomp(mec_sd)

fviz_pca_biplot(pmec, axes = c(1,2), label="var",col.var = "black",habillage = as.factor(mec$Tractament),
                addEllipses = TRUE) + ggtitle("") +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))

fviz_pca_biplot(pmec, axes = c(2,3), label="var",col.var = "black",habillage = as.factor(mec$Tractament),
                addEllipses = TRUE) + ggtitle("") +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))


#TRATAMIENTO - VARIABLES RELACIONADAS

trat<-ab[which(ab$EspecieObj == "MECAL"),]
trat<-trat[complete.cases(trat),]
trat_sd<-as.data.frame(scale(trat[,c(10:12,14,15,32:34)]))

ptrat <- prcomp(trat_sd)

fviz_pca_biplot(ptrat, axes = c(1,2), label="var",col.var = "black",habillage = as.factor(trat$Tractament),
                addEllipses = TRUE) + ggtitle("") +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = "white"))

