


setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")

f<-read.csv("Variables.csv",sep = ",",header=TRUE,fill = TRUE)
f<-f[which(f$EspecieObj == "BUOED"),]
trat<-f[ ,which(colnames(f) %in% c("IDfinca","Codi_Finca","Any","CF_A","Subzone","Zone","EspecieObj",
                                      "Recob_plotViu","Recob_plotMort","PromigAltura1Plot",
                                      "Simpson","lev_ind","Tractament"))]

trat<-f[which(f$EspecieObj == "BUOED"),]
trat<-trat[complete.cases(trat),] #Si añado lev_ind se van 9 observaciones
#Change treatment control so that it is the intercept
trat$Tractament<-as.character(trat$Tractament)
trat$Tractament[which(trat$Tractament == "Control")]<-"Acontrol"

library(lme4)

alt<-lm(PromigAltura1Plot ~ Tractament, data = trat)
summary(alt)

viv<-lm(Recob_plotViu ~ Tractament, data = trat)
summary(viv)

muer<-lm(Recob_plotMort ~ Tractament, data = trat)
summary(muer)


#Plot

trat$Tractament[which(trat$Tractament == "Acontrol")]<-"Control"
trat$Tractament[which(trat$Tractament == "Alfals")]<-"Alfalfa"
trat$Tractament[which(trat$Tractament == "Curronar")]<-"Compactar"
trat$Tractament[which(trat$Tractament == "Llaurar")]<-"Labrar"
trat$Tractament[which(trat$Tractament == "Picar i herbicidar")]<-"Picar y herbicidar"


par(mfrow = c(3,1),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,0) + 0.1)

boxplot(Recob_plotViu ~ Tractament, trat)
mtext("% Veg viva",side=2,line=3, cex = 0.9)
boxplot(Recob_plotMort ~ Tractament, trat) 
mtext("% Veg muerta",side=2,line=3, cex = 0.9)
boxplot(PromigAltura1Plot ~ Tractament, trat)
mtext("Altura",side=2,line=3, cex = 0.9)

mtext("Tratamiento",side=1,line=1,outer = TRUE,cex = 0.9)
