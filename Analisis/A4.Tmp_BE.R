
rm(list=ls())

#Nº of fields in BE to select fields in 2017

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")

f <- read.csv("Variables.csv", sep = ",", header=TRUE, fill = TRUE)
f <- f[which(f$EspecieObj == "BUOED"), ]
f[f == 99.00] <- NA
f <- f[-which(duplicated(f[ , 2:15])), ]
f <- f[which(complete.cases(f$Contatge)), ]

e <- f[ , which(colnames(f) %in% c("CF_A", "Contatge", "Recob_plotViu", "Recob_plotMort","lev_ind", "Simpson", # All variables
                                   "PromigAltura1Plot", "biom", "SAI_sd","LAI_sd", "TBL_500", "PAR_500","Fallow_500", 
                                   "Irri_500","Zone", "Subzone", "Tractament", "Any", "Codi_Finca"))]

e <- e[ ,c(3:19,1,2)]


e <-e[-which(duplicated(e[ , 1])), ]
length(unique(f$Codi_Finca))

be <- e[ which(e$Subzone %in% "BE"), ]
be <- na.omit(be)

be_2015 <- be[which(be$Any == "2015"), ] #46
be_2016 <- be[which(be$Any == "2016"), ] #58
