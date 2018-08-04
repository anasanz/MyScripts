

library(dplyr)
library(tidyr)

setwd("~/Datos/Datos barbechos arrendados/Variables")
#setwd("~/PhD/Datos/Datos barbechos arrendados/Variables")

f <- read.csv("Variables.csv", sep = ",", header=TRUE, fill = TRUE)
colnames(f)[6] <- "EspecieObj"
f[f == 99.00] <- NA
f <- f[-which(duplicated(f[ , 2:15])), ]
f <- f[which(complete.cases(f$Contatge)), ]



f <- f[ , which(colnames(f) %in% c("CF_A", "EspecieObj", "Contatge", "Recob_plotViu", "Recob_plotMort","lev_ind", "Simpson", # All variables
                                   "PromigAltura1Plot", "biom", "SAI_sd","LAI_sd", "TBL_500", "PAR_500","Fallow_500", 
                                   "shan_500", "Zone", "Tractament", "Any", "Codi_Finca", "area"))]

f <- na.omit(f)
f <- f[-which(f$Tractament == "Pastoreada"), ] # Remove grazing because is only present in 2017
f <- f[-which(duplicated(f[ , 3:9])), ] # Remove duplicates

length(which(f$EspecieObj == "TERAX_m")) #619 FF per species. f is the raw data in which all analyses are based



#Select 
f <- f[ , which(colnames(f) %in% c("Codi_Finca", "Any", "Tractament"))]
f$Tractament <- as.character(f$Tractament)
f$Codi_Finca <- as.character(f$Codi_Finca)
f$Any <- as.character(f$Any)
f$Tractament[which(f$Tractament == "Curronar")] <- "Llaurar" 
#Frequency
freq <- xtabs(  ~ Tractament + Any, data = f)

