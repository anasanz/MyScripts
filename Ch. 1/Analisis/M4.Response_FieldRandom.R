

library(tuple)
setwd("C:/Users/ana.sanz/OneDrive/PhD/First chapter/Datos/Datos barbechos arrendados/Variables")

sp <- read.csv("Data_path_submission2_sp.csv", sep = ",", header=TRUE, fill = TRUE)
sp <- sp[which(sp$Species == "SC"), ]

tot <- length(unique(sp$Codi_Finca)) # Total number of fields
trip <- length(triplicate(sp$Codi_Finca)) # Nº of fields sampled 3 years
prop3 <- trip/tot*100
dup <- length(duplicate(sp$Codi_Finca)) # Nº of fields sampled 3 years
prop2 <- dup/tot*100

