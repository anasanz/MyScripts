
# INCLUDE IRRI OR TREE????

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")

#BUOED: In all zones

f <- read.csv("Variables.csv", sep = ",", header=TRUE, fill = TRUE)
f <- f[which(f$EspecieObj == "BUOED"), ]
f[f == 99.00] <- NA
f <- f[-which(duplicated(f[ , 2:15])), ]
f <- f[which(complete.cases(f$Contatge)), ]
length(unique(f$Codi_Finca))
f <- na.omit(f)


sum(f$Irri_500)
sum(f$Tree_500)
sum(f$Fallow_500)


length(which(f$Irri_500 > 0 & f$Subzone == "AF"))
length(which(f$Irri_500 > 0 & f$Subzone == "SI"))
length(which(f$Irri_500 > 0 & f$Subzone == "BE"))
length(which(f$Irri_500 > 0 & f$Subzone == "BM"))
length(which(f$Irri_500 > 0 & f$Subzone == "GR"))
length(which(f$Irri_500 > 0 & f$Subzone == "UT"))

length(which(f$Irri_500 > 0 & f$Zone == "OCCIDENTAL"))
length(which(f$Irri_500 > 0 & f$Zone == "ORIENTAL")) # En 170 de 441



length(which(f$Tree_500 > 0 & f$Subzone == "AF"))
length(which(f$Tree_500 > 0 & f$Subzone == "SI"))
length(which(f$Tree_500 > 0 & f$Subzone == "BE"))
length(which(f$Tree_500 > 0 & f$Subzone == "BM"))
length(which(f$Tree_500 > 0 & f$Subzone == "GR"))
length(which(f$Tree_500 > 0 & f$Subzone == "UT"))

length(which(f$Tree_500 > 0 & f$Zone == "OCCIDENTAL"))
length(which(f$Tree_500 > 0 & f$Zone == "ORIENTAL"))


#Terax: Only ORIENTALES

f <- read.csv("Variables.csv", sep = ",", header=TRUE, fill = TRUE)

f <- f[which(f$EspecieObj == "TERAX_M"), ]
f <- f[ which(f$Zone == "ORIENTAL"), ] # - 224
f[f == 99.00] <- NA
f <- f[-which(duplicated(f[ , 2:15])), ]
f <- f[which(complete.cases(f$Contatge)), ]
f <- na.omit(f)

sum(f$Irri_500)
sum(f$Tree_500)
sum(f$Fallow_500)



length(which(f$Irri_500 > 0 & f$Subzone == "SI"))
length(which(f$Irri_500 > 0 & f$Subzone == "BE"))
length(which(f$Irri_500 > 0 & f$Subzone == "BM"))

length(which(f$Irri_500 > 0 & f$Zone == "ORIENTAL")) #En 100/275



length(which(f$Tree_500 > 0 & f$Subzone == "SI"))
length(which(f$Tree_500 > 0 & f$Subzone == "BE"))
length(which(f$Tree_500 > 0 & f$Subzone == "BM"))

length(which(f$Tree_500 > 0 & f$Zone == "OCCIDENTAL"))
length(which(f$Tree_500 > 0 & f$Zone == "ORIENTAL"))

