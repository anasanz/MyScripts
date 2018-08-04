
#ORTHOPTER BIOMASS (USING REGRESSION EQUATION)
setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Consultas")
ab<-read.csv("Birddata_abundance.csv",sep = ";",header=TRUE,fill = TRUE) #birddata_abundance es el archivo mÃ¡s reciente de abundancia (9/10)
or<-ab[which(ab$EspecieObj == "ALARV"),c(1:3,5,8:10)]
or<-or[which(or$Any != "2014"), ]
or<-or[which(or$OrtopterPetits != 99.0),] #Definitive orthopter file

#Equation for orthopters:
#W = a*L^b (W=Weight; a = intercept; b = slope; L = Length)
#Categories: Petits = < 2; Mitjans: 2-4; Grans: > 4
#Mean length: Petits =  (10 mm); Mitjans: 30 (mm); Grans: 50 (mm)
#a = 0.0255 ; b = 2.637

jumpy_weight<-function (L,Size) {
  w <- 0.0255*L^2.637
  biom <- w*Size
  return (biom)
}

or$wp<-jumpy_weight(L = 10, Size = or$OrtopterPetits)
or$wm<-jumpy_weight(L = 30, Size = or$OrtopterMitjans)
or$wg<-jumpy_weight(L = 50, Size = or$OrtoptersGrans)

for (i in 1:531){
  or$biom [i] <- sum(wp [i],wm [i],wg [i])
}

setwd("C:/Users/Ana/Documents/PHD/PHD/Datos barbechos arrendados/Variables")

write.csv(or,"or_biomass.csv")


#With all (2017)

#ORTHOPTER BIOMASS (USING REGRESSION EQUATION)
setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Consultas")
ab<-read.csv("Birddata_abundance_FINAL.csv",sep = ";",header=TRUE,fill = TRUE) #birddata_abundance es el archivo mÃ¡s reciente de abundancia (9/10)
or<-ab[which(ab$especieObjectiu == "BUOED"),c(1:3,5,8:10)]
or<-or[which(or$Any != "2014"), ]
or<-or[which(or$OrtopterPetits != 99.0),] #Definitive orthopter file

#Equation for orthopters:
#W = a*L^b (W=Weight; a = intercept; b = slope; L = Length)
#Categories: Petits = < 2; Mitjans: 2-4; Grans: > 4
#Mean length: Petits =  (10 mm); Mitjans: 30 (mm); Grans: 50 (mm)
#a = 0.0255 ; b = 2.637

jumpy_weight<-function (L,Size) {
  w <- 0.0255*L^2.637
  biom <- w*Size
  return (biom)
}

or$wp<-jumpy_weight(L = 10, Size = or$OrtopterPetits)
or$wm<-jumpy_weight(L = 30, Size = or$OrtopterMitjans)
or$wg<-jumpy_weight(L = 50, Size = or$OrtoptersGrans)

for (i in 1:821){
  or$biom [i] <- sum (or$wp[i], or$wm [i], or$wg [i])
}

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")

write.csv(or,"or_biomass_FINAL.csv")
