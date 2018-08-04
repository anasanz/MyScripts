

#GESTIÓN BARBECHOS: TENDENCIAS COBERTURA DE LA VEGETACIÓN


#1. TENDENCIA COBERTURA CON MEDIA PONDERADA (PESO = CATEGORÍA DE COBERTURA) DE COBERTURA Y ALTURA

  ######1.1. Tendencia de cobertura

library(tidyr)
library(dplyr)

rm(list=ls())
setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Fincas sin sp 3 años")

fin<-read.csv("Fincas 3 años.csv",sep = ",",
              header=TRUE,fill = TRUE)

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")
veg<-read.csv("EvolucioOcells_20170503.csv",sep = ";",
              header=TRUE,fill = TRUE)

vfin<-veg[which(veg$Codi_Finca %in% fin$Codi_Finca), ]


#Como no voy a ver el efecto de las especies, restringir los datos para 1 observación por año y por parcela

vfin<-vfin[vfin$especieObjectiu == "BUOED", ]

vwide<-vfin[ ,c(1,3,5,29,14:28)]

vlong<-gather(vwide,category,valor_cobertura,CSol_nu:Cobert_Alt9b,factor_key = TRUE)
vlong<-arrange(vlong,Codi_Finca,Any)

vlong$peso<-vlong$category
vlong$peso<-as.character(vlong$peso)
vlong$peso[vlong$peso == "CSol_nu"] <- 2.5
vlong$peso[vlong$peso == "Cobert_Alt1"] <- 15
vlong$peso[vlong$peso == "Cobert_Alt2"] <- 15
vlong$peso[vlong$peso == "Cobert_Alt3"] <- 15
vlong$peso[vlong$peso == "Cobert_Alt4"] <- 37.5
vlong$peso[vlong$peso == "Cobert_Alt4b"] <- 50
vlong$peso[vlong$peso == "Cobert_Alt5"] <- 37.5
vlong$peso[vlong$peso == "Cobert_Alt5b"] <- 50
vlong$peso[vlong$peso == "Cobert_Alt6"] <- 37.5
vlong$peso[vlong$peso == "Cobert_Alt6b"] <- 50
vlong$peso[vlong$peso == "Cobert_Alt7"] <- 75
vlong$peso[vlong$peso == "Cobert_Alt7b"] <- 87.5
vlong$peso[vlong$peso == "Cobert_Alt8"] <- 75
vlong$peso[vlong$peso == "Cobert_Alt8b"] <- 87.5
vlong$peso[vlong$peso == "Cobert_Alt9"] <- 75
vlong$peso[vlong$peso == "Cobert_Alt9b"] <- 87.5

#Create a unique code for each finca-any
vlong<-unite(vlong,code,Codi_Finca,Any,sep="_",remove=FALSE)

#Create a column to make the weighted average, containing a value only in the categories that don't
#have a 0 value. (column peso2)

vlong$peso<-as.numeric(vlong$peso)

vlong$peso2<-ifelse (vlong$valor_cobertura == 0,
                     0,vlong$peso)

# Make the weighted average by "code"
df_summary <- 
  vlong %>% 
  group_by(code) %>% 
  summarise(wc = weighted.mean(valor_cobertura, peso2))
cob_av <- left_join(vlong, df_summary, by = 'code')

# Selecciono sólo una observación con la media ponderada
cob_av<-cob_av[cob_av$category == "CSol_nu", ]
cob_av$wc[is.na(cob_av$wc)] <- 0

#TENDENCIA ANUAL COBERTURA
par(mfrow = c(1,1))
boxplot(wc ~ Any, cob_av, main = "Weighted vegetation cover (%)")




######1.2. Tendencia anual altura

vfin<-veg[which(veg$Codi_Finca %in% fin$Codi_Finca), ]

#Como no voy a ver el efecto de las especies, restringir los datos para 1 observación por año y por parcela

vfin<-vfin[vfin$especieObjectiu == "BUOED", ]
vwide<-vfin[ ,c(1,3,5,29,14:28)]

vlong<-gather(vwide,category,valor_cobertura,CSol_nu:Cobert_Alt9b,factor_key = TRUE)
vlong<-arrange(vlong,Codi_Finca,Any)

vlong$peso<-vlong$category
vlong$peso<-as.character(vlong$peso)
vlong$peso[vlong$peso == "CSol_nu"] <- 0
vlong$peso[vlong$peso == "Cobert_Alt1"] <- 10
vlong$peso[vlong$peso == "Cobert_Alt2"] <- 30
vlong$peso[vlong$peso == "Cobert_Alt3"] <- 50
vlong$peso[vlong$peso == "Cobert_Alt4"] <- 10
vlong$peso[vlong$peso == "Cobert_Alt4b"] <- 10
vlong$peso[vlong$peso == "Cobert_Alt5"] <- 30
vlong$peso[vlong$peso == "Cobert_Alt5b"] <- 30
vlong$peso[vlong$peso == "Cobert_Alt6"] <- 50
vlong$peso[vlong$peso == "Cobert_Alt6b"] <- 50
vlong$peso[vlong$peso == "Cobert_Alt7"] <- 10
vlong$peso[vlong$peso == "Cobert_Alt7b"] <- 10
vlong$peso[vlong$peso == "Cobert_Alt8"] <- 30
vlong$peso[vlong$peso == "Cobert_Alt8b"] <- 30
vlong$peso[vlong$peso == "Cobert_Alt9"] <- 50
vlong$peso[vlong$peso == "Cobert_Alt9b"] <- 50

#Create a unique code for each finca-any
vlong<-unite(vlong,code,Codi_Finca,Any,sep="_",remove=FALSE)

#Create a column to make the weighted average, containing a value only in the categories that don't
#have a 0 value. (column peso2)

vlong$peso<-as.numeric(vlong$peso)

vlong$peso2<-ifelse (vlong$valor_cobertura == 0,
                     0,vlong$peso)

# Make the weighted average by "code"
df_summary <- 
  vlong %>% 
  group_by(code) %>% 
  summarise(wc = weighted.mean(valor_cobertura, peso2))
alt_av <- left_join(vlong, df_summary, by = 'code')

# Selecciono sólo una observación con la media ponderada
alt_av<-alt_av[alt_av$category == "CSol_nu", ]
alt_av$wc[is.na(alt_av$wc)] <- 0

#TENDENCIA ANUAL ALTURA
boxplot(wc ~ Any, alt_av, main = "Weighted vegetation height (cm)")




##################En la tendencia de cobertura y altura, asciende en 2015 y desciende en 2016. 
##################Separar por sectores y secanos a ver si hay diferencia

#Añadir sectores y secanos a la nueva data.frame
vrec<-vfin
vrec$Sector1<-vrec$Sector
vrec$Sector1[vrec$Sector1 == 4] <- "AF"
vrec$Sector1[vrec$Sector1 == 2] <- "BE"
vrec$Sector1[vrec$Sector1 == 3] <- "BM"
vrec$Sector1[vrec$Sector1 == 5] <- "GR"
vrec$Sector1[vrec$Sector1 == 1] <- "SI"
vrec$Sector1[vrec$Sector1 == 10] <- "UT"

vrec$Secano<-vrec$Sector
vrec$Secano[vrec$Sector == 4] <- "OCCIDENTAL"
vrec$Secano[vrec$Sector == 2] <- "ORIENTAL"
vrec$Secano[vrec$Sector == 3] <- "ORIENTAL"
vrec$Secano[vrec$Sector == 5] <- "OCCIDENTAL"
vrec$Secano[vrec$Sector == 1] <- "ORIENTAL"
vrec$Secano[vrec$Sector == 10] <- "OCCIDENTAL"



#COBERTURA POR SECTORES Y POR SECANOS

cob_av$Sector1<-vrec$Sector1
cob_av$Secano<-vrec$Secano

boxplot(wc ~ Sector1, cob_av, main = "Weighted vegetation cover (%)")
boxplot(wc ~ Secano, cob_av, main = "Weighted vegetation cover (%)")
#Aquí se ve que la la media de cobertura es menor en los orientales. Ver tendencia temporal

  #Tendencia temporal en secanos occidentales y orientales
voc_cob <- cob_av[cob_av$Secano == "OCCIDENTAL", ]
boxplot(wc ~ Any, voc_cob, main = "OCCIDENTAL: Weighted vegetation cover (%)")

vor_cob <- cob_av[cob_av$Secano == "ORIENTAL", ]
boxplot(wc ~ Any, vor_cob, main = "ORIENTAL: Weighted vegetation cover (%)")
#Aquí se puede ver que en el sector oriental ha habido un aumento del 2014 a 2015, 
#mientras que en el occidental se ha mantenido y en 2016 ha descendido

#En 2014 aparece una media de 0 porque el muestreo del recubrimiento de los plots se hacía 
#por parcela (codi finca, A y B) y el muestreo de cobertura general por unitat. Esto hace
#que figure como 0, porque los datos son de unitat y no están asociados a una finca.

#La media de 0 sólo está en el sector oriental porque son los datos recogidos por Joan+Jaume (BE, BM y SI),
#En los que sólo aparece el codi__unitat y no el de finca. Por lo que el 2014 SI SE PUEDE USAR PARA OCCIDENTALES


#ALTURA POR SECTORES Y SECANOS

alt_av$Sector1<-vrec$Sector1
alt_av$Secano<-vrec$Secano

boxplot(wc ~ Sector1, alt_av, main = "Weighted vegetation height (cm)") #Menos en SI y BE por los 0 de 2014 posiblement
boxplot(wc ~ Secano, alt_av, main = "Weighted vegetation height (cm)") #Mas bajo en orientales por los 0 de 2014?
#Igual que la cobertura. Hay menos en los orientales

#Tendencia temporal en secanos occidentales y orientales
voc_alt <- alt_av[alt_av$Secano == "OCCIDENTAL", ]
boxplot(wc ~ Any, voc_alt, main = "OCCIDENTAL: Weighted vegetation height (cm)")

vor_alt <- alt_av[alt_av$Secano == "ORIENTAL", ]
boxplot(wc ~ Any, vor_alt, main = "ORIENTAL: Weighted vegetation height (cm)")

#IGUAL QUE EN COBERTURA. Se puede ver que en el sector oriental ha habido un aumento, 
#mientras que en el occidental se ha mantenido y en 2016 ha descendido. 
#Media de 0 en 2014 por la misma razón que en la cobertura

#FALTARÍA SEPARAR ENTRE BARBECHO Y ALFALFA PARA LOS ORIENTALES, PERO COMO A LO MEJOR NO USO ESTA
#INFORMACIÓN PORQUE NO HAY DATOS DEL 2014 (Y USO LA DE LOS PLOTS INSTEAD), DE MOMENTO NO LO 
#SEPARO



# Mirar índice para las coberturas de vegetación: Heterogeneidad/Predominancia de una cobertura

vfin<-veg[which(veg$Codi_Finca %in% fin$Codi_Finca), ]
vfin<-vfin[vfin$especieObjectiu == "BUOED", ]
vfin<-vfin[ ,c(1,2,3,5,29,14:28)]

#Crear una nueva columna para cada categoría de 0/1

vfin$one<-ifelse (vfin$Cobert_Alt1 > 0,
                     1,0)
vfin$two<-ifelse (vfin$Cobert_Alt2 > 0,
                  1,0)
vfin$three<-ifelse (vfin$Cobert_Alt3 > 0,
                  1,0)
vfin$four<-ifelse (vfin$Cobert_Alt4 > 0,
                  1,0)
vfin$fourb<-ifelse (vfin$Cobert_Alt4b > 0,
                   1,0)
vfin$five<-ifelse (vfin$Cobert_Alt5 > 0,
                   1,0)
vfin$fiveb<-ifelse (vfin$Cobert_Alt5b > 0,
                   1,0)
vfin$six<-ifelse (vfin$Cobert_Alt6 > 0,
                    1,0)
vfin$sixb<-ifelse (vfin$Cobert_Alt6b > 0,
                  1,0)
vfin$seven<-ifelse (vfin$Cobert_Alt7 > 0,
                  1,0)
vfin$sevenb<-ifelse (vfin$Cobert_Alt7b > 0,
                   1,0)
vfin$eight<-ifelse (vfin$Cobert_Alt8 > 0,
                    1,0)
vfin$eightb<-ifelse (vfin$Cobert_Alt8b > 0,
                    1,0)
vfin$nine<-ifelse (vfin$Cobert_Alt9 > 0,
                    1,0)
vfin$nineb<-ifelse (vfin$Cobert_Alt9b > 0,
                     1,0)
vfin$solnu<-ifelse (vfin$CSol_nu > 0,
                    1,0)

vfin<-vfin[,-c(5:20)]

#Crear índice de heterogeneidad que va del 0 al 16 (0 menos heterogéneo y 16 más heterogéneo)
vhet<-vfin %>% 
  #rowwise will make sure the sum operation will occur on each row
  rowwise() %>% 
  #then a simple sum(..., na.rm=TRUE) is enough to result in what you need
  mutate(het = sum(one,two,three,four,fourb,five,fiveb,six,sixb,seven,sevenb,eight,eightb,nine,nineb,solnu))

#Ver tendencias en heterogeneidad#
vrec<-vhet
vrec$Sector1<-vrec$Sector
vrec$Sector1[vrec$Sector1 == 4] <- "AF"
vrec$Sector1[vrec$Sector1 == 2] <- "BE"
vrec$Sector1[vrec$Sector1 == 3] <- "BM"
vrec$Sector1[vrec$Sector1 == 5] <- "GR"
vrec$Sector1[vrec$Sector1 == 1] <- "SI"
vrec$Sector1[vrec$Sector1 == 10] <- "UT"

vrec$Secano<-vrec$Sector
vrec$Secano[vrec$Sector == 4] <- "OCCIDENTAL"
vrec$Secano[vrec$Sector == 2] <- "ORIENTAL"
vrec$Secano[vrec$Sector == 3] <- "ORIENTAL"
vrec$Secano[vrec$Sector == 5] <- "OCCIDENTAL"
vrec$Secano[vrec$Sector == 1] <- "ORIENTAL"
vrec$Secano[vrec$Sector == 10] <- "OCCIDENTAL"

#HETEROGENEIDAD POR SECTORES Y POR SECANOS

boxplot(het ~ Sector1, vrec, main = "Heterogeneity index (0-16)")
boxplot(het ~ Secano, vrec, main = "Heterogeneity index (0-16)")
#Aquí se ve que la la media de cobertura es menor en los orientales. Ver tendencia temporal

#Tendencia temporal en secanos occidentales y orientales
voc_het <- vrec[vrec$Secano == "OCCIDENTAL", ]
boxplot(het ~ Any, voc_het, main = "OCCIDENTAL: Heterogeneity index (0-16)")

vor_het <- vrec[vrec$Secano == "ORIENTAL", ]
boxplot(het ~ Any, vor_het, main = "ORIENTAL: Heterogeneity index (0-16)")

#???Aumenta la heterogeneidad en ambos secanos. En 2014 media 0 por lo datos de codi_unitat


