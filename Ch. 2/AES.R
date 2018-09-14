
# FIND AREA AES
rm(list=ls())
library(rgdal)

# ---- Before 2014 there is no AES
# ---- 2014 ----

#Load data
setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014")
d14 <- read.csv("DUN2014.csv", sep = ";") #DUN
sp14 <- readOGR("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/SIGPAC_2014/SP14_clip.shp")

# Check duplicates DUN
dup_all <- d14[which(duplicated(d14$ID_REC)), ] #There are many duplicates that dont contain fallow (no possible AES)
                                            # Im interested in the duplicates that are caused by an extra strip of AES

#Categorías que podrían ser AES:
unique(d14$PROD_NOM) 
#SUPERFÍCIE LLIURE DE SEMBRA/GU
#GUARETS TRADICIONALS
#GUARETS MEDIAMBIENTAL COMPLEME
#GUARETS MEDIAMBIENTAL R.CEE 20
aes_categories <- c("SUPERFÍCIE LLIURE DE SEMBRA/GU", "GUARETS TRADICIONALS", "GUARETS MEDIAMBIENTAL COMPLEME", "GUARETS MEDIAMBIENTAL R.CEE 20")

#Subset only with those categories
aes <- d14[which(d14$PROD_NOM %in% aes_categories), ]
# Take ID_REC of those categories
id_aes <- unique(aes$ID_REC)
#Subset with only ids that contain possible AES
rec_aes <- d14[which(d14$ID_REC %in% id_aes), ]

# Check duplicates in dataset with ids that have an aes category (and maybe other, because it is a strip)
rec_dup_aes <- rec_aes[which(duplicated(rec_aes$ID_REC)), ] #Only the duplicated one
id_dup_aes <- unique(rec_aes$ID_REC)

rec_dup_aes_all <- d14[which(d14$ID_REC %in% id_dup_aes), ] # Todas las categorias que podrian ser AES + duplicados (Strips)

rec_dup_aes_strips <- rec_dup_aes_all[which(duplicated(rec_dup_aes_all$ID_REC)), ] #Only the duplicated one
id_strips <- unique(rec_dup_aes_strips$ID_REC)

aes_strips <- d14[which(d14$ID_REC %in% id_strips), ] # Recintos de AES en bandas junto con campo de cultivo 2014: SEGURO AES


# Mirar sólo los recintos dentro de ZEPA (Los de fuera no son AES). Sólo se puede mirar el ID de los recintos espacialmente
# con la capa del sigpac
# Load sigpac2014 clipped with red_natura

sp14_zepa <- readOGR("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/SIGPAC_2014/SP14_clip_inzepa.shp")
id_inzepa <- unique(sp14_zepa@data$ID_REC) # IDs
# Subset la DUN sólo con las de dentro de zepa y ver categorías
d14 <- d14[which(d14$ID_REC %in% id_inzepa), ] # Todas las categorias que podrian ser AES + duplicados (Strips)
#Categorías que podrían ser AES:
unique(d14$PROD_NOM)
d14$PROD_NOM <- as.character(d14$PROD_NOM)
xtabs(~PROD_NOM, d14) 
s <- d14[which(d14$PROD_NOM == "SUPERFÍCIE LLIURE DE SEMBRA/GU"), ] # Aquí se pueden detectar tambien las bandas (dif ha)
s2 <- s[which(s$ID_REC == "25249:0:0:1:111:13"), ]
s3 <- d14[which(d14$ID_REC == "25249:0:0:1:111:13"), ]
s4 <- d14[which(d14$ID_REC == "25046:0:0:35:26:6"), ]
which(s$PROD_NOM != "SUPERFÍCIE LLIURE DE SEMBRA/GU")
#SUPERFÍCIE LLIURE DE SEMBRA/GU: 4242
#GUARETS TRADICIONALS: 226
#GUARETS MEDIAMBIENTAL COMPLEME: 62
#GUARETS MEDIAMBIENTAL R.CEE 20: 4

# Considerar todos como AES o sólo algunos???

# 1. ---- Load data ---- 

sp14 <- readOGR("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/SIGPAC_2014/SP14_clip_project2.shp")
setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014")
d14 <- read.csv("DUN2014.csv", sep = ";") #DUN

# 2. ---- Identificar las fincas que son AES por franja+cultivo ----

# Check duplicates DUN
dup_all <- d14[which(duplicated(d14$ID_REC)), ] #There are many duplicates that dont contain fallow (no possible AES)
# Im interested in the duplicates that are caused by an extra strip of AES

#Categorías que podrían ser AES:
unique(d14$PROD_NOM) 
#SUPERFÍCIE LLIURE DE SEMBRA/GU
#GUARETS TRADICIONALS
#GUARETS MEDIAMBIENTAL COMPLEME
#GUARETS MEDIAMBIENTAL R.CEE 20
aes_categories <- c("SUPERFÍCIE LLIURE DE SEMBRA/GU", "GUARETS TRADICIONALS", "GUARETS MEDIAMBIENTAL COMPLEME", "GUARETS MEDIAMBIENTAL R.CEE 20")

#Subset only with those categories
aes <- d14[which(d14$PROD_NOM %in% aes_categories), ]
# Take ID_REC of those categories
id_aes <- unique(aes$ID_REC)
#Subset with only ids that contain possible AES
rec_aes <- d14[which(d14$ID_REC %in% id_aes), ]

# Check duplicates in dataset with ids that have an aes category (and maybe other, because it is a strip)
rec_dup_aes <- rec_aes[which(duplicated(rec_aes$ID_REC)), ] #Only the duplicated one
id_dup_aes <- unique(rec_aes$ID_REC)

rec_dup_aes_all <- d14[which(d14$ID_REC %in% id_dup_aes), ] # Todas las categorias que podrian ser AES + duplicados (Strips)

rec_dup_aes_strips <- rec_dup_aes_all[which(duplicated(rec_dup_aes_all$ID_REC)), ] #Only the duplicated one
id_strips <- unique(rec_dup_aes_strips$ID_REC)

aes14_strips <- d14[which(d14$ID_REC %in% id_strips), ] # Recintos de AES en bandas junto con campo de cultivo 2014: SEGURO AES

# 3. ---- Juntar info sigpac - dun ----
  # 3.1. ---- Juntar información normal cultivos (no franjas) ----
sp14@data <- sp14@data[ ,which(colnames(sp14@data) %in% c("CAMPANYA", "ID_REC", "COMARCA", "US", "HA", "Shape_Leng", "Shape_Area"))]
sp14@data
head(sp14@data)



# Del 2015 al 2017: 
# Disponibel: Capa con las parcelas de AES. No digitalizada la franja de la submedida Cultivo + Franja barbecho (ID recinto duplicado para cada uso)
# Juntar en el mismo ID-REC ambos usos junto con el área

# ---- 2015 ----

d15 <- readOGR("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS/AES/Codi364_DUN2015-2017/364/364/364_2015.shp")
head(d15@data)

# Limpiar columnas
d15@data <- d15@data[, which(colnames(d15@data) %in% c("OBJECTID", "CAMPANYA", "ID_REC", "M2_SP", "HA_SP", "HA_DEC", 
                                        "US_SP", "PENDENT", "PROD_NUM", "PROD_NOM", "AJUTS", "ZEPA"))]
d15@data$US_CU <- 0
d15@data$HA_CU <- 0
d15@data$AES <- 0
d15@data$HA_AES <- 0


#INTENTO 1
# Identificar los ID duplicados (franjas)
dup_all <- d15@data[which(duplicated(d15@data$ID_REC)), ]
dup_all$ID_REC <- as.character(dup_all$ID_REC)

id_dup <- unique(dup_all$ID_REC)

for (i in 1:nrow(d15@data)){
  
  if (d15@data$ID_REC[i] %in% id_dup) { # Check if ID_REC is duplicated (if it is a franja)
    
    if (d15@data$PROD_NOM[i] == "GUARET SIE/ SUP. LLIURE SEMBRA") {
      
      
    }# Si es ordi, blat tou o triticale meter info en cultivo y si es guaret en AES
    d15@data$[i] <- d15@data$PROD_NOM[i]
    }

    }
i = 1

# INTENTO 2_
library(tidyr)

d15@data$HA_DEC <- as

id_dup <- unique(dup_all$ID_REC)

for (i in 1:length(id_dup)){
  
  tmp <- d15@data[which(d15@data$ID_REC == id_dup[i]), ] # Select the duplicated rows for each franja
  spread(tmp, tmp$PROD_NOM, tmp$HA_DEC) #No funciona. AQUI ME QUEDO (la idea es spread para combinar filas?)
  
  for (j in 1: nrow(d15@data)){
    
  }
  
  if (d15@data$ID_REC id_dup) { # Check if ID_REC is duplicated (if it is a franja)
    
    if (d15@data$PROD_NOM[i] == "GUARET SIE/ SUP. LLIURE SEMBRA") {
      
      
    }# Si es ordi, blat tou o triticale meter info en cultivo y si es guaret en AES
    d15@data$[i] <- d15@data$PROD_NOM[i]
  }
  
}

# SOlo dataset con ID duplicados

rec_dup_aes_all <- d15@data[which(d15@data$ID_REC %in% id_dup), ] # Todas las categorias que podrian ser AES + duplicados (Strips)
rec_dup_aes_all <- rec_dup_aes_all[order(rec_dup_aes_all$ID_REC), ]



# Ordenar por orden alfabético para que los duplicados siempre sean la misma categoría

d15@data <- d15@data[order(d15@data$PROD_NOM), ]





