
# FIND AREA AES
rm(list=ls())
library(rgdal)
library(dplyr)
library(raster)
library(tidyr)

# ---- 2010 ----
# Load dun2010 (with measure 312)

setwd("C:/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/DUN/ID_REC_AES")

d10 <- read.csv("IDREC_AES_2010.csv", sep = ";")

# Limpiar columnas

colnames(d10)[which(colnames(d10) == "Superfície.declarada")] <- "HA_DEC"
colnames(d10)[which(colnames(d10) == "Superfície.SIGPAC")] <- "HA_SP"
colnames(d10)[which(colnames(d10) == "Ús.SIGPAC")] <- "US_SP"
colnames(d10)[which(colnames(d10) == "Nom.producte")] <- "PROD_NOM"
colnames(d10)[which(colnames(d10) == "Campanya")] <- "CAMPANYA"


d10 <- d10[, which(colnames(d10) %in% c("CAMPANYA", "ID_REC", "HA_DEC", "HA_SP", 
                                                       "US_SP", "PROD_NOM"))]

d10$HA_Crop <- 0
d10$STRIP <- 0
d10$HA_Fallow <- 0
d10$id_obj <- seq(1,nrow(d10),1)

d10$PROD_NOM <- as.character(d10$PROD_NOM) #To simplify: Any crop = CROP and any fallow = FALLOW
unique(d10$PROD_NOM)
d10$PROD_NOM[which(d10$PROD_NOM %in% c("ORDI", "BLAT TOU", "ALTRES CEREALS", "TRITICALE", "CIVADA"))] <- "CROP"
d10$PROD_NOM[which(d10$PROD_NOM %in% c("SUPERF-CIE LLIURE DE SEMBRA", "RETIRADA VOLUNT+RIA","GUARETS TRADICIONALS", "GUARETS MEDIAMBIENTAL COMPLEME" ))] <- "FALLOW"

#Make easier the join later
d10$CAMPANYA <- as.character(d10$CAMPANYA) 
d10$ID_REC <- as.character(d10$ID_REC) 
d10$US_SP <- as.character(d10$US_SP)
d10$HA_DEC <- as.numeric(d10$HA_DEC)
d10$id_obj <- as.character(d10$id_obj)

#Check duplicated

dup_all <- d10[which(duplicated(d10$ID_REC)), ] #Identify duplicates (franjas)
dup_all$ID_REC <- as.character(dup_all$ID_REC)
id_dup <- unique(dup_all$ID_REC) # List ids duplicated
dup <- d10[which(d10$ID_REC %in% id_dup), ] # Only to check
dup <- arrange(dup,desc(ID_REC))

#Data frame to store

df <- as.data.frame(matrix(0, nrow = length(id_dup), ncol = 8))
colnames(df) <- c("CAMPANYA", "ID_REC", "HA_SP", 
                  "US_SP","HA_Crop", "STRIP","HA_Fallow","id_obj")

#Make easier the join later
df$CAMPANYA <- as.character(df$CAMPANYA) 
df$ID_REC <- as.character(df$ID_REC) 
df$US_SP <- as.character(df$US_SP)
df$id_obj <- as.character(df$id_obj)


# Combine information strips

for (i in 1:length(id_dup)){
  
  tmp <- d10[which(d10$ID_REC == id_dup[i]), ] # Select the duplicated rows for each franja
  tmp_spread <- spread(tmp, PROD_NOM, HA_DEC, fill = NA, drop = TRUE) # Spread to isolate the size of crop/fallows
  
  tmp_spread$HA_Crop <- sum(tmp_spread$CROP[which(!is.na(tmp_spread$CROP))]) # Sum in case there is more than one crop and place
  tmp_spread$HA_Fallow <- sum(tmp_spread$FALLOW[which(!is.na(tmp_spread$FALLOW))]) # number in unique variable for field HA_crop/fallow
  
  tmp_unique <- tmp_spread[-which(duplicated(tmp_spread$ID_REC)), 
                           -which(colnames(tmp_spread) %in% c("CROP", "FALLOW"))] #Remove duplicates and crop/fallow cols
  df[i, ] <- tmp_unique
}

# Identify strips (franjas): 1/0
for (i in 1:nrow(df)){
  if (df$HA_Crop[i] > 0 & df$HA_Fallow[i] > 0) {df$STRIP[i] <- 1}
}

# Join select id_rec from sigpac that are AES (id from df)
# Load sigpac2010
sp10 <- readOGR("C:/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/SIGPAC_2010", layer = "sp10_clip")

# Same name df = sigpac
colnames(df)[colnames(df) == "ID_REC"] <- "CODI_REC"
id_aes <- unique(df$CODI_REC) # Take IDs from 

sp10_aes <- sp10[which(sp10@data$CODI_REC %in% id_aes), ] #clip only aes
head(sp10_aes@data)
# Join data from df (size of fields and strips from dun)
sp10_aes@data <- sp10_aes@data[ ,which(colnames(sp10_aes@data) %in% c("CODI_REC", "SUP_M2", "SUP_HA", 
                                                                      "PENDENT", "Shape_Leng", "Shape_Area"))]
sp10_aes@data <-left_join(sp10_aes@data, df, by = "CODI_REC")

#setwd("C:/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/Only AES")
#writeOGR(sp10_aes,
#         dsn = "C:/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/Only AES", layer = "AES_2010", driver = "ESRI Shapefile")

# ---- 2011 ----
# Load dun2011 (with measure 312)

setwd("C:/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/DUN/ID_REC_AES")

d11 <- read.csv("IDREC_AES_2011.csv", sep = ";")

# Limpiar columnas

colnames(d11)[which(colnames(d11) == "SuperfComprovada")] <- "HA_DEC"
colnames(d11)[which(colnames(d11) =="RecinteSIGPAC")] <- "ID_REC"
colnames(d11)[which(colnames(d11) == "Descripció.ajut")] <- "PROD_NOM"
colnames(d11)[which(colnames(d11) == "Campanya")] <- "CAMPANYA"


d11 <- d11[, which(colnames(d11) %in% c("CAMPANYA", "ID_REC", "HA_DEC", "PROD_NOM"))]

d11$HA_Crop <- 0
d11$STRIP <- 0
d11$HA_Fallow <- 0
d11$id_obj <- seq(1,nrow(d11),1)

d11$PROD_NOM <- as.character(d11$PROD_NOM) #To simplify: Any crop = CROP and any fallow = FALLOW
unique(d11$PROD_NOM)
d11$PROD_NOM[which(d11$PROD_NOM %in% "ZEPA: cereal hivern")] <- "CROP"
d11$PROD_NOM[which(d11$PROD_NOM %in% "ZEPAs:guaret mediamb")] <- "FALLOW"

#Make easier the join later
d11$CAMPANYA <- as.character(d11$CAMPANYA) 
d11$ID_REC <- as.character(d11$ID_REC) 
d11$HA_DEC <- as.numeric(d11$HA_DEC)
d11$id_obj <- as.character(d11$id_obj)

#Check duplicated

dup_all <- d11[which(duplicated(d11$ID_REC)), ] #Identify duplicates (franjas)
dup_all$ID_REC <- as.character(dup_all$ID_REC)
id_dup <- unique(dup_all$ID_REC) # List ids duplicated
dup <- d11[which(d11$ID_REC %in% id_dup), ] # Only to check
dup <- arrange(dup,desc(ID_REC))

#Data frame to store

df <- as.data.frame(matrix(0, nrow = length(id_dup), ncol = 6))
colnames(df) <- c("CAMPANYA", "ID_REC","HA_Crop", "STRIP","HA_Fallow","id_obj")

#Make easier the join later
df$CAMPANYA <- as.character(df$CAMPANYA) 
df$ID_REC <- as.character(df$ID_REC) 
df$id_obj <- as.character(df$id_obj)

# Combine information strips

for (i in 1:length(id_dup)){
  
  tmp <- d11[which(d11$ID_REC == id_dup[i]), ] # Select the duplicated rows for each franja
  tmp_spread <- spread(tmp, PROD_NOM, HA_DEC, fill = NA, drop = TRUE) # Spread to isolate the size of crop/fallows
  
  tmp_spread$HA_Crop <- sum(tmp_spread$CROP[which(!is.na(tmp_spread$CROP))]) # Sum in case there is more than one crop and place
  tmp_spread$HA_Fallow <- sum(tmp_spread$FALLOW[which(!is.na(tmp_spread$FALLOW))]) # number in unique variable for field HA_crop/fallow
  
  tmp_unique <- tmp_spread[-which(duplicated(tmp_spread$ID_REC)), 
                           -which(colnames(tmp_spread) %in% c("CROP", "FALLOW"))] #Remove duplicates and crop/fallow cols
  df[i, ] <- tmp_unique
}

# Identify strips (franjas): 1/0
for (i in 1:nrow(df)){
  if (df$HA_Crop[i] > 0 & df$HA_Fallow[i] > 0) {df$STRIP[i] <- 1}
}

# Join select id_rec from sigpac that are AES (id from df)
# Load sigpac2011
sp11 <- readOGR("C:/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/SIGPAC_2011", layer = "SP11_REC_clip")

id_aes <- unique(df$ID_REC) # Take IDs from aes (dun)

sp11_aes <- sp11[which(sp11@data$ID_REC %in% id_aes), ] #clip only aes
head(sp11_aes@data)

# Join data from df (size of fields and strips from dun)
sp11_aes@data <- sp11_aes@data[ ,which(colnames(sp11_aes@data) %in% c("ID_REC", "HA","PENDENT"))]
sp11_aes@data <-left_join(sp11_aes@data, df, by = "ID_REC")

setwd("C:/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/Only AES")
writeOGR(sp11_aes,
         dsn = "C:/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/Only AES", layer = "AES_2011", driver = "ESRI Shapefile")


# ---- 2014 ----

#Load data
setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014")
d14 <- read.csv("DUN2014.csv", sep = ";") #DUN
#sp14 <- readOGR("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/SIGPAC_2014/SP14_clip.shp")
sp14 <- readOGR("C:/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/SIGPAC_2014/SP14_clip.shp")

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

# Upload the projected dun layer
#d15 <- readOGR("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS/AES/Codi364_DUN2015-2017/364/364/364_2015.shp")
d15 <- readOGR("C:/OneDrive/PhD/Second chapter/Data/GIS/AES/Codi364_DUN2015-2017/Only AES/364", layer = "364_2015_proj50")

head(d15@data)

# Limpiar columnas

d15@data <- d15@data[, which(colnames(d15@data) %in% c("OBJECTID", "CAMPANYA", "ID_REC", "M2_SP", "HA_SP", "HA_DEC", 
                                        "US_SP", "PENDENT", "PROD_NUM", "PROD_NOM", "AJUTS", "ZEPA"))]

d15@data$HA_Crop <- 0
d15@data$STRIP <- 0
d15@data$HA_Fallow <- 0

d15@data$PROD_NOM <- as.character(d15@data$PROD_NOM) #To simplify: Any crop = CROP and any fallow = FALLOW
unique(d15@data$PROD_NOM)
d15@data$PROD_NOM[which(d15@data$PROD_NOM %in% c("ORDI", "BLAT TOU", "SÃˆGOL", "TRITICALE", "CIVADA"))] <- "CROP"
d15@data$PROD_NOM[which(d15@data$PROD_NOM %in% c("GUARET SIE/ SUP. LLIURE SEMBRA", "GUARET NO SIE/ SUP. LLIURE SE*"))] <- "FALLOW"

d15@data$AJUTS <- as.character(d15@data$AJUTS) #Make easier the join later
d15@data$CAMPANYA <- as.character(d15@data$CAMPANYA) 
d15@data$ID_REC <- as.character(d15@data$ID_REC) 
d15@data$US_SP <- as.character(d15@data$US_SP) 

#Check duplicated

dup_all <- d15@data[which(duplicated(d15@data$ID_REC)), ] #Identify duplicates (franjas)
dup_all$ID_REC <- as.character(dup_all$ID_REC)
id_dup <- unique(dup_all$ID_REC) # List ids duplicated
dup <- d15@data[which(d15@data$ID_REC %in% id_dup), ] # Only to check

#Data frame to store

df <- as.data.frame(matrix(0, nrow = length(id_dup), ncol = 13))
colnames(df) <- c("OBJECTID", "CAMPANYA","ID_REC","M2_SP","HA_SP","US_SP",
                  "PENDENT","PROD_NUM","AJUTS","ZEPA","HA_Crop","STRIP","HA_Fallow")

df$AJUTS <- as.character(df$AJUTS) #Make easier the join later
df$CAMPANYA <- as.character(df$CAMPANYA) 
df$ID_REC <- as.character(df$ID_REC) 
df$US_SP <- as.character(df$US_SP) 

# Combine information strips

for (i in 1:length(id_dup)){
  
  tmp <- d15@data[which(d15@data$ID_REC == id_dup[i]), ] # Select the duplicated rows for each franja
  tmp_spread <- spread(tmp, PROD_NOM, HA_DEC, fill = NA, drop = TRUE) # Spread to isolate the size of crop/fallows
  
  tmp_spread$HA_Crop <- sum(tmp_spread$CROP[which(!is.na(tmp_spread$CROP))]) # Sum in case there is more than one crop and place
  tmp_spread$HA_Fallow <- sum(tmp_spread$FALLOW[which(!is.na(tmp_spread$FALLOW))]) # number in unique variable for field HA_crop/fallow
  
  tmp_unique <- tmp_spread[-which(duplicated(tmp_spread$ID_REC)), 
                           -which(colnames(tmp_spread) %in% c("CROP", "FALLOW"))] #Remove duplicates and crop/fallow cols

  df[i, ] <- tmp_unique
}

# Identify strips (franjas): 1/0
for (i in 1:nrow(df)){
  if (df$HA_Crop[i] > 0 & df$HA_Fallow[i] > 0) {df$STRIP[i] <- 1}
}

# Make df having the same columns (add prod_nom and ha_dec).
df$PROD_NOM <- "DUP"
df$HA_DEC <- NA

# df joins information from strips and the ones that are duplicated and are not strips (eg: two different crops in same rec)


# Join with spatial information

#1. Join information to layer with only duplicated IDs

d15_dup <- d15[which(d15@data$ID_REC %in% id_dup), ] # Layer with only ids that are duplicated
d15_dup@data$ID_REC <- as.character(d15_dup@data$ID_REC)
d15_dup@data <- d15_dup@data[ ,c(2,3)] # Keep only the ID_REC, OBJ_ID y CAMPANYA (Año)
d15_dup <- d15_dup[-which(duplicated(d15_dup@data$ID_REC)), ] #Remove duplicated

# Join df - d15_dup
d15_dup@data <- left_join(d15_dup@data, df, by = "ID_REC") # Spatial features duplicated

#Fix Campanya name to make it fit
d15_dup@data <- d15_dup@data[ ,-4]
colnames(d15_dup@data)[1] <- "CAMPANYA"

# 2. Merge d15_dup with the rest of the layer without duplicates
d15_nodup <- d15[-which(d15@data$ID_REC %in% id_dup), ] # Layer with only ids that are NOT duplicated
head(d15_nodup@data)
head(d15_dup@data) # Same names. Ready to merge



#d15_full <- union(d15_dup,d15_nodup)
d15_full2 <- bind(d15_dup,d15_nodup)


setwd("C:/OneDrive/PhD/Second chapter/Data/GIS/AES/Codi364_DUN2015-2017/Only AES/364")
writeOGR(d15_full2,dsn = "C:/OneDrive/PhD/Second chapter/Data/GIS/AES/Codi364_DUN2015-2017/Only AES/364", layer = "AES_2015_proj", driver = "ESRI Shapefile")



# ---- 2016 ----

# Upload the projected dun layer
#d16 <- readOGR("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/GIS/AES/Codi364_DUN2015-2017/364/364/364_2015.shp")
d16 <- readOGR("C:/OneDrive/PhD/Second chapter/Data/GIS/AES/Codi364_DUN2016-2017/Only AES/364", layer = "364_2016_proj50")

head(d16@data)

# Strip digitalized! So decide what Im gonna do as for 2017...


# Limpiar columnas

d16@data <- d16@data[, which(colnames(d16@data) %in% c("OBJECTID", "CAMPANYA", "ID_REC", "M2_SP", "HA_SP", "HA_DEC", 
                                                       "US_SP", "PENDENT", "PROD_NUM", "PROD_NOM", "AJUTS", "ZEPA"))]

d16@data$HA_Crop <- 0
d16@data$STRIP <- 0
d16@data$HA_Fallow <- 0

d16@data$PROD_NOM <- as.character(d16@data$PROD_NOM) #To simplify: Any crop = CROP and any fallow = FALLOW
unique(d16@data$PROD_NOM)
d16@data$PROD_NOM[which(d16@data$PROD_NOM %in% c("ORDI", "BLAT TOU", "SÃˆGOL", "TRITICALE", "CIVADA"))] <- "CROP"
d16@data$PROD_NOM[which(d16@data$PROD_NOM %in% c("GUARET SIE/ SUP. LLIURE SEMBRA", "GUARET NO SIE/ SUP. LLIURE SE*"))] <- "FALLOW"

d16@data$AJUTS <- as.character(d16@data$AJUTS) #Make easier the join later
d16@data$CAMPANYA <- as.character(d16@data$CAMPANYA) 
d16@data$ID_REC <- as.character(d16@data$ID_REC) 
d16@data$US_SP <- as.character(d16@data$US_SP) 

#Check duplicated

dup_all <- d16@data[which(duplicated(d16@data$ID_REC)), ] #Identify duplicates (franjas)
dup_all$ID_REC <- as.character(dup_all$ID_REC)
id_dup <- unique(dup_all$ID_REC) # List ids duplicated
dup <- d16@data[which(d16@data$ID_REC %in% id_dup), ] # Only to check

#Data frame to store

df <- as.data.frame(matrix(0, nrow = length(id_dup), ncol = 13))
colnames(df) <- c("OBJECTID", "CAMPANYA","ID_REC","M2_SP","HA_SP","US_SP",
                  "PENDENT","PROD_NUM","AJUTS","ZEPA","HA_Crop","STRIP","HA_Fallow")

df$AJUTS <- as.character(df$AJUTS) #Make easier the join later
df$CAMPANYA <- as.character(df$CAMPANYA) 
df$ID_REC <- as.character(df$ID_REC) 
df$US_SP <- as.character(df$US_SP) 

# Combine information strips

for (i in 1:length(id_dup)){
  
  tmp <- d16@data[which(d16@data$ID_REC == id_dup[i]), ] # Select the duplicated rows for each franja
  tmp_spread <- spread(tmp, PROD_NOM, HA_DEC, fill = NA, drop = TRUE) # Spread to isolate the size of crop/fallows
  
  tmp_spread$HA_Crop <- sum(tmp_spread$CROP[which(!is.na(tmp_spread$CROP))]) # Sum in case there is more than one crop and place
  tmp_spread$HA_Fallow <- sum(tmp_spread$FALLOW[which(!is.na(tmp_spread$FALLOW))]) # number in unique variable for field HA_crop/fallow
  
  tmp_unique <- tmp_spread[-which(duplicated(tmp_spread$ID_REC)), 
                           -which(colnames(tmp_spread) %in% c("CROP", "FALLOW"))] #Remove duplicates and crop/fallow cols
  
  df[i, ] <- tmp_unique
}

# Identify strips (franjas): 1/0
for (i in 1:nrow(df)){
  if (df$HA_Crop[i] > 0 & df$HA_Fallow[i] > 0) {df$STRIP[i] <- 1}
}

# Make df having the same columns (add prod_nom and ha_dec).
df$PROD_NOM <- "DUP"
df$HA_DEC <- NA

# df joins information from strips and the ones that are duplicated and are not strips (eg: two different crops in same rec)


# Join with spatial information

#1. Join information to layer with only duplicated IDs

d16_dup <- d16[which(d16@data$ID_REC %in% id_dup), ] # Layer with only ids that are duplicated
d16_dup@data$ID_REC <- as.character(d16_dup@data$ID_REC)
d16_dup@data <- d16_dup@data[ ,c(2,3)] # Keep only the ID_REC, OBJ_ID y CAMPANYA (Año)
d16_dup <- d16_dup[-which(duplicated(d16_dup@data$ID_REC)), ] #Remove duplicated

# Join df - d16_dup
d16_dup@data <- left_join(d16_dup@data, df, by = "ID_REC") # Spatial features duplicated

#Fix Campanya name to make it fit
d16_dup@data <- d16_dup@data[ ,-4]
colnames(d16_dup@data)[1] <- "CAMPANYA"

# 2. Merge d16_dup with the rest of the layer without duplicates
d16_nodup <- d16[-which(d16@data$ID_REC %in% id_dup), ] # Layer with only ids that are NOT duplicated
head(d16_nodup@data)
head(d16_dup@data) # Same names. Ready to merge



#d16_full <- union(d16_dup,d16_nodup)
d16_full2 <- bind(d16_dup,d16_nodup)


setwd("C:/OneDrive/PhD/Second chapter/Data/GIS/AES/Codi364_DUN2016-2017/Only AES/364")
writeOGR(d16_full2,dsn = "C:/OneDrive/PhD/Second chapter/Data/GIS/AES/Codi364_DUN2016-2017/Only AES/364", layer = "AES_2016_proj", driver = "ESRI Shapefile")

# ---- 2017 ----
d17 <- readOGR("C:/OneDrive/PhD/Second chapter/Data/GIS/AES/Codi364_DUN2015-2017/Only AES/364", layer = "364_2017_proj50")

head(d17@data)

# Limpiar columnas

d17@data <- d17@data[, which(colnames(d17@data) %in% c("OBJECTID", "CAMPANYA", "ID_REC", "M2_SP", "HA_SP", "HA_DEC", 
                                                       "US_SP", "PENDENT", "PROD_NOM", "AJUTS", "ZEPA"))]
d17@data$HA_Crop <- 0
d17@data$STRIP <- 0
d17@data$HA_Fallow <- 0

d17@data$PROD_NOM <- as.character(d17@data$PROD_NOM) #To simplify: Any crop = CROP and any fallow = FALLOW
unique(d17@data$PROD_NOM)
d17@data$PROD_NOM[which(d17@data$PROD_NOM %in% c("ORDI", "BLAT TOU", "TRITICALE", "CIVADA"))] <- "CROP"
d17@data$PROD_NOM[which(d17@data$PROD_NOM %in% c("GUARET SIE/ SUP. LLIURE SEMBRA", "GUARET NO SIE/ SUP. LLIURE SE*"))] <- "FALLOW"

d17@data$AJUTS <- as.character(d17@data$AJUTS) #Make easier the join later
d17@data$CAMPANYA <- as.character(d17@data$CAMPANYA) 
d17@data$ID_REC <- as.character(d17@data$ID_REC) 
d17@data$US_SP <- as.character(d17@data$US_SP) 

#Check duplicated

dup_all <- d17@data[which(duplicated(d17@data$ID_REC)), ] #Identify duplicates (franjas)
dup_all$ID_REC <- as.character(dup_all$ID_REC)
id_dup <- unique(dup_all$ID_REC) # List ids duplicated
dup <- d17@data[which(d17@data$ID_REC %in% id_dup), ] # Only to check
dup <- arrange(dup, desc(ID_REC))

#Data frame to store

df <- as.data.frame(matrix(0, nrow = length(id_dup), ncol = 13))
colnames(df) <- c("OBJECTID", "CAMPANYA","ID_REC","M2_SP","HA_SP","US_SP",
                  "PENDENT","PROD_NUM","AJUTS","ZEPA","HA_Crop","STRIP","HA_Fallow")

df$AJUTS <- as.character(df$AJUTS) #Make easier the join later
df$CAMPANYA <- as.character(df$CAMPANYA) 
df$ID_REC <- as.character(df$ID_REC) 
df$US_SP <- as.character(df$US_SP) 

# Combine information strips

for (i in 1:length(id_dup)){
  
  tmp <- d17@data[which(d17@data$ID_REC == id_dup[i]), ] # Select the duplicated rows for each franja
  tmp_spread <- spread(tmp, PROD_NOM, HA_DEC, fill = NA, drop = TRUE) # Spread to isolate the size of crop/fallows
  
  tmp_spread$HA_Crop <- sum(tmp_spread$CROP[which(!is.na(tmp_spread$CROP))]) # Sum in case there is more than one crop and place
  tmp_spread$HA_Fallow <- sum(tmp_spread$FALLOW[which(!is.na(tmp_spread$FALLOW))]) # number in unique variable for field HA_crop/fallow
  
  tmp_unique <- tmp_spread[-which(duplicated(tmp_spread$ID_REC)), 
                           -which(colnames(tmp_spread) %in% c("CROP", "FALLOW"))] #Remove duplicates and crop/fallow cols
  
  df[i, ] <- tmp_unique
}

# Identify strips (franjas): 1/0
for (i in 1:nrow(df)){
  if (df$HA_Crop[i] > 0 & df$HA_Fallow[i] > 0) {df$STRIP[i] <- 1}
}

# Make df having the same columns (add prod_nom and ha_dec).
df$PROD_NOM <- "DUP"
df$HA_DEC <- NA
# df joins information from strips and the ones that are duplicated and are not strips 
#(eg: two different crops in same rec)

# Join with spatial information

#1. Join information to layer with only duplicated IDs

d17_dup <- d17[which(d17@data$ID_REC %in% id_dup), ] # Layer with only ids that are duplicated
d17_dup@data$ID_REC <- as.character(d17_dup@data$ID_REC)
d17_dup@data <- d17_dup@data[ ,c(2,3)] # Keep only the ID_REC, OBJ_ID y CAMPANYA (Año)
d17_dup <- d17_dup[-which(duplicated(d17_dup@data$ID_REC)), ] #Remove duplicated

# Join df - d17_dup
d17_dup@data <- left_join(d17_dup@data, df, by = "ID_REC") # Spatial features duplicated

#Fix Campanya name to make it fit
d17_dup@data <- d17_dup@data[ ,-4]
colnames(d17_dup@data)[1] <- "CAMPANYA"

# 2. Merge d17_dup with the rest of the layer without duplicates
d17_nodup <- d17[-which(d17@data$ID_REC %in% id_dup), ] # Layer with only ids that are NOT duplicated
head(d17_nodup@data)
head(d17_dup@data) # Same names. Ready to merge

#d17_full <- union(d17_dup,d17_nodup)
d17_full2 <- bind(d17_dup,d17_nodup)

setwd("C:/OneDrive/PhD/Second chapter/Data/GIS/AES/Codi364_DUN2015-2017/Only AES/364")
writeOGR(d17_full2,
         dsn = "C:/OneDrive/PhD/Second chapter/Data/GIS/AES/Codi364_DUN2015-2017/Only AES/364", 
         layer = "AES_2017", driver = "ESRI Shapefile")
# The strip of 2017 is digitalized!! So this layer is wrong, 
# check what I should do (keeping strip spatial info or merging to be consistent in analyses)
