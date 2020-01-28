
# FIND AREA AES

rm(list=ls())
library(rgdal)
library(dplyr)
library(raster)
library(tidyr)

# ---- 2010 ----
# Load dun2010 (with measure 312)

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/AES/2010-2014/DUN/ID_REC_AES")

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
sp10 <- readOGR("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/AES/2010-2014/SIGPAC_2010", layer = "sp10_clip")

# Same name df = sigpac
colnames(df)[colnames(df) == "ID_REC"] <- "CODI_REC"
id_aes <- unique(df$CODI_REC) # Take IDs from 

sp10_aes <- sp10[which(sp10@data$CODI_REC %in% id_aes), ] #clip only aes
head(sp10_aes@data)
# Join data from df (size of fields and strips from dun)
sp10_aes@data <- sp10_aes@data[ ,which(colnames(sp10_aes@data) %in% c("CODI_REC", "SUP_M2", "SUP_HA", 
                                                                      "PENDENT", "Shape_Leng", "Shape_Area"))]
sp10_aes@data <-left_join(sp10_aes@data, df, by = "CODI_REC")

setwd("C:/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/Only AES")
writeOGR(sp10_aes,
         dsn = "C:/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/Only AES", layer = "AES_2010", driver = "ESRI Shapefile")

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


# ---- 2012 ----
# Load dun2012 (with measure 312)

setwd("C:/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/DUN/ID_REC_AES")

d12 <- read.csv("IDREC_AES_2012.csv", sep = ";")

# Limpiar columnas

colnames(d12)[which(colnames(d12) == "SuperfComprovada")] <- "HA_DEC"
colnames(d12)[which(colnames(d12) =="RecinteSIGPAC")] <- "ID_REC"
colnames(d12)[which(colnames(d12) == "Descripció.ajut")] <- "PROD_NOM"
colnames(d12)[which(colnames(d12) == "Campanya")] <- "CAMPANYA"


d12 <- d12[, which(colnames(d12) %in% c("CAMPANYA", "ID_REC", "HA_DEC", "PROD_NOM"))]

d12$HA_Crop <- 0
d12$STRIP <- 0
d12$HA_Fallow <- 0
d12$id_obj <- seq(1,nrow(d12),1)

d12$PROD_NOM <- as.character(d12$PROD_NOM) #To simplify: Any crop = CROP and any fallow = FALLOW
unique(d12$PROD_NOM)
d12$PROD_NOM[which(d12$PROD_NOM %in% "ZEPA: cereal hivern")] <- "CROP"
d12$PROD_NOM[which(d12$PROD_NOM %in% "ZEPAs:guaret mediamb")] <- "FALLOW"

#Make easier the join later
d12$CAMPANYA <- as.character(d12$CAMPANYA) 
d12$ID_REC <- as.character(d12$ID_REC) 
d12$HA_DEC <- as.numeric(d12$HA_DEC)
d12$id_obj <- as.character(d12$id_obj)

#Check duplicated

dup_all <- d12[which(duplicated(d12$ID_REC)), ] #Identify duplicates (franjas)
dup_all$ID_REC <- as.character(dup_all$ID_REC)
id_dup <- unique(dup_all$ID_REC) # List ids duplicated
dup <- d12[which(d12$ID_REC %in% id_dup), ] # Only to check
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
  
  tmp <- d12[which(d12$ID_REC == id_dup[i]), ] # Select the duplicated rows for each franja
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
# Load sigpac2012
sp12 <- readOGR("C:/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/SIGPAC_2012", layer = "SP12_clip")

id_aes <- unique(df$ID_REC) # Take IDs from aes (dun)

sp12_aes <- sp12[which(sp12@data$ID_REC %in% id_aes), ] #clip only aes
head(sp12_aes@data)

# Join data from df (size of fields and strips from dun)
sp12_aes@data <- sp12_aes@data[ ,which(colnames(sp12_aes@data) %in% c("ID_REC", "HA","PENDENT","SHAPE_Leng","SHAPE_Area"))]
sp12_aes@data <-left_join(sp12_aes@data, df, by = "ID_REC")

setwd("C:/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/Only AES")
writeOGR(sp12_aes,
         dsn = "C:/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/Only AES", layer = "AES_2012", driver = "ESRI Shapefile")

# ---- 2013 ----
# Load dun2013 (with measure 312)

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/AES/2010-2014/DUN/ID_REC_AES")

d13 <- read.csv("ID_REC_AES_2013.csv", sep = ";")

# Limpiar columnas

colnames(d13)[which(colnames(d13) == "A_DEC")] <- "HA_DEC"
colnames(d13)[which(colnames(d13) == "ID_PAR")] <- "ID_REC"

d13 <- d13[, which(colnames(d13) %in% c("ID_REC", "HA_DEC", "HA_SP", 
                                        "US_SP", "PROD_NOM"))]

d13$HA_Crop <- 0
d13$STRIP <- 0
d13$HA_Fallow <- 0
d13$id_obj <- seq(1,nrow(d13),1)

d13$PROD_NOM <- as.character(d13$PROD_NOM) #To simplify: Any crop = CROP and any fallow = FALLOW
unique(d13$PROD_NOM)
d13$PROD_NOM[which(d13$PROD_NOM %in% c("ORDI", "BLAT TOU", "ALTRES CEREALS", "TRITICALE", "CIVADA", "SÈGOL"))] <- "CROP"
d13$PROD_NOM[which(d13$PROD_NOM %in% c("SUPERFÍCIE LLIURE DE SEMBRA/GU","GUARETS TRADICIONALS", "GUARETS MEDIAMBIENTAL COMPLEME", "GUARETS MEDIAMBIENTAL R.CEE 20" ))] <- "FALLOW"

#Make easier the join later
d13$ID_REC <- as.character(d13$ID_REC) 
d13$US_SP <- as.character(d13$US_SP)
d13$HA_DEC <- as.numeric(d13$HA_DEC)
d13$id_obj <- as.character(d13$id_obj)

#Check duplicated

dup_all <- d13[which(duplicated(d13$ID_REC)), ] #Identify duplicates (franjas)
dup_all$ID_REC <- as.character(dup_all$ID_REC)
id_dup <- unique(dup_all$ID_REC) # List ids duplicated
dup <- d13[which(d13$ID_REC %in% id_dup), ] # Only to check
dup <- arrange(dup,desc(ID_REC))
du <- arrange(dup, desc(HA_DEC))
dup[which(dup$ID_REC == "25010:0:0:6:12:13"), ]

#Data frame to store

df <- as.data.frame(matrix(0, nrow = length(id_dup), ncol = 7))
colnames(df) <- c("ID_REC", "HA_SP", 
                  "US_SP","HA_Crop", "STRIP","HA_Fallow","id_obj")

#Make easier the join later
df$ID_REC <- as.character(df$ID_REC) 
df$US_SP <- as.character(df$US_SP)
df$id_obj <- as.character(df$id_obj)


# Combine information strips

for (i in 1:length(id_dup)){
  
  tmp <- d13[which(d13$ID_REC == id_dup[i]), ] # Select the duplicated rows for each franja
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
# Load sigpac2013
sp13 <- readOGR("C:/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/SIGPAC_2013", layer = "SP13_clip")
head(sp13@data)
# Same name df = sigpac
id_aes <- unique(df$ID_REC) # Take IDs from 

sp13_aes <- sp13[which(sp13@data$ID_REC %in% id_aes), ] #clip only aes
head(sp13_aes@data)
# Join data from df (size of fields and strips from dun)
sp13_aes@data <- sp13_aes@data[ ,which(colnames(sp13_aes@data) %in% c("ID_REC", "HA", 
                                                                      "PENDENT", "Shape_Leng", "Shape_Area"))]
sp13_aes@data <-left_join(sp13_aes@data, df, by = "ID_REC")

setwd("C:/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/Only AES")
writeOGR(sp13_aes,
         dsn = "C:/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/Only AES", layer = "AES_2013", driver = "ESRI Shapefile")


# ---- 2014 ----
# Load dun2014 (with measure 312)

setwd("C:/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/DUN/ID_REC_AES")

d14 <- read.csv("ID_REC_AES_2014.csv", sep = ";")

# Limpiar columnas

colnames(d14)[which(colnames(d14) == "A_DEC")] <- "HA_DEC"
colnames(d14)[which(colnames(d14) == "ID_PAR")] <- "ID_REC"

d14 <- d14[, which(colnames(d14) %in% c("ID_REC", "HA_DEC", "HA_SP", 
                                        "US_SP", "PROD_NOM"))]

d14$HA_Crop <- 0
d14$STRIP <- 0
d14$HA_Fallow <- 0
d14$id_obj <- seq(1,nrow(d14),1)

d14$PROD_NOM <- as.character(d14$PROD_NOM) #To simplify: Any crop = CROP and any fallow = FALLOW
unique(d14$PROD_NOM)
d14$PROD_NOM[which(d14$PROD_NOM %in% c("ORDI", "BLAT TOU", "ALTRES CEREALS", "TRITICALE", "CIVADA", "SÈGOL"))] <- "CROP"
d14$PROD_NOM[which(d14$PROD_NOM %in% c("SUPERFÍCIE LLIURE DE SEMBRA/GU","GUARETS TRADICIONALS", "GUARETS MEDIAMBIENTAL COMPLEME", "GUARETS MEDIAMBIENTAL R.CEE 20" ))] <- "FALLOW"

#Make easier the join later
d14$ID_REC <- as.character(d14$ID_REC) 
d14$US_SP <- as.character(d14$US_SP)
d14$HA_DEC <- as.numeric(d14$HA_DEC)
d14$id_obj <- as.character(d14$id_obj)

#Check duplicated

dup_all <- d14[which(duplicated(d14$ID_REC)), ] #Identify duplicates (franjas)
dup_all$ID_REC <- as.character(dup_all$ID_REC)
id_dup <- unique(dup_all$ID_REC) # List ids duplicated
dup <- d14[which(d14$ID_REC %in% id_dup), ] # Only to check
dup <- arrange(dup,desc(ID_REC))

#Data frame to store

df <- as.data.frame(matrix(0, nrow = length(id_dup), ncol = 7))
colnames(df) <- c("ID_REC", "HA_SP", 
                  "US_SP","HA_Crop", "STRIP","HA_Fallow","id_obj")

#Make easier the join later
df$ID_REC <- as.character(df$ID_REC) 
df$US_SP <- as.character(df$US_SP)
df$id_obj <- as.character(df$id_obj)


# Combine information strips

for (i in 1:length(id_dup)){
  
  tmp <- d14[which(d14$ID_REC == id_dup[i]), ] # Select the duplicated rows for each franja
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
# Load sigpac2014
sp14 <- readOGR("C:/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/SIGPAC_2014", layer = "SP14_clip_project2")
head(sp14@data)
# Same name df = sigpac
id_aes <- unique(df$ID_REC) # Take IDs from 

sp14_aes <- sp14[which(sp14@data$ID_REC %in% id_aes), ] #clip only aes
head(sp14_aes@data)
# Join data from df (size of fields and strips from dun)
sp14_aes@data <- sp14_aes@data[ ,which(colnames(sp14_aes@data) %in% c("ID_REC", "HA", 
                                                                      "PENDENT", "Shape_Leng", "Shape_Area"))]
sp14_aes@data <-left_join(sp14_aes@data, df, by = "ID_REC")

setwd("C:/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/Only AES")
writeOGR(sp14_aes,
         dsn = "C:/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/Only AES", layer = "AES_2014", driver = "ESRI Shapefile")


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

# STRIP DIGITALIZED! SO USE LAYER (CHECK) AND JUST HOMOGENIZE NAMES CROP/FALLOW
d16 <- readOGR("C:/OneDrive/PhD/Second chapter/Data/GIS/AES/Codi364_DUN2015-2017/Only AES/365", layer = "365_2016_proj")

# Homogenize names crop/fallow
d16@data$PROD_NOM <- as.character(d16$PROD_NOM) #To simplify: Any crop = CROP and any fallow = FALLOW
unique(d16@data$PROD_NOM)
d16@data$PROD_NOM[which(d16@data$PROD_NOM %in% c("ORDI", "BLAT TOU", "MILL", "TRITICALE", "CIVADA"))] <- "CROP"
d16@data$PROD_NOM[which(d16@data$PROD_NOM %in% c("GUARET SIE/ SUP. LLIURE SEMBRA","GUARET NO SIE/ SUP. LLIURE SE*"))] <- "FALLOW"

# Select relevant columns
d16@data <- d16@data[ ,which(colnames(d16@data) %in% c("OBJECTID", "CAMPANYA", "ID_REC", "M2_SP", "HA_SP", "HA_DEC", "PROD_NOM",
                                                                      "PENDENT", "Shape_Leng", "Shape_Area"))]

# Save layer
setwd("C:/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/Only AES")
writeOGR(d16,
         dsn = "C:/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/Only AES", layer = "AES_2016", driver = "ESRI Shapefile")

# ---- 2017 ----
# STRIP DIGITALIZED! SO USE LAYER (CHECK) AND JUST HOMOGENIZE NAMES CROP/FALLOW
d17 <- readOGR("C:/OneDrive/PhD/Second chapter/Data/GIS/AES/Codi364_DUN2015-2017/Only AES/364", layer = "364_2017_proj50")

# Homogenize names crop/fallow
d17@data$PROD_NOM <- as.character(d17$PROD_NOM) #To simplify: Any crop = CROP and any fallow = FALLOW
unique(d17@data$PROD_NOM)
d17@data$PROD_NOM[which(d17@data$PROD_NOM %in% c("ORDI", "BLAT TOU", "TRITICALE", "CIVADA"))] <- "CROP"
d17@data$PROD_NOM[which(d17@data$PROD_NOM %in% c("GUARET SIE/ SUP. LLIURE SEMBRA","GUARET NO SIE/ SUP. LLIURE SE*"))] <- "FALLOW"

# Select relevant columns
d17@data <- d17@data[ ,which(colnames(d17@data) %in% c("OBJECTID", "CAMPANYA", "ID_REC", "M2_SP", "HA_SP", "HA_DEC", "PROD_NOM",
                                                       "PENDENT", "Shape_Leng", "Shape_Area"))]

# Save layer
setwd("C:/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/Only AES")
writeOGR(d17,
         dsn = "C:/OneDrive/PhD/Second chapter/Data/GIS/AES/2010-2014/Only AES", layer = "AES_2017", driver = "ESRI Shapefile")

# ---- 2018 ----
# STRIP DIGITALIZED! SO USE LAYER (CHECK) AND JUST HOMOGENIZE NAMES CROP/FALLOW
d18 <- readOGR("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/AES/shp_AES_2018_2019", layer = "M102018_EPSG23031")

# Homogenize names crop/fallow
d18@data$PROD_NOM <- as.character(d18$PROD_NOM) #To simplify: Any crop = CROP and any fallow = FALLOW
unique(d18@data$PROD_NOM2)
d18@data$PROD_NOM2 <- "CROP"
d18@data$PROD_NOM2[which(d18@data$PROD_NOM %in% c("GUARET SIE/ SUP. LLIURE SEMBRA","GUARET NO SIE/ SUP. LLIURE SE*"))] <- "FALLOW"

# Select relevant columns
d18@data <- d18@data[ ,which(colnames(d18@data) %in% c("OBJECTID", "CAMPANYA", "ID_REC", "M2_SP", "HA_SP", "HA_DEC", "PROD_NOM2",
                                                       "PENDENT", "Shape_Leng", "Shape_Area"))]
colnames(d18@data)[10] <- "PROD_NOM"
# Save layer
setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/AES/Only AES/EPSG23031")
writeOGR(d18,
         dsn = "C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/AES/Only AES", layer = "AES_2018_EPSG23031", driver = "ESRI Shapefile")

# ---- 2019 ----
# STRIP DIGITALIZED! SO USE LAYER (CHECK) AND JUST HOMOGENIZE NAMES CROP/FALLOW
d19 <- readOGR("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/AES/shp_AES_2018_2019", layer = "M102019_EPSG23031")

# Homogenize names crop/fallow
d19@data$PROD_NOM <- as.character(d19$PROD_NOM) #To simplify: Any crop = CROP and any fallow = FALLOW
unique(d19@data$PROD_NOM2)
d19@data$PROD_NOM2 <- "CROP"
d19@data$PROD_NOM2[which(d19@data$PROD_NOM %in% c("GUARET SIE/ SUP. LLIURE SEMBRA","GUARET NO SIE/ SUP. LLIURE SE*"))] <- "FALLOW"

# Select relevant columns
d19@data <- d19@data[ ,which(colnames(d19@data) %in% c("OBJECTID", "CAMPANYA", "ID_REC", "M2_SP", "HA_SP", "HA_DEC", "PROD_NOM2",
                                                       "PENDENT", "Shape_Leng", "Shape_Area"))]
colnames(d19@data)[10] <- "PROD_NOM"

# Save layer
setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/AES/Only AES/EPSG23031")
writeOGR(d19,
         dsn = "C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/AES/Only AES", layer = "AES_2019_EPSG23031", driver = "ESRI Shapefile")


# ---- Correct units layers 2010, 2013, 2014 ----
aes10 <- readOGR("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/AES/Only AES", layer = "AES_2010_EPSG23031")
aes13 <- readOGR("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/AES/Only AES", layer = "AES_2013_EPSG23031")
aes14 <- readOGR("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/AES/Only AES", layer = "AES_2014_EPSG23031")

aes10@data <- aes10@data[ ,-which(colnames(aes10@data) %in% "HA_SP")]
colnames(aes10@data)[colnames(aes10@data) == "HA_SP2"] <- "HA_SP" #Because it had already a field with this name

# Correct separatledly per year because I dont know how to index this loop
# aes10
aes10@data$HA_SP <- aes10@data$HA_SP/1000000
aes10@data$HA_Crop <- aes10@data$HA_Crop/10000
aes10@data$HA_Fallow <- aes10@data$HA_Fallow/10000
#writeOGR(aes10, dsn = "C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/AES/2010-2014/Only AES", layer = "AES_2010_EPSG23031", driver = "ESRI Shapefile", overwrite_layer = TRUE)

# aes13
aes13@data$HA_SP <- aes13@data$HA_SP/1000000
aes13@data$HA_Crop <- aes13@data$HA_Crop/10000
aes13@data$HA_Fallow <- aes13@data$HA_Fallow/10000
#writeOGR(aes13, dsn = "C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/AES/2010-2014/Only AES", layer = "AES_2013_EPSG23031", driver = "ESRI Shapefile", overwrite_layer = TRUE)

# aes14
aes14@data$HA_SP <- aes14@data$HA_SP/1000000
aes14@data$HA_Crop <- aes14@data$HA_Crop/10000
aes14@data$HA_Fallow <- aes14@data$HA_Fallow/10000
#writeOGR(aes14, dsn = "C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS/AES/2010-2014/Only AES", layer = "AES_2014_EPSG23031", driver = "ESRI Shapefile", overwrite_layer = TRUE)


