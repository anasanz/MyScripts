#### Get data into shape for DS analysis ###
# 20-2-19
# Third chapter: 
# Because of mistake in data base, repeat analyses from 2015 - 2019



rm(list=ls())

library(dplyr)
library(stringr)

setwd("D:/PhD/Third chapter/Data")
dat <- read.csv("Data_DS_fixed.csv", sep = ";")

dat$Especie <- as.character(dat$Especie)
dat <- dat[ ,-4] 
colnames(dat)[which(colnames(dat) == "Transecte_detall_Id_transecte_detall")] <- "Id_transecte_detall" # To make it equal to 2018



# ---- Column names ----
names(dat)
colnames(dat)[which(colnames(dat) == "Id_transecte_detall")] <- "Sample.Label"
colnames(dat)[which(colnames(dat) == "Codi_seca")] <- "Region.Label"
colnames(dat)[which(colnames(dat) == "Any")] <- "Year"
colnames(dat)[which(colnames(dat) == "Especie")] <- "Species"
colnames(dat)[which(colnames(dat) == "Nombre")] <- "Count"
colnames(dat)[which(colnames(dat) == "Sexe")] <- "Sex"
colnames(dat)[which(colnames(dat) == "Us")] <- "Crop_type"
colnames(dat)[which(colnames(dat) == "Tipus_observacio")] <- "Obs_type"
colnames(dat)[which(colnames(dat) == "Hora_inici")] <- "Start_time"
colnames(dat)[which(colnames(dat) == "Observador")] <- "Observer"
colnames(dat)[which(colnames(dat) == "Vent")] <- "Wind"
colnames(dat)[which(colnames(dat) == "Nuvolositat")] <- "Clouds"
colnames(dat)[which(colnames(dat) == "Temperatura")] <- "Temp"
dat$Effort <- 500

# ----- Create variable transectID, than matches with the code of the GIS layers (i.e., two digits: 09) ----

#1. Add a 0 before the transect number
for (i in 1:nrow(dat)){ 
  dat$Num_transecte[i] <- paste(0,dat$Num_transecte[i], sep = "")
}

#2. Keep only the last 2 digits (or 3 in the case of the transects that contain 100)

for (i in 1:nrow(dat)) { 
  cent <- substr(dat$Num_transecte[i], 4,4)
  cent <- as.numeric(cent) # NA if it doesnt have 4 digits
  if(is.na(cent)) { # if is NA (has 3 digits)
    dat$Num_transecte[i] <- str_sub(dat$Num_transecte[i], start = -2) # Keep the last 2
  } else { dat$Num_transecte[i] <- str_sub(dat$Num_transecte[i], start = -3)} # Otherwise, keep the last 3
}


# Create variable by pasting it
for (i in 1:nrow(dat)){ 
  dat$transectID[i] <- paste(dat$Region.Label[i],dat$Num_transecte[i], sep = "")
}

# ---- Distance ----

setwd("D:/PhD/Third chapter/Data")
band <- read.csv("Banda.csv", sep = ";")
colnames(band)[1] <- "Banda"

dat <- left_join(dat, band, by = "Banda") # Joind bands (distbegin/distend)

dat <- dat[which(!is.na(dat$Banda)), ]

dat$distance <- NA # Medium point of each bin except in bin 4

for (i in 1:nrow(dat)){
  if (dat$Banda[i] == 1) {dat$distance[i] = 12.5}
  else if (dat$Banda[i] == 2) {dat$distance[i] = 37.5}
  else if (dat$Banda[i] == 3) {dat$distance[i] = 75}
  else if (dat$Banda[i] == 4) {dat$distance[i] = 150}
  else  {dat$distance[i] = 350} # Considering that in theory the truncation distance is 500 m (no 1000)
}

# ---- Repeated observations ----

# There are few transects that have 2 census in the same year-season.
# Because I have joined the bin5 it doesn't work with the indexes from 2010-2018 without bin5. 
# But it works from when I didn't delete any observations

for (i in 1:nrow(dat)){ 
  dat$T_Y[i] <- paste(dat$transectID[i],dat$Year[i], sep = "_")
}

trans <- dat[!duplicated(dat$Sample.Label), which(colnames(dat) %in% c("Sample.Label", "T_Y"))]
trans_rep <- trans[which(duplicated(trans$T_Y)), ]

# These are the ones without bin 5
#rem <- c(122, 181, 165, 110, 197, 125, 178, 160, 170, 192, 253, 
#1357, 804, 809, 243, 1203, 711)

# These are the ones from before, that work now as well:
# DUPLICATES: The sample label changed when removing observations related bin4. The previous
# label (data not modified is listed in blue)
# In some its because census were repeated in january, april and may. Take the ones of late April/May (In AL):
# Remove sample.label: 122, 198, 178, 114, 216, 131, 194, 172, 184, 210, 281
# In others, 2 of the same season
# Remove sample.label: 1505, 1737, 1744. Take the ones I could modify
# In others, different weather conditions. Take good coditions
# Remove sample.label: 268, 1350, 1594
# Comparison with the new data (modified is in excelfile DataDS_comparedup)
# Remove the one repeated from 2019: 

# These are the one that I deleted to analyse the data before realizing about the mistake:
#rem <- c(122, 198, 178, 114, 216, 131, 194, 172, 184, 210, 281,
#         1505, 1737, 1744, 268, 1350, 1594)

# Now, the numbers are the same but some duplicates have been corrected, so there are less:
rem <- c(122, 198, 178, 114, 216, 131, 194, 172, 184, 210, 281, 1594, 1737, 1744)

dat <- dat[-which(dat$Sample.Label %in% rem), ] # No repeated observations

trans2 <- dat[!duplicated(dat$Sample.Label), which(colnames(dat) %in% c("Sample.Label", "T_Y"))]
trans_rep <- trans2[which(duplicated(trans2$T_Y)), ]


# ---- Remove transects that are irrigated (and therefore have very different conditions) ----
setwd("D:/PhD/Second chapter/Data")
irri <- read.csv("TransecteAnyReg.csv", sep = ";")
colnames(irri)[2] <- "Num_transecte"
colnames(irri)[1] <- "Region.Label"

# CREATE TRANSECT ID VARIABLE
# Add a 0 before the transect number
for (i in 1:nrow(irri)){ 
  irri$Num_transecte[i] <- paste(0,irri$Num_transecte[i], sep = "")}
# Keep only the last 2 digits 
library(stringr)
for (i in 1:nrow(irri)){ 
  irri$Num_transecte[i] <- str_sub(irri$Num_transecte[i], start = -2)}
# Create variable by pasting it
for (i in 1:nrow(irri)){ 
  irri$transectID[i] <- paste(irri$Region.Label[i],irri$Num_transecte[i], sep = "")}

# Remove the ones irrigated all years
irri_all <- irri$transectID[which(irri$Regadio == 1)]
dat <- dat[-which(dat$transectID %in% irri_all), ]

# Remove the ones irrigated the year it changed (report remove = 1 for the ones to remove)

irri_change <- irri[which(!is.na(irri$X1er.año.cambio)), ]
irri_change_ID <- irri_change$transectID
irri_change_year <- irri_change$X1er.año.cambio
dat$remove <- NA

for (i in 1:nrow(dat)){
  if (sum(dat$transectID[i] == irri_change_ID)>0) { # For the transects that changed from irrigation
    tmp_change <- irri_change_year[which(irri_change_ID == dat$transectID[i])] # Year of change
    
    if(dat$Year[i] >= tmp_change){
      dat[i,which(colnames(dat) %in% "remove")] <- 1 # Data from that year gets a 1 (to be removed)
    }}
}

# Remove the ones (1) and column remove

dat <- dat[-which(dat$remove == 1), ]
dat <- dat[ ,-which(colnames(dat) %in% "remove")]

# Remove AR (Only has greening and has a very different structure than the rest, with fruit trees)
# When we were not planning to use Greening we were removing AL because it didnt have almost any measure
# (we left BA because it had more AES). In this case we don't remove AL because we are going to test greening


dat <- dat[-which(dat$Region.Label %in% c("AR")), ]


# Co-variate Zone 

unique(dat$Region.Label)
dat$Zone <- NA
dat$Zone[dat$Region.Label == "BA"] <- "OC"
dat$Zone[dat$Region.Label == "BM"] <- "OR"
dat$Zone[dat$Region.Label == "SI"] <- "OR"
dat$Zone[dat$Region.Label == "AF"] <- "OC"
dat$Zone[dat$Region.Label == "BE"] <- "OR"
dat$Zone[dat$Region.Label == "GR"] <- "OC"

# FIX OBSERVATION CO-VARIATES
# Temperature: mistakes typing
dat$Temp[which(dat$Temp == 0)] <- 10
dat$Temp[which(dat$Temp == 100)] <- 10

# Na (cojo el valor de el transecto anterior realizado o algo fiable)
unique(dat$Temp)
dat[which(is.na(dat$Temp)), ] 
dat$Temp[which(dat$T_Y == "AF09_2018")] <- 20
dat$Temp[which(dat$T_Y == "GR13_2011")] <- 10

#Wind
unique(dat$Wind)
dat[which(is.na(dat$Wind)), ] 
dat$Wind[which(dat$T_Y == "BM06_2010")] <- 1

# FOR THE CONSERVATION_MEASURES PAPER, select the YEARS OF STUDY (2015 - 2019)
dat <- dat[which(dat$Year %in% c(2015, 2016, 2017, 2018, 2019)), ]

setwd("D:/PhD/Third chapter/Data")
write.csv(dat,"DataDS_ch3_allsp_1519_FIXED.csv") # Data set with everything fixed EXCEPT SPECIES








