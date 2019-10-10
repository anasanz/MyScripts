
# Get data into shape for DS analysis (for package DISTANCE)

rm(list=ls())

library(dplyr)
library(stringr)

#setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data")
setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/Data")

dat <- read.csv("DataDS_bin5.csv", sep = ";")
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
setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/Data")
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
# Even when joining with 2018 is okay because the sample label of the repeated ones is the same
# and there is no repeated transects in 2018

for (i in 1:nrow(dat)){ 
  dat$T_Y[i] <- paste(dat$transectID[i],dat$Year[i], sep = "_")
}

trans <- dat[!duplicated(dat$Sample.Label), which(colnames(dat) %in% c("Sample.Label", "T_Y"))]
trans_rep <- trans[which(duplicated(trans$T_Y)), ]

# DUPLICATES: The sample label changed when removing observations related bin4. The previous
# label (data not modified is listed in blue)
# In some its because census were repeated in january, april and may. Take the ones of late April/May (In AL):
# Remove sample.label: 122, 198, 178, 114, 216, 131, 194, 172, 184, 210, 281
# In others, 2 of the same season
# Remove sample.label: 1505, 1737, 1744. Take the ones I could modify
# In others, different weather conditions. Take good coditions
# Remove sample.label: 268, 1350, 1594
# Comparison with the new data (modified is in excelfile DataDS_comparedup)
rem <- c(122, 181, 165, 110, 197, 125, 178, 160, 170, 192, 253, 
         1357, 804, 809, 243, 1203, 711)
dat <- dat[-which(dat$Sample.Label %in% rem), ] # No repeated observations

trans2 <- dat[!duplicated(dat$Sample.Label), which(colnames(dat) %in% c("Sample.Label", "T_Y"))]
trans_rep <- trans2[which(duplicated(trans2$T_Y)), ]

# ---- Remove species ----

# Remove species that are MIGRANT and therefore are not link to the transect and the
# scale of the study
setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Second chapter/Data")
all <- read.csv("index_selec_communities_FSP_DG_GB.csv", sep = ";")
mig <- all[which(all$NO.FS.DG.GB == 1),]
sp_mig <- as.character(unique(mig$Species)) #Vector with species to delete

dat <- dat[-which(dat$Species %in% sp_mig), ] # No repeated observations

# Remove species that are very very scarce (around less than 10 observations per year)
scarce <- all[which(all$remove == 1),]
sp_scarce <- as.character(unique(scarce$Species)) #Vector with species to delete
dat <- dat[-which(dat$Species %in% sp_scarce), ]

# ---- Join observations from PTALC and PTORI ---- --> TRY NOT TO IN THIS CHAPTER

#for (i in 1:nrow(dat)){
#  if (dat$Species[i] == "PTALC" | dat$Species[i] == "PTORI")
#  {dat[i, which(colnames(dat) %in% "Species")] <- "SAND" }
#}


# ---- Remove transects that are irrigated (and therefore have very different conditions) ----
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

# Make plot all species to compare Seen vS. Heard
setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/Explore")

pdf("df_all_observations_b5.pdf")
spec <- unique(dat$Species)

par(mfrow = c(4,3))
for (i in 1:length(spec)){
  hist(dat$distance[which(dat$Species %in% spec[i])], breaks = c(0,25,50,99,200,500),
       main = paste(spec[i], "- Distances"), col = "grey", freq = FALSE) 
}
dev.off()
# It seems like there are few important species that have a "bad detection curve"
# (TERAX and BUOED) I will see if joining the bin 1 - bin 2 and creating one last bin (up to 1 km)
# the detection curves improve
# The detection curve improves a bit, but it is till weird for BUOED and TERAX

# Join bin 1 - bin 2
dat$Banda_new <- NA
dat$Banda_new[dat$Banda == 5] <- 4
dat$Banda_new[dat$Banda == 4] <- 3
dat$Banda_new[dat$Banda == 3] <- 2
dat$Banda_new[dat$Banda == 2] <- 1
dat$Banda_new[dat$Banda == 1] <- 1

dat$distance_new <- NA 
dat$distance_new[dat$Banda_new == 4] <- 350
dat$distance_new[dat$Banda_new == 3] <- 150
dat$distance_new[dat$Banda_new == 2] <- 75
dat$distance_new[dat$Banda_new == 1] <- 25


setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/Explore")
pdf("df_all_observations_3bins_b5.pdf")
spec <- unique(dat$Species)

par(mfrow = c(4,3))
for (i in 1:length(spec)){
  hist(dat$distance_new[which(dat$Species %in% spec[i])],breaks = c(0,50,99,200, 500),
       main = paste(spec[i], "- Distances"), col = "grey", freq = FALSE) 
}
dev.off()



# Remove AL and AR (Don't have any of the measures in which we are interested)

dat <- dat[-which(dat$Region.Label %in% c("AL", "AR")), ]


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


# JOIN SPECIES FROM STURNUS (STVUL + STUNI = STSSP). Because it is very difficult to tell the difference
dat[which(dat$Species == "STSSP"), ]
dat$Species[which(dat$Species == "STVUL")] <- "STSSP" 
dat$Species[which(dat$Species == "STUNI")] <- "STSSP" 


#write.csv(dat,"DataDS_ready_ALL.csv") # ALL because includes 2018. This is the one that I analyze
