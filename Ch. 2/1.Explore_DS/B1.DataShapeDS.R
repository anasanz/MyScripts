
# Get data into shape for DS analysis (for package DISTANCE)

rm(list=ls())

library(dplyr)
library(stringr)

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data")
dat <- read.csv("DataDS.csv", sep = ";")
dat$Especie <- as.character(dat$Especie)
dat <- dat[ ,-4]

# ---- Column names ----
names(dat)
colnames(dat)[which(colnames(dat) == "Transecte_detall.Id_transecte_detall")] <- "Sample.Label"
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

#2. Keep only the last 2 digits 
library(stringr)
for (i in 1:nrow(dat)){ 
  dat$Num_transecte[i] <- str_sub(dat$Num_transecte[i], start = -2)
}

# Create variable by pasting it
for (i in 1:nrow(dat)){ 
  dat$transectID[i] <- paste(dat$Region.Label[i],dat$Num_transecte[i], sep = "")
}

# ---- Distance ----
setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data")
band <- read.csv("Banda.csv", sep = ";")
colnames(band)[1] <- "Banda"

dat <- left_join(dat, band, by = "Banda") # Joind bands (distbegin/distend)

dat <- dat[which(!is.na(dat$Banda)), ]

dat$distance <- NA # Medium point of each bin except in bin 4

for (i in 1:nrow(dat)){
  if (dat$Banda[i] == 1) {dat$distance[i] = 12.5}
 else if (dat$Banda[i] == 2) {dat$distance[i] = 37.5}
  else if (dat$Banda[i] == 3) {dat$distance[i] = 75}
  else  {dat$distance[i] = 150} 
}

# ---- Repeated observations ----
# There are few transects that have 2 census in the same year-season.

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
all <- read.csv("index_selec_communities_FSP_DG_GB.csv", sep = ";")
mig <- all[which(all$NO.FS.DG.GB == 1),]
sp_mig <- as.character(unique(mig$Species)) #Vector with species to delete

dat <- dat[-which(dat$Species %in% sp_mig), ] # No repeated observations

# Remove species that are very very scarce (around less than 10 observations per year)
scarce <- all[which(all$remove == 1),]
sp_scarce <- as.character(unique(scarce$Species)) #Vector with species to delete
dat <- dat[-which(dat$Species %in% sp_scarce), ]

write.csv(dat,"DataDS_ready.csv")







# ---- Covariate: Minuts from sunrise (future) ----
# ---- Area region ----
# Depends on the study question: where do I want to calculate abundance?
