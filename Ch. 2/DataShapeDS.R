
# Get data into shape for DS analysis (for package DISTANCE)

rm(list=ls())

library(dplyr)

setwd("~/Second chapter/Data")
dat <- read.csv("DataDS.csv", sep = ";")
dat$Especie <- as.character(dat$Especie)
dat <- dat[ ,-4]

# ---- Column names ----
names(dat)
colnames(dat)[which(colnames(dat) == "Transecte_detall_Id_transecte_detall")] <- "Sample.Label"
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

# ---- Covariate: Minuts from sunrise (future) ----
# ---- Distance ----
setwd("~/Second chapter/Data")
band <- read.csv("Banda.csv", sep = ";")
colnames(band)[1] <- "Banda"

dat <- left_join(dat, band, by = "Banda") # Joind bands (distbegin/distend)

dat <- dat[which(!is.na(dat$Banda)), ]

dat$distance <- NA # Medium point of each bin except in bin 4

for (i in 1:nrow(dat)){
  if (dat$Banda[i] == 1) {dat$distance[i] = 12.5}
 else if (dat$Banda[i] == 2) {dat$distance[i] = 37.5}
  else if (dat$Banda[i] == 3) {dat$distance[i] = 75}
  else  {dat$distance[i] = 100} # Here is more than 100, so Im not sure which measure I should put
}

write.csv(dat,"DataDS_ready.csv")



# ---- Area region ----
# Depends on the study question: where do I want to calculate abundance?
