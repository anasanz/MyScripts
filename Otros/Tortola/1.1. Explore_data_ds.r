
##################################################################################
############      EXPLORE DATA TORTOLA SOCC AMPLIADO      ########################
##################################################################################

rm(list=ls())

library(rgdal)
library(sp)
library(raster)
library(dplyr)
library(tidyr)
library(splitstackshape)

# --- Localización espacial de transectos ---- #

# Todos los transectos
tr_all <- readOGR("D:/PhD/Otros/Tórtola/Data", layer = "SOCC_2020_v4")

# Base de datos socc ampliado (con distance sampling)
setwd("D:/PhD/Otros/Tórtola/Data")
ds <- read.csv("2020_04.csv", sep = ";")
colnames(ds)[1] <- "Itinerari"
id_itinerari <- unique(ds$Itinerari)

tr_ds <- tr_all[which(tr_all$Itinerari %in% id_itinerari), ]
proj4string(tr_all)
#writeOGR(tr_ds,"D:/PhD/Otros/Tórtola", layer = "DS_SOCC_2020", driver = "ESRI Shapefile")

# ---- Exploración datos DS ---- #

# Problem: rows are not independent
# Check average group size detected during farmdindis
setwd("D:/PhD/Third chapter/Data")
farm <- read.csv("DataDS_ch3_15_19_READY_FIXED.csv") 
farm <- farm[which(farm$Species == "STTUR"), ]
observations <- xtabs(~ Count, farm) 
hist(farm$Count, breaks = 12, at = c(0,1,2,3,4,5,6,7,8,9,10,11,12))
mean(farm$Count)

farm2 <- farm[which(farm$Count != 12), ]
observations2 <- xtabs(~ Count, farm2) 
mean(farm2$Count)

# Proporción escuchadas/vistas
nrow(farm[which(farm$Obs_type == "V"), ])/nrow(farm)

# El tamaño medio de grupo detectado en farmdindis no es muy alto.
# No podremos estimar abundancias, y violar la asumcion de datos independencia
# Puede hacer que los estimates sean "overly precise". 
# Pero podemos dar valores de "Probabilidad de declive"

# Put in ds format (1 row per observation)
datds <- gather(ds, Banda, count, banda1:banda3)
datds <- datds[which(datds$count != 0), ]
datds <- datds[ ,-c(5,6,8)]

dat <- expandRows(datds, 'count')


# Explorar periodo: Elegir periodo 2 porque ya están todas

freq <- dat %>% count(AnySOCC, Periode)
dat <- dat[which(dat$Periode == 2), -3]

# Cambiar columnas

colnames(dat) <- c("Site", "Year", "Observer", "Section", "Bin")

# Crear nueva columna: Itinerari_seccion
dat$site_sec <- paste(dat$Site, dat$Section, sep = "_")
dat$site_year <- paste(dat$Site, dat$Year, sep = "_")
length(unique(dat$site_year)) # 767 transect-year

# ---- OBSERVER ---- #

obs <- unique(dat$Observer)

# Cuantos transectos ha hecho cada observador

  data_obs <- dat %>%
    group_by(Observer) %>%
    summarise(n_transects = n_distinct(site_year))
  
  freq_obs <- arrange(data_obs, n_transects)
  freq_obs$Observer <- factor(freq_obs$Observer, levels = freq_obs$Observer)
  
  barplot(freq_obs$n_transects ~ freq_obs$Observer, las = 2)
 
  # Select observers that did less than 5 census
  obs_less5 <- freq_obs[which(freq_obs$n_transects < 5), ]
  obs_less5 <- unique(obs_less5$Observer)
  dat_less5 <- dat[which(dat$Observer %in% obs_less5), ] # Perdemos 354 observaciones, pero para meter observador como variable es necesario

# Remove
dat <- dat[-which(dat$Observer %in% obs_less5), ] # I DON'T REMOVE HERE, CHECK WHEN THE ABSENCES ARE ADDED
length(unique(dat$site_year)) # 633 transect-year (se pierden 134 transect-year)

# ---- DETECTION CURVE ---- #

dat$distance <- NA # Medium point of each bin except in bin 4

for (i in 1:nrow(dat)){
  if (dat$Bin[i] == "banda1") {dat$distance[i] = 12.5} # 0-25
  else if (dat$Bin[i] == "banda2") {dat$distance[i] = 62.5} # 25-100
  else  {dat$distance[i] = 550} # 100 - 1000
}

hist(dat$distance,breaks = c(0,25,99,1000), main = "Detection curve", col = "grey", freq = FALSE) 

# Detection curve per observer (with more than 5 census)

obs <- unique(dat$Observer) 

par(mfrow = c())
for (i in 1:length(obs)){
     hist(dat$distance[which(dat$Observer %in% obs[i])],breaks = c(0,25,99,1000),
          main = paste(obs[i], "- Distances"), col = "grey", freq = FALSE)
}


# ---- YEARS ---- #

data_years <- dat %>%
  group_by(Year) %>%
  summarise(n_transects = n_distinct(site_year))

# El año que hay menos transectos es el 2003, y hay 9 (54 sections)
# Yo haría un análisis para todos los años 2002-2019, y otro para 2010 - 2019 (e incluso podemos probar 2007-2019)
# DEPENDE DE LAS VARIABLES TAMBIÉN

# Save
setwd("D:/PhD/Otros/Tórtola/Data")
write.csv(dat, "tortola_ds.csv")
