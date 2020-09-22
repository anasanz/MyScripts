
##################################################################################
############      EXPLORE DATA TORTOLA SOCC AMPLIADO      ########################
##################################################################################

library(rgdal)
library(sp)
library(raster)
library(dplyr)
library(tidyr)
library(splitstackshape)

# --- Localización espacial de transectos ---- #

# Todos los transectos
tr_all <- readOGR("D:/PhD/Otros/Tórtola", layer = "SOCC_2020_v4")

# Base de datos socc ampliado (con distance sampling)
setwd("D:/PhD/Otros/Tórtola")
ds <- read.csv("2020_04.csv", sep = ";")
colnames(ds)[1] <- "Itinerari"
id_itinerari <- unique(ds$Itinerari)

tr_ds <- tr_all[which(tr_all$Itinerari %in% id_itinerari), ]
proj4string(tr_all)
writeOGR(tr_ds,"D:/PhD/Otros/Tórtola", layer = "DS_SOCC_2020", driver = "ESRI Shapefile")

# ---- Exploración datos DS ---- #

freq <- ds %>% count(AnySOCC, Periode)

# Observadores

obs <- unique(ds$Observador)

freq_obs1 <- ds %>% count(AnySOCC, Observador)
freq_obs2 <- ds %>% count(Observador)

# Select observers that did more than 5 census
obs_more5 <- freq_obs2[which(freq_obs2$n > 5), ]
obs_more5 <- unique(obs_more5$Observador)



# Put in ds format (1 row per observation)
datds <- gather(ds, Banda, count, banda1:banda3)
datds <- datds[which(datds$count != 0), ]
datds <- datds[ ,-c(5,6,8)]

dat <- expandRows(datds, 'count')

# Detection curve

dat$distance <- NA # Medium point of each bin except in bin 4

for (i in 1:nrow(dat)){
  if (dat$Banda[i] == "banda1") {dat$distance[i] = 12.5} # 0-25
  else if (dat$Banda[i] == "banda2") {dat$distance[i] = 62.5} # 25-100
  else  {dat$distance[i] = 550} # 100 - 1000
}

hist(dat$distance,breaks = c(0,25,99,1000), main = "Detection curve", col = "grey", freq = FALSE) 

# Detection curve per observer (with more than 5 census)
dat_obs5 <- dat[which(dat$Observador %in% obs_more5), ] # Se pierden 196 censos en total de 4748


par(mfrow = c())
for (i in 1:length(obs_more5)){
     hist(dat_obs5$distance[which(dat_obs5$Observador %in% obs_more5[i])],breaks = c(0,25,99,1000),
          main = paste(obs_more5[i], "- Distances"), col = "grey", freq = FALSE)
}

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


