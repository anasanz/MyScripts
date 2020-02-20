# Check and select species that will be used for the community model (CH 3)

rm(list=ls())

library(dplyr)
library(tidyr)

setwd("D:/PhD/Third chapter/Data")

dat <- read.csv("DataDS_ch3_allsp_1519_FIXED.csv", sep = ",")

#### Remove species that are MIGRANT and therefore are not link to the transect and the  scale of the study ####

setwd("D:/PhD/Second chapter/Data")

all <- read.csv("index_selec_communities_FSP_DG_GB.csv", sep = ";")
mig <- all[which(all$NO.FS.DG.GB == 1),]
sp_mig <- as.character(unique(mig$Species)) #Vector with species to delete

dat <- dat[-which(dat$Species %in% sp_mig), ] 


#### Remove very scarce (less than 20 detections in total, which is the lower threshold to include PTORI) ####
setwd("D:/PhD/Third chapter/Data")
dat_info <- read.csv("infospecies_ch3_1519_FIXED_FINAL.csv", sep = ";")
scarce <- dat_info[which(dat_info$remove_veryscarce == 1), ]
sp_scarce <- as.character(unique(scarce$Species)) #Vector with species to delete
dat <- dat[-which(dat$Species %in% sp_scarce), ]

#### Join species in sp. where it is hard to tell the difference ####

# JOIN SPECIES FROM STURNUS (STVUL + STUNI = STSSP)
dat[which(dat$Species == "STSSP"), ]
dat$Species[which(dat$Species == "STVUL")] <- "STSSP" 
dat$Species[which(dat$Species == "STUNI")] <- "STSSP" 

# REMOVE GASSP BECAUSE GATHE AND GACRI HAVE A LOT OF OBSERVATIONS
dat <- dat[-which(dat$Species == "GASSP"), ]

#### Remove species with a bad detection curve ####

dc <- dat_info[which(dat_info$remove_detectioncurve == 1), ]
sp_dc <- as.character(unique(dc$Species)) #Vector with species to delete
dat <- dat[-which(dat$Species %in% sp_dc), ]

#### Remove species with no biological meaning related to the study question ####
bio <- dat_info[which(dat_info$remove_biologicalsense == 1), ]
sp_bio <- as.character(unique(bio$Species)) #Vector with species to delete
dat <- dat[-which(dat$Species %in% sp_bio), ]


#### Include/Check TERAX_F and TERAX_M because they have different requirements (just in case) ####

dat$Species2 <- dat$Species
dat$Species2 <- as.character(dat$Species2)
dat$Species2[which(dat$Species2 == "TERAX" & dat$Sex == 0)] <- "TERAX_ind"
dat$Species2[which(dat$Species2 == "TERAX" & dat$Sex == 1)] <- "TERAX_M"
dat$Species2[which(dat$Species2 == "TERAX" & dat$Sex == 2)] <- "TERAX_F"

setwd("D:/PhD/Third chapter/Data")
write.csv(dat,"DataDS_ch3_15_19_READY_FIXED.csv") #Final data set ready for analyzing


