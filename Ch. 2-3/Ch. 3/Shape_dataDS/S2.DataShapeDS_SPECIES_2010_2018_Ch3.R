

# Check and select species that will be used for the community model (CH 3)

rm(list=ls())

library(dplyr)
library(tidyr)

setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/Data")

dat <- read.csv("DataDS_ch3_allsp.csv", sep = ",")

#### Remove species that are MIGRANT and therefore are not link to the transect and the  scale of the study ####

setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Second chapter/Data")

all <- read.csv("index_selec_communities_FSP_DG_GB.csv", sep = ";")
mig <- all[which(all$NO.FS.DG.GB == 1),]
sp_mig <- as.character(unique(mig$Species)) #Vector with species to delete

dat <- dat[-which(dat$Species %in% sp_mig), ] 

#### CHECK species with low sample size ####

# ----1. Number of detections ----

freq <- as.data.frame(xtabs(~Species + Year, dat))
freq <- spread(freq,Year, Freq) # Number of detections (ds observations) of each species per year in all transects (nrows per species and year)
freq$remove <- 0 # To export data set and select the ones that are present in enough transects

freq$total_detections <- rowSums(freq[ ,c(2:10)])

freq <- freq[-which(freq$total_detections == 0), ]

setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/Data")
# write.csv(freq, "NumberDetect_sp_year_ch3.csv")
# In the last chapter the threshold was 20 detections per year per species. But in this one not, because I loose information of relevant
# scarce species.

# 2. ---- Proportion of transects occupied by each species yearly ----

# Number of transects per year

d_transects <- dat[ ,which(colnames(dat) %in% c("Year", "T_Y", "transectID"))]
d_transects <- d_transects[which(!duplicated(d_transects$T_Y)), ]

trans <- aggregate(transectID ~ Year, data = d_transects, FUN = length)
colnames(trans)[2] <- "Number of transects"
n_transects <- trans$`Number of transects`

# Create dataframe with proportion

year <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
sp <- unique(dat$Species)

prop_sp <- as.data.frame(matrix(ncol = length(year), nrow = length(sp)))
rownames(prop_sp) <- sp
colnames(prop_sp) <- year

for (i in 1:length(sp)){
  d_sp <- dat[which(dat$Species == sp[i]), ]
  for (t in 1:9){
    d_t <- d_sp[which(d_sp$Year == year[t]), ]
    d_prop <- (length(unique(d_t$T_Y))/n_transects[t])*100
    prop_sp[i,t] <- d_prop
  }
}

d_prop <- (length(unique(d_sp$T_Y))/1083)*100

prop_sp <- round(prop_sp,2)

prop_sp$Mean_proportion <- round(apply(prop_sp,1,mean), 2) # average of species occupancy in transects 

setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/Data")
#write.csv(prop_sp, "Proptransects_sp_year_ch3.csv")

#### Remove species very very scarce (less than 10 detections in total and 0.7% of transects occupied per year) ####

# Join both tables together
prop_sp$Species <- rownames(prop_sp)
tab <- left_join(freq, prop_sp, by = "Species")

setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/Data")
#write.csv(tab, "infospecies_ch3.csv")

dat_scarce <- read.csv("infospecies_ch3_final.csv", sep = ";")
scarce <- dat_scarce[which(dat_scarce$remove == 1),]
sp_scarce <- as.character(unique(scarce$Species)) #Vector with species to delete
dat <- dat[-which(dat$Species %in% sp_scarce), ]

#### Join species in sp. where it is hard to tell the difference ####

# JOIN SPECIES FROM STURNUS (STVUL + STUNI = STSSP)
dat[which(dat$Species == "STSSP"), ]
dat$Species[which(dat$Species == "STVUL")] <- "STSSP" 
dat$Species[which(dat$Species == "STUNI")] <- "STSSP" 

# REMOVE GASSP BECAUSE GATHE AND GACRI HAVE A LOT OF OBSERVATIONS
dat <- dat[-which(dat$Species == "GASSP"), ]

##### Check detection curves ####

# Make plot all species to compare Seen vS. Heard
setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/Explore")

pdf("df_all_observations_b5_allsp.pdf")
spec <- dat_scarce$Species[-which(dat_scarce$Species %in% sp_scarce)] # Order descending of proportion of transects occupied
spec <- spec[-(which(spec %in% c("STVUL", "STUNI", "GASSP")))]
#spec <- unique(dat$Species)

par(mfrow = c(4,3))
for (i in 1:length(spec)){
  hist(dat$distance[which(dat$Species %in% spec[i])], breaks = c(0,25,50,99,200,500),
       main = paste(spec[i], "- Distances"), col = "grey", freq = FALSE) 
}
dev.off()

# It seems like there are few important species that have a "bad detection curve"
# (TERAX and BUOED) 

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






aggregate( ~ transectID + Species + Year , data = dat, FUN = 'sum') # Number of individuals of the species in the year
sum_year <- spread(sum_year, Species, Count)

