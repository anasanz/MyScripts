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

#### CHECK species with low sample size ####

# ---- 1. Number of detections ----

freq <- as.data.frame(xtabs(~Species + Year, dat))
freq <- spread(freq,Year, Freq) # Number of detections (ds observations) of each species per year in all transects (nrows per species and year)
freq$remove <- 0 # To export data set and select the ones that are present in enough transects

freq$total_detections <- rowSums(freq[ ,c(2:6)])

freq <- freq[-which(freq$total_detections == 0), ]

setwd("D:/PhD/Third chapter/Data")
write.csv(freq, "NumberDetect_sp_year_ch3_1519_FIXED.csv")
# In the last chapter the threshold was 20 detections per year per species. But in this one not, because I loose information of relevant
# scarce species.

# ---- 2. Proportion of transects occupied by each species yearly ----

# Number of transects per year

d_transects <- dat[ ,which(colnames(dat) %in% c("Year", "T_Y", "transectID"))]
d_transects <- d_transects[which(!duplicated(d_transects$T_Y)), ]

trans <- aggregate(transectID ~ Year, data = d_transects, FUN = length)
colnames(trans)[2] <- "Number of transects"
n_transects <- trans$`Number of transects`

# Create dataframe with proportion

year <- c(2015, 2016, 2017, 2018, 2019)
sp <- unique(dat$Species)

prop_sp <- as.data.frame(matrix(ncol = length(year), nrow = length(sp)))
rownames(prop_sp) <- sp
colnames(prop_sp) <- year

for (i in 1:length(sp)){
  d_sp <- dat[which(dat$Species == sp[i]), ]
  for (t in 1:5){
    d_t <- d_sp[which(d_sp$Year == year[t]), ]
    d_prop <- (length(unique(d_t$T_Y))/n_transects[t])*100
    prop_sp[i,t] <- d_prop
  }
}

#d_prop <- (length(unique(d_sp$T_Y))/1083)*100

prop_sp <- round(prop_sp,2)

prop_sp$Mean_proportion <- round(apply(prop_sp,1,mean), 2) # average of species occupancy in transects 

setwd("D:/PhD/Third chapter/Data")
write.csv(prop_sp, "Proptransects_sp_year_ch3_1519_FIXED.csv")

# Join both tables together
prop_sp$Species <- rownames(prop_sp)
tab <- left_join(freq, prop_sp, by = "Species")

setwd("D:/PhD/Third chapter/Data")
write.csv(tab, "infospecies_ch3_1519_FIXED.csv")


#####
#### Remove very scarce (less than 30 detections in total, which is the lower threshold to include PTORI) ####

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

#### Remove species with no biological meaning related to the study question ####
bio <- dat_info[which(dat_info$remove_biologicalsense == 1), ]
sp_bio <- as.character(unique(bio$Species)) #Vector with species to delete
dat <- dat[-which(dat$Species %in% sp_bio), ]

##### Check detection curves ####

setwd("D:/PhD/Third chapter/Explore")

spec <- as.character(dat_info$Species)
spec <- spec[-which(spec %in% sp_scarce)] # Order descending of proportion of transects occupied
spec <- spec[-which(spec %in% sp_bio)]

pdf("df_all_observations_allsp_1519.pdf")

par(mfrow = c(4,3))
for (i in 1:length(spec)){
  hist(dat$distance[which(dat$Species %in% spec[i])], breaks = c(0,25,50,99,200,500),
       main = paste(spec[i], "- Distances"), col = "grey", freq = FALSE) 
}
dev.off()


#### Remove species with a bad detection curve ####

setwd("D:/PhD/Third chapter/Data")
dat_info <- read.csv("infospecies_ch3_1519_FIXED_FINAL.csv", sep = ";")

dc <- dat_info[which(dat_info$remove_detectioncurve == 1), ]
sp_dc <- as.character(unique(dc$Species)) #Vector with species to delete
dat <- dat[-which(dat$Species %in% sp_dc), ]



# Plot all observations
setwd("D:/PhD/Third chapter/Explore")

pdf("df_all_observations_goodsp_1519.pdf")
spec_no_order <- unique(dat$Species)
spec <- dat_info$Species[which(dat_info$Species %in% spec_no_order)]

par(mfrow = c(4,3))
for (i in 1:length(spec)){
  hist(dat$distance[which(dat$Species %in% spec[i])], breaks = c(0,25,50,99,200,500),
       main = paste(spec[i], "- Distances"), col = "grey", freq = FALSE, xlab = " ") 
}
dev.off()

# Plot only seen

setwd("D:/PhD/Third chapter/Explore")
pdf("df_all_observations_goodsp_1519_seen.pdf")

par(mfrow = c(4,3))
for (i in 1:length(spec)){
  hist(dat$distance[which(dat$Species %in% spec[i] & dat$Obs_type == "V")], breaks = c(0,25,50,99,200,500),
       main = paste(spec[i], "- Distances"), col = "grey", freq = FALSE, xlab = " ") 
}
dev.off()

# Plot only heard

setwd("D:/PhD/Third chapter/Explore")
pdf("df_all_observations_goodsp_1519_heard.pdf")

par(mfrow = c(4,3))
for (i in 1:length(spec)){
  if (sum(dat$distance[which(dat$Species %in% spec[i] & dat$Obs_type == "S")]) == 0)
    plot(1,xlab = " ", ylab = " ", main = spec[i]) else hist(dat$distance[which(dat$Species %in% spec[i] & dat$Obs_type == "S")], breaks = c(0,25,50,99,200,500),
                                                             main = paste(spec[i], "- Distances"), col = "grey", freq = FALSE, xlab = " ") 
}
dev.off()

# Finally because the detection curves of the heard observations are only increasing, we can not include Heard vS Seen as a co-variate 
# in sigma. Therefore, we will pool all the observations together and analyze it as a whole with the hazard rate detection function.

#  TERAX AND BUOED PROBLEM: Doesn't occur with data from 2015 - 2019!!
#### Include/Check TERAX_F and TERAX_M because they have different requirements ####

dat$Species2 <- dat$Species
dat$Species2 <- as.character(dat$Species2)
dat$Species2[which(dat$Species2 == "TERAX" & dat$Sex == 0)] <- "TERAX_ind"
dat$Species2[which(dat$Species2 == "TERAX" & dat$Sex == 1)] <- "TERAX_M"
dat$Species2[which(dat$Species2 == "TERAX" & dat$Sex == 2)] <- "TERAX_F"

par(mfrow = c(1, 3))
hist(dat$distance[which(dat$Species2  == "TERAX_ind")], breaks = c(0,25,50,99,200,500),
     main = "TERAX_ind - Distances", col = "grey", freq = FALSE, xlab = " ")
hist(dat$distance[which(dat$Species2  == "TERAX_M")], breaks = c(0,25,50,99,200,500),
     main = "TERAX_M - Distances", col = "grey", freq = FALSE, xlab = " ")
hist(dat$distance[which(dat$Species2  == "TERAX_F")], breaks = c(0,25,50,99,200,500),
     main = "TERAX_F - Distances", col = "grey", freq = FALSE, xlab = " ")



# ---- Check species with bad/good pvalues (when running models) to see where could be the problem ----

# MECAL (BAD BP) 

# All obs
par(mfrow = c(1, 1))
hist(dat$distance[which(dat$Species  == "MECAL")], breaks = c(0,25,50,99,200,500),
     main = "MECAL - all observations", col = "grey", freq = FALSE, xlab = " ")

# By zone
zepas <- unique(dat$Region.Label)
par(mfrow = c(4, 2))

for (i in 1:length(zepas)){
  hist(dat$distance[which(dat$Species  == "MECAL" & dat$Region.Label == zepas[i])], breaks = c(0,25,50,99,200,500),
       main = paste("MECAL -", zepas[i]), col = "grey", freq = FALSE, xlab = " ")
}

# By year
year <- unique(dat$Year)
par(mfrow = c(3, 2))

for (i in 1:length(year)){
  hist(dat$distance[which(dat$Species  == "MECAL" & dat$Year == year[i])], breaks = c(0,25,50,99,200,500),
       main = paste("MECAL -", year[i]), col = "grey", freq = FALSE, xlab = " ")
}

# By observer
ob <- unique(dat$Observer)
par(mfrow = c(4, 2))

for (i in 1:length(ob)){
  hist(dat$distance[which(dat$Species  == "MECAL" & dat$Observer == ob[i])], breaks = c(0,25,50,99,200,500),
       main = paste("MECAL -", ob[i]), col = "grey", freq = FALSE, xlab = " ")
}


# MICAL
# All obs
par(mfrow = c(1, 1))
hist(dat$distance[which(dat$Species  == "MICAL")], breaks = c(0,25,50,99,200,500),
     main = "MICAL - all observations", col = "grey", freq = FALSE, xlab = " ")

# By zone
zepas <- unique(dat$Region.Label)
par(mfrow = c(4, 2))

for (i in 1:length(zepas)){
  hist(dat$distance[which(dat$Species  == "MICAL" & dat$Region.Label == zepas[i])], breaks = c(0,25,50,99,200,500),
       main = paste("MICAL -", zepas[i]), col = "grey", freq = FALSE, xlab = " ")
}

# By year
year <- unique(dat$Year)
par(mfrow = c(3, 2))

for (i in 1:length(year)){
  hist(dat$distance[which(dat$Species  == "MICAL" & dat$Year == year[i])], breaks = c(0,25,50,99,200,500),
       main = paste("MICAL -", year[i]), col = "grey", freq = FALSE, xlab = " ")
}

xtabs(~ Year + Banda , data = dat[which(dat$Species == "MICAL"), ])

# By observer
ob <- unique(dat$Observer)
par(mfrow = c(4, 2))

for (i in 1:length(ob)){
  hist(dat$distance[which(dat$Species  == "MICAL" & dat$Observer == ob[i])], breaks = c(0,25,50,99,200,500),
       main = paste("MICAL -", ob[i]), col = "grey", freq = FALSE, xlab = " ")
}

# LASEN: This one has good bp!!!
# All obs
par(mfrow = c(1, 1))
hist(dat$distance[which(dat$Species  == "LASEN")], breaks = c(0,25,50,99,200,500),
     main = "LASEN - all observations", col = "grey", freq = FALSE, xlab = " ")

# By zone
zepas <- unique(dat$Region.Label)
par(mfrow = c(4, 2))

for (i in 1:length(zepas)){
  hist(dat$distance[which(dat$Species  == "LASEN" & dat$Region.Label == zepas[i])], breaks = c(0,25,50,99,200,500),
       main = paste("LASEN -", zepas[i]), col = "grey", freq = FALSE, xlab = " ")
}

# By year
year <- unique(dat$Year)
par(mfrow = c(3, 2))

for (i in 1:length(year)){
  hist(dat$distance[which(dat$Species  == "LASEN" & dat$Year == year[i])], breaks = c(0,25,50,99,200,500),
       main = paste("LASEN -", year[i]), col = "grey", freq = FALSE, xlab = " ")
}

# By observer
ob <- unique(dat$Observer)
par(mfrow = c(4, 2))

for (i in 1:length(ob)){
  hist(dat$distance[which(dat$Species  == "LASEN" & dat$Observer == ob[i])], breaks = c(0,25,50,99,200,500),
       main = paste("LASEN -", ob[i]), col = "grey", freq = FALSE, xlab = " ")
}

# PYRAX: This one has good bp!!!
# All obs
par(mfrow = c(1, 1))
hist(dat$distance[which(dat$Species  == "PYRAX")], breaks = c(0,25,50,99,200,500),
     main = "PYRAX - all observations", col = "grey", freq = FALSE, xlab = " ")

# By zone
zepas <- unique(dat$Region.Label)
par(mfrow = c(4, 2))

for (i in 1:length(zepas)){
  hist(dat$distance[which(dat$Species  == "PYRAX" & dat$Region.Label == zepas[i])], breaks = c(0,25,50,99,200,500),
       main = paste("PYRAX -", zepas[i]), col = "grey", freq = FALSE, xlab = " ")
}

# By year
year <- unique(dat$Year)
par(mfrow = c(3, 2))

for (i in 1:length(year)){
  hist(dat$distance[which(dat$Species  == "PYRAX" & dat$Year == year[i])], breaks = c(0,25,50,99,200,500),
       main = paste("PYRAX -", year[i]), col = "grey", freq = FALSE, xlab = " ")
}

# By observer
ob <- unique(dat$Observer)
par(mfrow = c(4, 2))

for (i in 1:length(ob)){
  hist(dat$distance[which(dat$Species  == "PYRAX" & dat$Observer == ob[i])], breaks = c(0,25,50,99,200,500),
       main = paste("PYRAX -", ob[i]), col = "grey", freq = FALSE, xlab = " ")
}

# ---- Check if CABRA, PTALC are present outside their range ----
cabra <- dat[which(dat$Species == "CABRA"), ]
xtabs(~ Region.Label + Year, data = cabra)

ptalc <- dat[which(dat$Species == "PTALC"), ]
xtabs(~ Region.Label + Year, data = ptalc)

