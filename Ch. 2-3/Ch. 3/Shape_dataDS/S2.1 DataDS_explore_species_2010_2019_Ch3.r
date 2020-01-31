

# Check and select species that will be used for the community model (CH 3)

rm(list=ls())

library(dplyr)
library(tidyr)

setwd("D://PhD/Third chapter/Data")

dat <- read.csv("DataDS_ch3_allsp_1019.csv", sep = ",")

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

freq$total_detections <- rowSums(freq[ ,c(2:11)])

freq <- freq[-which(freq$total_detections == 0), ]

setwd("C:/Users/Ana/Documents/PhD/Third chapter/Data")
# write.csv(freq, "NumberDetect_sp_year_ch3_1019.csv")
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

year <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)
sp <- unique(dat$Species)

prop_sp <- as.data.frame(matrix(ncol = length(year), nrow = length(sp)))
rownames(prop_sp) <- sp
colnames(prop_sp) <- year

for (i in 1:length(sp)){
  d_sp <- dat[which(dat$Species == sp[i]), ]
  for (t in 1:10){
    d_t <- d_sp[which(d_sp$Year == year[t]), ]
    d_prop <- (length(unique(d_t$T_Y))/n_transects[t])*100
    prop_sp[i,t] <- d_prop
  }
}

d_prop <- (length(unique(d_sp$T_Y))/1083)*100

prop_sp <- round(prop_sp,2)

prop_sp$Mean_proportion <- round(apply(prop_sp,1,mean), 2) # average of species occupancy in transects 

setwd("C:/Users/Ana/Documents/PhD/Third chapter/Data")
#write.csv(prop_sp, "Proptransects_sp_year_ch3_1019.csv")

# Join both tables together
prop_sp$Species <- rownames(prop_sp)
tab <- left_join(freq, prop_sp, by = "Species")

setwd("C:/Users/Ana/Documents/PhD/Third chapter/Data")
#write.csv(tab, "infospecies_ch3_1019.csv")


# AQUI
#####
#### Remove very scarce (less than 30 detections in total, which is the lower threshold to include PTORI) ####

setwd("D:/PhD/Third chapter/Data")
dat_info <- read.csv("infospecies_ch3_FINAL_1019.csv", sep = ";")
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

#pdf("df_all_observations_allsp_1019.pdf")

par(mfrow = c(4,3))
for (i in 1:length(spec)){
  hist(dat$distance[which(dat$Species %in% spec[i])], breaks = c(0,25,50,99,200,500),
       main = paste(spec[i], "- Distances"), col = "grey", freq = FALSE) 
}
#dev.off()


#### Remove species with a bad detection curve ####

dc <- dat_info[which(dat_info$remove_detectioncurve == 1), ]
sp_dc <- as.character(unique(dc$Species)) #Vector with species to delete
dat <- dat[-which(dat$Species %in% sp_dc), ]



# Plot all observations
setwd("D:/PhD/Third chapter/Explore")

pdf("df_all_observations_goodsp_1019.pdf")
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
pdf("df_all_observations_goodsp_1019_seen.pdf")

par(mfrow = c(4,3))
for (i in 1:length(spec)){
  hist(dat$distance[which(dat$Species %in% spec[i] & dat$Obs_type == "V")], breaks = c(0,25,50,99,200,500),
       main = paste(spec[i], "- Distances"), col = "grey", freq = FALSE, xlab = " ") 
}
dev.off()

# Plot only heard

setwd("D:/PhD/Third chapter/Explore")
pdf("df_all_observations_goodsp_1019_heard.pdf")

par(mfrow = c(4,3))
for (i in 1:length(spec)){
  if (sum(dat$distance[which(dat$Species %in% spec[i] & dat$Obs_type == "S")]) == 0)
    plot(1,xlab = " ", ylab = " ", main = spec[i]) else hist(dat$distance[which(dat$Species %in% spec[i] & dat$Obs_type == "S")], breaks = c(0,25,50,99,200,500),
                                                             main = paste(spec[i], "- Distances"), col = "grey", freq = FALSE, xlab = " ") 
}
dev.off()

# Finally because the detection curves of the heard observations are only increasing, we can not include Heard vS Seen as a co-variate 
# in sigma. Therefore, we will pool all the observations together and analyze it as a whole with the hazard rate detection function.

######  TERAX AND BUOED PROBLEM #####

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

# The detection curves are actually better!! Check how many detections from each

# ----1. Number of detections ----

freq <- as.data.frame(xtabs(~Species2 + Year, dat))
freq <- spread(freq,Year, Freq) # Number of detections (ds observations) of each species per year in all transects (nrows per species and year)
freq$remove <- 0 # To export data set and select the ones that are present in enough transects

freq$total_detections <- rowSums(freq[ ,c(2:10)])

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
sp <- unique(dat$Species2)

prop_sp <- as.data.frame(matrix(ncol = length(year), nrow = length(sp)))
rownames(prop_sp) <- sp
colnames(prop_sp) <- year

for (i in 1:length(sp)){
  d_sp <- dat[which(dat$Species2 == sp[i]), ]
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

# Join both tables together
prop_sp$Species2 <- rownames(prop_sp)
tab <- left_join(freq, prop_sp, by = "Species2")

#### Esto es muy interesante, porque en verdad no hay muchas observaciones indeterminadas, por lo que podrÃ?a 
# Analizar por separado TERAX_M y TERAX_F

# ---- Grouping bins 1 & 2 ----

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

# ---- Check by zones to see if one zone is more problematic ----

##### TERAX
zepas <- unique(dat$Region.Label)
par(mfrow = c(4, 2))

for (i in 1:length(zepas)){
  hist(dat$distance[which(dat$Species  == "TERAX" & dat$Region.Label == zepas[i])], breaks = c(0,25,50,99,200,500),
       main = paste("TERAX -", zepas[i]), col = "grey", freq = FALSE, xlab = " ")
}


datprueba <- dat
datprueba <- datprueba[which(datprueba$Region.Label %in% c("AF", "SI")), ]

par(mfrow = c(1, 2))
hist(datprueba$distance[which(datprueba$Species  == "TERAX")], breaks = c(0,25,50,99,200,500),
     main = "TERAX -", col = "grey", freq = FALSE, xlab = " ")
hist(dat$distance[which(dat$Species  == "TERAX")], breaks = c(0,25,50,99,200,500),
     main = "TERAX -", col = "grey", freq = FALSE, xlab = " ")

# Machos
par(mfrow = c(1, 2))
hist(datprueba$distance[which(datprueba$Species2  == "TERAX_M")], breaks = c(0,25,50,99,200,500),
     main = "TERAX_M", col = "grey", freq = FALSE, xlab = " ")
hist(dat$distance[which(dat$Species2  == "TERAX_M")], breaks = c(0,25,50,99,200,500),
     main = "TERAX_M", col = "grey", freq = FALSE, xlab = " ")

# Hembras
par(mfrow = c(1, 2))
hist(datprueba$distance[which(datprueba$Species2  == "TERAX_F")], breaks = c(0,25,50,99,200,500),
     main = "TERAX_F", col = "grey", freq = FALSE, xlab = " ")
hist(dat$distance[which(dat$Species2  == "TERAX_M")], breaks = c(0,25,50,99,200,500),
     main = "TERAX_F", col = "grey", freq = FALSE, xlab = " ")

## So, only using AF and SI, looks reasonable for males...could we set the rest of the areas to 0 
# as we would do with the other species as it if would be out of the distribution range?

##### BUOED
zepas <- unique(dat$Region.Label)
par(mfrow = c(4, 2))

for (i in 1:length(zepas)){
  hist(dat$distance[which(dat$Species  == "BUOED" & dat$Region.Label == zepas[i])], breaks = c(0,25,50,99,200,500),
       main = paste("BUOED -", zepas[i]), col = "grey", freq = FALSE, xlab = " ")
}

datprueba <- dat
datprueba <- datprueba[which(datprueba$Region.Label %in% c("AF", "SI", "GR", "AL")), ] # BM se puede añadir pero empeora

par(mfrow = c(1, 2))
hist(datprueba$distance[which(datprueba$Species  == "BUOED")], breaks = c(0,25,50,99,200,500),
     main = "BUOED -", col = "grey", freq = FALSE, xlab = " ")
hist(dat$distance[which(dat$Species  == "BUOED")], breaks = c(0,25,50,99,200,500),
     main = "BUOED -", col = "grey", freq = FALSE, xlab = " ")
