
rm(list=ls())

##############################################################################################################
######################           Bayesian p-values OBS. MODEL      ##########################################
###########################################################################################################

# Model 3.2

setwd("S:/PhD/Second chapter/Data")
s <- read.csv("sp_trend_dg.csv", sep = ";")
s_good <- as.vector(s$Species[which(s$include_samplesize == 1)])
remove_3.2 <- c("CACHL", "CAINA", "CIJUN", "COCOT", "COLIV", "LUARB","LUMEG","MIMIG", "OEHIS","ORORI","PIVIR", "PYRAX","SESER","STUNI", "STVUL","TUMER", "TUVIS")
s_good <- s_good[-which(s_good %in% remove_3.2)] # SPECIES THAT CONVERGE FOR MODEL 3.2

# Load data

setwd("S:/PhD/Second chapter/Data/Results/TRIM/3.2autoreg_simple")
load("spConvergence_light.RData")

# Make s_good with the same order than species[[]]
# 1. Remove from species the ones that dont converge to make it 26
species <- species[-c(4, 5, 19, 24, 25)]

Bp3 <- data.frame(matrix(NA,ncol = 5,nrow = length(s_good)))
colnames(Bp3) <- c("sp", "Bp.Obs", "lci", "uci", "over_0")

for (i in 1:length(s_good)) {
  sum <- data.frame(species[[i]][[2]]) 
  est_Bp3 <- sum[which(rownames(sum) %in% "Bp.Obs"), c(1,3,7,10)]
  Bp3[i,1] <- s_good[i]
  Bp3[i,c(2:5)] <- est_Bp3 
}

Bp3_bad <- Bp3[which(Bp3$Bp.Obs < 0.1 | Bp3$Bp.Obs > 0.9), ]

#### Check effect of temperature for species where it is significant for m6

setwd("S:/PhD/Second chapter/Data")
s <- read.csv("sp_trend_dg.csv", sep = ";")
s_good <- as.vector(s$Species[which(s$include_samplesize == 1)])
remove_6 <- c("CACHL", "CAINA", "CIJUN", "COCOT", "COLIV", "LUARB", "LUMEG", "MIMIG", "OEHIS", "ORORI", "PIVIR", "PYRAX", "STUNI", "STVUL", "TUMER", "TUVIS")
s_good <- s_good[-which(s_good %in% remove_6)] # SPECIES THAT CONVERGE FOR MODEL 6

setwd("S:/PhD/Second chapter/Data/Results/TRIM/6temp")
load("spConvergence_light.RData")

Bp6 <- data.frame(matrix(NA,ncol = 5,nrow = length(s_good)))
colnames(Bp6) <- c("sp", "Bp.Obs", "lci", "uci", "over_0")

for (i in 1:length(s_good)) {
  sum <- data.frame(species[[i]][[2]]) 
  est_Bp6 <- sum[which(rownames(sum) %in% "Bp.Obs"), c(1,3,7,10)]
  Bp6[i,1] <- s_good[i]
  Bp6[i,c(2:5)] <- est_Bp6 
}

Bp6_bad <- Bp6[which(Bp6$Bp.Obs < 0.1 | Bp6$Bp.Obs > 0.9), ]

# For the bad Bayesian Bp. I have runned the 6 model with another detection function (6.1)
# Check if the Bayesian p-values improved

setwd("S:/PhD/Second chapter/Data/Results/TRIM/6temp/HR_df")

load("spConvergence_light.RData")

Bp6 <- data.frame(matrix(NA,ncol = 5,nrow = length(s_good)))
colnames(Bp6) <- c("sp", "Bp.Obs", "lci", "uci", "over_0")

for (i in 1:length(s_good)) {
  sum <- data.frame(species[[i]][[2]]) 
  est_Bp6 <- sum[which(rownames(sum) %in% "Bp.Obs"), c(1,3,7,10)]
  Bp6[i,1] <- s_good[i]
  Bp6[i,c(2:5)] <- est_Bp6 
}

Bp6_bad <- Bp6[which(Bp6$Bp.Obs < 0.1 | Bp6$Bp.Obs > 0.9), ]
