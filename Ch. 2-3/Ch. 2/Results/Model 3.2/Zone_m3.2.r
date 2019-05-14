
rm(list=ls())

##############################################################################################################
################################           Zone                   ##########################################
###########################################################################################################

#### Check effect of temperature for species where it is significant for m6

# For legend:

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

zone <- data.frame(matrix(NA,ncol = 5,nrow = length(s_good)))
colnames(zone) <- c("sp", "est", "lci", "uci", "over_0")

for (i in 1:length(s_good)) {
  sum <- data.frame(species[[i]][[2]]) 
  est_zone <- sum[which(rownames(sum) %in% "bzB.sig"), c(1,3,7,10)]
  zone[i,1] <- s_good[i]
  zone[i,c(2:5)] <- est_zone 
}

zone_sig <- zone[which(zone$over_0 == 0), ] # In general species have a higher sigma in zone 1 (oriental)
s_zone <- zone_sig$sp
