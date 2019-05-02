
rm(list=ls())

library(dplyr)

##############################################################################################################
################################           Temperature              ##########################################
###########################################################################################################

#### Check effect of temperature for species where it is significant for m6

setwd("S:/PhD/Second chapter/Data")
s <- read.csv("sp_trend_dg.csv", sep = ";")
s_good <- as.vector(s$Species[which(s$include_samplesize == 1)])
remove_6 <- c("CACHL", "CAINA", "CIJUN", "COCOT", "COLIV", "LUARB", "LUMEG", "MIMIG", "OEHIS", "ORORI", "PIVIR", "PYRAX", "STUNI", "STVUL", "TUMER", "TUVIS")
s_good <- s_good[-which(s_good %in% remove_6)] # SPECIES THAT CONVERGE FOR MODEL 6

setwd("S:/PhD/Second chapter/Data/Results/TRIM/6temp")
load("spConvergence_light.RData")

temp <- data.frame(matrix(NA,ncol = 5,nrow = length(s_good)))
colnames(temp) <- c("sp", "est", "lci", "uci", "over_0")

for (i in 1:length(s_good)) {
  sum <- data.frame(species[[i]][[2]]) 
  est_temp <- sum[which(rownames(sum) %in% "bTemp.sig"), c(1,3,7,10)]
  temp[i,1] <- s_good[i]
  temp[i,c(2:5)] <- est_temp 
}

temp_sig <- temp[which(temp$over_0 == 0), ] # Species which detection is detected by temperature
s_temp <- temp_sig$sp

##############################################################################################################
######################           Bayesian p-values                ##########################################
###########################################################################################################

# Join Bayesian p-values

# Observation model
Bp6 <- data.frame(matrix(NA,ncol = 5,nrow = length(s_good)))
colnames(Bp6) <- c("sp", "Bp.Obs", "lci", "uci", "over_0")

for (i in 1:length(s_good)) {
  sum <- data.frame(species[[i]][[2]]) 
  est_Bp6 <- sum[which(rownames(sum) %in% "Bp.Obs"), c(1,3,7,10)]
  Bp6[i,1] <- s_good[i]
  Bp6[i,c(2:5)] <- est_Bp6 
}
Bp6 <- Bp6[,c(1,2)]
Bp6_bad <- Bp6[which(Bp6$Bp.Obs < 0.1 | Bp6$Bp.Obs > 0.9), ] # Bad bayesian p-values

# Join with temperature
tab1 <- left_join(temp,Bp6, by = "sp")

# Abundance model
BpN6 <- data.frame(matrix(NA,ncol = 5,nrow = length(s_good)))
colnames(BpN6) <- c("sp", "Bp.N", "lci", "uci", "over_0")

for (i in 1:length(s_good)) {
  sum <- data.frame(species[[i]][[2]]) 
  est_BpN6 <- sum[which(rownames(sum) %in% "Bp.N"), c(1,3,7,10)]
  BpN6[i,1] <- s_good[i]
  BpN6[i,c(2:5)] <- est_BpN6 
}
BpN6 <- BpN6[,c(1,2)]
BpN6_bad <- BpN6[which(BpN6$Bp.Obs < 0.1 | BpN6$Bp.Obs > 0.9), ] # Bad bayesian p-values

# Join all
tab2 <- left_join(tab1,BpN6, by = "sp")
tab2[,c(2,3,4,6,7)] <- round(tab2[,c(2,3,4,6,7)], digits = 3)
tab2$CI <- paste("[", tab2$lci, " - ", tab2$uci, "]", sep = "")
setwd("S:/PhD/Second chapter/Data/Results/Paper")
write.csv(tab2, "Table1.csv")

