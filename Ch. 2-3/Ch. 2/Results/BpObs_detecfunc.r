

##############################################################################################################
###########          Check detection function of species with bad bayesian p-values      ######################
###########################################################################################################


# 1. Species with bad bayesian p-values

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

sbp <- Bp6_bad$sp

# Load data

setwd("S:/PhD/Second chapter/Data")
d <- read.csv("DataDS_ready_ALL.csv")
colnames(d)[which(colnames(d) == "Count")] <- "Cluster" 
par(mfrow = c(3,2))
for (i in 1:6){
  d1 <- d[which(d$Species == sbp[i]), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "distance"))]
  col <- c("blue", "orange", "green", "grey")
  hist(d1$distance, main = sbp[i], breaks = c(0,25,50,99,200), col = col)

}

par(mfrow = c(2,2))
for (i in 7:10){
  d1 <- d[which(d$Species == sbp[i]), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "distance"))]
  col <- c("blue", "orange", "green", "grey")
  hist(d1$distance, main = sbp[i], breaks = c(0,25,50,99,200), col = col)
  
}

par(mfrow = c(5,2))
for (i in 1:10){
  d1 <- d[which(d$Species == sbp[i]), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "distance"))]
  col <- c("blue", "orange", "green", "grey")
  hist(d1$distance, main = sbp[i], breaks = c(0,25,50,99,200), col = col)
  
}