rm(list=ls())

library(dplyr)

##############################################################################################################
################################           Temperature              ##########################################
###########################################################################################################

#### Check effect of temperature for species where it is significant for m6

s_good <- c("SYCAN", "CACHL", "LASEN", "ALRUF", "PAMAJ", "ALARV", "CABRA", "CACAR", "COPAL", "PIPIC",
            "COOEN", "HIRUS", "PYRAX", "CAINA", "COMON", "PADOM", "PAMON", "FATIN", "SESER", "TUMER", "GATHE",
            "BUOED", "SYMEL", "UPEPO", "GACRI", "STSSP", "MICAL", "MEAPI", "TERAX", "MECAL") # Species in order of appearance in the list

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Resubmission/Results/Compiled_FINAL")
load("spConvergence_light_resub.RData")



##### Value for CI #####


temp <- data.frame(matrix(NA,ncol = 5,nrow = length(s_good)))
colnames(temp) <- c("sp", "est", "lci", "uci", "over_0")

for (i in 1:length(s_good)) {
  sum <- data.frame(species[[i]][[2]]) 
  est_temp <- sum[which(rownames(sum) %in% "bTemp.sig"), c(1,3,7,10)]
  temp[i,1] <- s_good[i]
  if(nrow(est_temp) == 0)
    next
  temp[i,c(2:5)] <- est_temp 
}

temp_sig <- temp[which(temp$over_0 == 0), ] # Species which detection is detected by temperature
s_temp <- temp_sig$sp
temp <- cbind(temp[ ,1], round(temp[ ,c(2:5)],2))
colnames(temp)[1] <- "sp"
#setwd("S:/Results/chapter2/Plots")
#write.csv(temp, "Table1.csv")

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
tab2 <- tab2[ ,c(1,2,8,6,7)]
tab3 <- arrange(tab2, sp)

library(dplyr)
setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data")
leg <- read.csv("leg_species.csv", sep = ";")
leg <- arrange(leg,codiEspecie)
colnames(leg)[1] <- "sp"

tab4 <- left_join(tab3,leg)
tab4 <- tab4[ ,c(1,7,2:5)]

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Resubmission/Results/Final")
write.csv(tab4, "TableS4_temp_BP.csv")


