
rm(list=ls())

library(dplyr)

##############################################################################################################
################################           Temperature              ##########################################
###########################################################################################################

#### Check effect of temperature for species where it is significant for m6

setwd("D:/PhD/Second chapter/Data")
s <- read.csv("sp_trend_dg.csv", sep = ";")
s_good <- as.vector(s$Species[which(s$include_samplesize == 1)])
remove_6 <- c("CACHL", "CAINA", "CIJUN", "COCOT", "COLIV", "LUARB", "LUMEG", "MIMIG", "OEHIS", "ORORI", "PIVIR", "PYRAX", "STUNI", "STVUL", "TUMER", "TUVIS")
s_good <- s_good[-which(s_good %in% remove_6)] # SPECIES THAT CONVERGE FOR MODEL 6

setwd("D:/PhD/Second chapter/Data/Results/TRIM/6temp")
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
s_temp <- s_temp[c(1,3)]


# Plot predicted values for temperature for significant species

setwd("S:/PhD/Second chapter/Data/Results/Plots/6temp/sig")
#pdf("6.pdf")

par(mfrow = c(2,1),
    mar = c(2,2,3,2),
    oma = c(2,3,1,2))

for (xxx in 1:2){
  xxx = 1
setwd("D:/PhD/Second chapter/Data/Results/TRIM/6temp")
load(paste("HDS_",s_temp[xxx],".RData", sep = ""))

# 1. Predictions of the model Sigma ~ Temperature

temp_pred <- seq(5, 25, length.out = 500)
outall <- do.call(rbind,out$samples)
pred <- list()
for(i in 1:dim(outall)[1]){ 
  pred[[i]] <- exp(outall[i,"mu.sig"] + 
                     outall[i,"bTemp.sig"]*temp_pred) } # Pred contains the list of the prediction of sigma for each iteration #(one prediction line per iteration)

predall <- do.call(rbind,pred) # All predictions/iterations together in one data frame (where columns are the prediction per each predictor (area) values)
lci <- uci <- mean.pred <- 0 

for(i in 1:length(temp_pred)){
  lci[i]  <- quantile(predall[,i],probs = 0.025) 
  uci[i]  <- quantile(predall[,i],probs = 0.975)
  mean.pred[i]  <- mean(predall[,i])
}

# 2. Plot

plot(-15, xlim=c(5,25), ylim=c(min(lci), max(uci)), main = s_temp[xxx], xlab = "Temperature", ylab = "Abundance") # area_SG_HA unscaled variable

polygon( x = c(temp_pred, rev(temp_pred)),
         y = c(lci, rev(uci)), 
         col = adjustcolor(c("grey"),alpha.f = 0.6),
         border = NA)
points(mean.pred ~ temp_pred, type="l")

}
mtext("Temperature", side = 1, line = 1, cex = 1.2, outer = TRUE)
mtext("Sigma", side = 2, line = 1, cex = 1.2, outer = TRUE)

par(mfrow = c(3,1),
    mar = c(2,2,3,2),
    oma = c(2,3,1,2))
s_temp <- temp_sig$sp
s_temp <- s_temp[c(2,6,7)]

for (xxx in 1:3){ 
#dev.off()
xxx = 1
setwd("S:/PhD/Second chapter/Data/Results/TRIM/6temp")
load(paste("HDS_",s_temp[xxx],".RData", sep = ""))

# 1. Predictions of the model Sigma ~ Temperature

temp_pred <- seq(5, 30, length.out = 500)
outall <- do.call(rbind,out$samples)
pred <- list()
for(i in 1:dim(outall)[1]){ 
  pred[[i]] <- exp(outall[i,"mu.sig"] + 
                     outall[i,"bTemp.sig"]*temp_pred) } # Pred contains the list of the prediction of sigma for each iteration #(one prediction line per iteration)

predall <- do.call(rbind,pred) # All predictions/iterations together in one data frame (where columns are the prediction per each predictor (area) values)
lci <- uci <- mean.pred <- 0 

for(i in 1:length(temp_pred)){
  lci[i]  <- quantile(predall[,i],probs = 0.025) 
  uci[i]  <- quantile(predall[,i],probs = 0.975)
  mean.pred[i]  <- mean(predall[,i])
}

# 2. Plot

plot(-15, xlim=c(5,25), ylim=c(min(lci), max(uci)), main = s_temp[xxx], xlab = "Temperature", ylab = "Abundance") # area_SG_HA unscaled variable

polygon( x = c(temp_pred, rev(temp_pred)),
         y = c(lci, rev(uci)), 
         col = adjustcolor(c("grey"),alpha.f = 0.6),
         border = NA)
points(mean.pred ~ temp_pred, type="l")
}
##?? This is wrong, ask Rahel how to plot predictions in sigma
mtext("Temperature", side = 1, line = 1, cex = 0.5, outer = TRUE)
mtext("Sigma", side = 2, line = 1, cex = 0.5, outer = TRUE)

####################################################################
#############    PRED EXP RS ####################################
#################################################################

##sigma by temp

pdf("temperature_sigma_exp.pdf")

par(mfrow = c(4,2),
    mar = c(2,2,3,2),
    oma = c(2,3,1,2))


for (xxx in 1:7){
  xxx = 1
  setwd("S:/PhD/Second chapter/Data/Results/TRIM/6temp")
  load(paste("HDS_",s_temp[xxx],".RData", sep = ""))
  
  # 1. Predictions of the model Sigma ~ Temperature
  
  temp_pred <- seq(5, 30, length.out = 500)
temp<-seq(5,25,1)
outall <- do.call(rbind,out$samples) 

# Total population (since both follow the same trend)
pred.exp <- matrix(NA, dim(outall)[1], length(temp))
for(i in 1:dim(outall)[1]){ 
  ##calculate population, year 1
  pred.exp[i,] <- exp(as.vector(outall[i,"mu.sig"])+outall[i,"bTemp.sig"]*temp)
}

predall.exp <- pred.exp
lci.exp <- uci.exp <- mean.pred.exp <- 0 

for(i in 1:length(temp)){
  lci.exp[i]  <- quantile(predall.exp[,i],probs = 0.025) 
  uci.exp[i]  <- quantile(predall.exp[,i],probs = 0.975)
  mean.pred.exp[i]  <- mean(predall.exp[,i])
}


plot(temp, mean.pred.exp, type="l", ylim=c(min(lci.exp), max(uci.exp)))
points(temp, lci.exp, type='l', col='red')
points(temp, uci.exp, type='l', col='red')

}

########################################################################
##### Plot removing the exponential predictions to compare ############
########################################################################

s_temp <- temp_sig$sp

setwd("S:/PhD/Second chapter/Data/Results/Plots/6temp/sig")

pdf("temperature_sigma.pdf")

par(mfrow = c(4,2),
    mar = c(2,2,3,2),
    oma = c(2,3,1,2))


for (xxx in 1:7){

  setwd("D:/PhD/Second chapter/Data/Results/TRIM/6temp")
  load(paste("HDS_",s_temp[xxx],".RData", sep = ""))
  
  # 1. Predictions of the model Sigma ~ Temperature
  
  temp_pred <- seq(5, 30, length.out = 500)
  outall <- do.call(rbind,out$samples)
  pred <- list()
  for(i in 1:dim(outall)[1]){ 
    pred[[i]] <- outall[i,"mu.sig"] + 
                       outall[i,"bTemp.sig"]*temp_pred } # Pred contains the list of the prediction of sigma for each iteration #(one prediction line per iteration)
  
  predall <- do.call(rbind,pred) # All predictions/iterations together in one data frame (where columns are the prediction per each predictor (area) values)
  lci <- uci <- mean.pred <- 0 
  
  for(i in 1:length(temp_pred)){
    lci[i]  <- quantile(predall[,i],probs = 0.025) 
    uci[i]  <- quantile(predall[,i],probs = 0.975)
    mean.pred[i]  <- mean(predall[,i])
  }
  
  # 2. Plot
  
  plot(-15, xlim=c(5,25), ylim=c(min(lci), max(uci)), main = s_temp[xxx], xlab = "Temperature", ylab = "Abundance") # area_SG_HA unscaled variable
  
  polygon( x = c(temp_pred, rev(temp_pred)),
           y = c(lci, rev(uci)), 
           col = adjustcolor(c("grey"),alpha.f = 0.6),
           border = NA)
  points(mean.pred ~ temp_pred, type="l")
  
}
mtext("Temperature", side = 1, line = 1, cex = 0.9, outer = TRUE)
mtext("log(Sigma)", side = 2, line = 1, cex = 0.9, outer = TRUE)

dev.off()
