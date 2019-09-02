
rm(list=ls())

library(dplyr)

##############################################################################################################
################################           Temperature              ##########################################
###########################################################################################################

#### Check effect of temperature for species where it is significant for m6

s_good <- c("ALRUF","BUOED","CACAR","COOEN","COPAL","GACRI","GATHE","MEAPI","MECAL","PAMAJ","SESER","STSSP","SYCAN","SYMEL","TERAX","UPEPO",
            "MICAL","HIRUS","PADOM","PIPIC","PAMON", "COMON") # It has to be in this order because the list is in this order

##### Value for CI #####

# Load data

setwd("S:/PhD/Second chapter/Data/Results/TRIM/6temp/Final")
setwd("C:/Users/Ana/Documents/PhD/PhD_21_Junio_2019/Second chapter/Data/Results/TRIM/6temp/Final")

load("spConvergence_light_FINAL.RData")

temp <- data.frame(matrix(NA,ncol = 5,nrow = length(s_good)))
colnames(temp) <- c("sp", "est", "lci", "uci", "over_0")

for (i in 1:length(s_good)) {
  sum <- data.frame(species[[i]][[2]]) 
  est_temp <- sum[which(rownames(sum) %in% "bTemp.sig"), c(1,3,7,10)]
  temp[i,1] <- s_good[i]
  temp[i,c(2:5)] <- est_temp 
}

temp_sig <- temp[which(temp$over_0 == 0), ] # Species which detection is detected by temperature
s_temp <- temp_sig$sp[-6] # Remove COMON (Bad p-values anyway and graph is not nice)
s_temp_names <- c("Common wood pigeon", "Crested lark", "Thekla lark", "European bee-eater", "Corn bunting")


# Plot predicted values for temperature for significant species

setwd("S:/PhD/Second chapter/Data/Results/Final")
pdf("FigS3.pdf")

par(mfrow = c(3,2),
    mar = c(2,2,3,2),
    oma = c(2,3,1,2))

for (xxx in 1:length(s_temp)){
  setwd("S:/PhD/Second chapter/Data/Results/TRIM/compiled_final")
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
  
  plot(-15, xlim=c(5,25), ylim=c(min(lci), max(uci)), main = s_temp_names[xxx], xlab = "Temperature", ylab = "Abundance") # area_SG_HA unscaled variable
  
  polygon( x = c(temp_pred, rev(temp_pred)),
           y = c(lci, rev(uci)), 
           col = adjustcolor(c("grey"),alpha.f = 0.6),
           border = NA)
  points(mean.pred ~ temp_pred, type="l")
  
}
mtext("Temperature", side = 1, line = 1, cex = 1, outer = TRUE)
mtext("Sigma", side = 2, line = 1, cex = 1, outer = TRUE)


dev.off()
