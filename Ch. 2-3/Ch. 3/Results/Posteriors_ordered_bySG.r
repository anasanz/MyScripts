library(rjags)
library(jagsUI)
library(dplyr)

# Load species analyzed in the model

setwd("D:/PhD/Third chapter/Data")
d <- read.csv("DataDS_ch3_15_19_READY_FIXED.csv")
bad_bp <- c("GACRI", "GATHE", "PADOM", "STSSP") # to remove all species with bad bp except MICAL and MECAL
d <- d[-which(d$Species %in% bad_bp), ]

sp <- as.character(unique(d$Species))
sp <- sort(sp)
nSpecies <- length(sp)

# Load the three chains
load("D:/PhD/Third chapter/Data/model/15.1.1/JagsOutFOR15.1.1a.RData")
outa <- out
load("D:/PhD/Third chapter/Data/model/15.1.1/JagsOutFOR15.1.1b.RData")
outb <- out
load("D:/PhD/Third chapter/Data/model/15.1.1/JagsOutFOR15.1.1c.RData")
outc <- out
class(outc)


out.list<- list()
out.list[[1]] <- as.mcmc(outa$samples[[1]])
out.list[[2]] <- as.mcmc(outb$samples[[1]])
out.list[[3]] <- as.mcmc(outc$samples[[1]])

out.list <- as.mcmc.list(out.list)

source("D:/PhD/MyScripts/Ch. 2-3/Ch. 3/Data/ProcessCodaOutput.R")

out <- ProcessCodaOutput(out.list)
outall <- out$sims.list # 3 chains together 
df.outall <- as.data.frame(outall)

# Create data frame with species - coefficients together
sp.df <- data.frame(sp = sp, b.a1 = out$colnames.sims[grep("b.a1", out$colnames.sims)],
                    b.a2 = out$colnames.sims[grep("b.a2", out$colnames.sims)],
                    b.a3 = out$colnames.sims[grep("b.a3", out$colnames.sims)])

# Process samples

coeff <- c("b.a1", "b.a2", "b.a3")
names <- c("b.SG", "b.AES", "b.GREEN")

# 1. Order it by effect size sg

v1 <- data.frame(out$mean[names(out$mean) %in% coeff[1]])
v2 <- data.frame(sp.df[ ,colnames(sp.df) %in% coeff[1]])
values <- cbind(v1,v2)
colnames(values)[1] <- "mean"
colnames(values)[2] <- coeff[1]
values2 <- left_join(sp.df,values)
values_sorted <- arrange(values2, mean)
sp_sorted <-  values_sorted$sp
coef_sorted <- values_sorted[,-c(1,5)]
#coef_sorted <- values_sorted[,which(colnames(values_sorted) %in% coeff[1])]

setwd("D:/PhD/Third chapter/Data/Results_species/15.1/15.1.1_DATA_GOODsp_resiN")
pdf(paste("15.1.1_bySG3.pdf"),5,9)

par(mfrow = c(10,3),
    mar = c(2,1,2,0.5),
    oma = c(3,4,3,2)) 

for (i in 1:nSpecies){
  
  # Plot sg to have the name on the side
  sims_coef <- data.frame(outall[names(outall) %in% coeff[1]])
  colnames(sims_coef) <- values2[,which(colnames(values2) %in% coeff[1])]
  sims_coef <- as.matrix(sims_coef)
  dens_obs <- density(sims_coef[,which(colnames(sims_coef) %in% coef_sorted[i,1])] )# Density of iterations for coefficient
  mean_obs <- mean(sims_coef[,which(colnames(sims_coef) %in% coef_sorted[i,1])] )
  lci_obs  <- quantile(sims_coef[,which(colnames(sims_coef) %in% coef_sorted[i,1])], probs = 0.025) 
  uci_obs  <- quantile(sims_coef[,which(colnames(sims_coef) %in% coef_sorted[i,1])], probs = 0.975)
  
  
  # Plot
  
  plot(dens_obs, xlab = " ", ylab = " ", main = " " , axes = FALSE) 
  
  axis(1, pos = 0, tck = -0.02, cex.axis = 0.9, mgp = c(3, 0.2, 0))
  
  x1 <- min(which(dens_obs$x  >= lci_obs))  
  x2 <- max(which(dens_obs$x  <  uci_obs))
  polygon(x = c(dens_obs$x[c(x1,x1:x2, x2)]), y= c(0, dens_obs$y[x1:x2], 0), col="gray")
  
  segments(x0 = mean_obs, y0 = 0, x1 = , mean_obs, y1 = max(dens_obs$y)+2, col = "black", lwd = 1.2) #abline( a = 0,  v = mean_obs, col = "red", lwd = 1.5)
  segments(x0 = 0, y0 = 0, x1 = 0, y1 = max(dens_obs$y)+2, col = "red", lwd = 1.2, lty = 5)
  
  mtext(sp_sorted[i], line = 1, side = 2, cex = 0.8) 
  
  

for (c in 2:3){
  
    sims_coef <- data.frame(outall[names(outall) %in% coeff[c]])
    colnames(sims_coef) <- values2[,which(colnames(values2) %in% coeff[c])]
    sims_coef <- as.matrix(sims_coef)
    dens_obs <- density(sims_coef[,which(colnames(sims_coef) %in% coef_sorted[i,c])] )# Density of iterations for coefficient
    mean_obs <- mean(sims_coef[,which(colnames(sims_coef) %in% coef_sorted[i,c])] )
    lci_obs  <- quantile(sims_coef[,which(colnames(sims_coef) %in% coef_sorted[i,c])], probs = 0.025) 
    uci_obs  <- quantile(sims_coef[,which(colnames(sims_coef) %in% coef_sorted[i,c])], probs = 0.975)
    

    # Plot
    
    plot(dens_obs, xlab = " ", ylab = " ", main = " " , axes = FALSE) 
    
    axis(1, pos = 0, tck = -0.02, cex.axis = 0.9, mgp = c(3, 0.2, 0))
    
    x1 <- min(which(dens_obs$x  >= lci_obs))  
    x2 <- max(which(dens_obs$x  <  uci_obs))
    polygon(x = c(dens_obs$x[c(x1,x1:x2, x2)]), y= c(0, dens_obs$y[x1:x2], 0), col="gray")
    
    segments(x0 = mean_obs, y0 = 0, x1 = , mean_obs, y1 = max(dens_obs$y)+2, col = "black", lwd = 1.2) #abline( a = 0,  v = mean_obs, col = "red", lwd = 1.5)
    segments(x0 = 0, y0 = 0, x1 = 0, y1 = max(dens_obs$y)+2, col = "red", lwd = 1.2, lty = 5)
}
}
dev.off()

