
rm(list=ls())


library(rjags)
library(jagsUI)
library(dplyr)

source("D:/PhD/MyScripts/Ch. 2-3/Ch. 3/Results/Functions/ProcessCodaOutput.R")
source("D:/PhD/MyScripts/Ch. 2-3/Ch. 3/Results/Functions/plot.violins2.r")
source("D:/PhD/MyScripts/Ch. 2-3/Ch. 3/Results/Functions/DoScale.r")


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
out <- ProcessCodaOutput(out.list)

# Create data frame with species - coefficients together
sp.df <- data.frame(sp = sp, b.a1 = out$colnames.sims[grep("b.a1", out$colnames.sims)],
                    b.a2 = out$colnames.sims[grep("b.a2", out$colnames.sims)],
                    b.a3 = out$colnames.sims[grep("b.a3", out$colnames.sims)])


coeff <- c("b.a1", "b.a2", "b.a3")
names <- c("b.SG", "b.AES", "b.GREEN")


# ---- Wide ----
  
  # Order it by effect size of SG
  
  v1 <- data.frame(out$mean[names(out$mean) %in% coeff[1]])
  v2 <- data.frame(sp.df[ ,colnames(sp.df) %in% coeff[1]])
  values <- cbind(v1,v2)
  colnames(values)[1] <- "mean"
  colnames(values)[2] <- coeff[1]
  values$index <- rownames(values)
  values2 <- left_join(sp.df,values)
  values_sorted <- arrange(values2, mean)
  sp_sorted <-  values_sorted$sp
  coef_sorted <- values_sorted[,-c(1,5)]
  
  # Plot
  setwd("D:/PhD/Third chapter/Data/Results_species/15.1/15.1.1_DATA_GOODsp_resiN")
  pdf(paste("15.1.1.ViolinWide.pdf"))
  
  par(mfrow = c(1,3),
      mar = c(1,1.5,1.5,0),
      oma = c(3,4,3,1)) 
  
# SG
  plot(10, ylim = c(1, nSpecies+1), 
       xlim = c(-1,1.7), 
       type ="n", yaxt="n", xlab = "Beta", ylab = "", main = "SG")
  
  mtext("Species", line = 3, side = 2, cex = 0.8, outer = TRUE) 
  axis(2, c(1:nSpecies), labels = sp_sorted, las = 2, cex.axis = 0.9)
  
  for(i in 1:nSpecies){
    plot.violins3(list(out$sims.list$b.a1[ ,as.numeric(coef_sorted$index[i])]),
                  x = i,
                  at = i,
                  violin.width = 0.3,
                  col = "darkorange",
                  add = T,
                  alpha = 0.3,
                  scale.width = FALSE,
                  border.col = "darkorange",
                  horizontal = TRUE) }
  abline(v=0)
  
  # AES
  plot(10, ylim = c(1, nSpecies+1), 
       xlim = c(-1,1.7), 
       type ="n", yaxt="n", xlab = "Beta", ylab = "", main = "AES")
  
  for(i in 1:nSpecies){
    plot.violins3(list(out$sims.list$b.a2[ ,as.numeric(coef_sorted$index[i])]),
                  x = i,
                  at = i,
                  violin.width = 0.3,
                  col = "darkorange",
                  add = T,
                  alpha = 0.3,
                  scale.width = FALSE,
                  border.col = "darkorange",
                  horizontal = TRUE)}
  abline(v=0)
  
  #GREEN
  plot(10, ylim = c(1, nSpecies+1), 
       xlim = c(-1,1.7), 
       type ="n", yaxt="n", xlab = "Beta", ylab = "", main = "Greening")
  
  for(i in 1:nSpecies){
    plot.violins3(list(out$sims.list$b.a3[ ,as.numeric(coef_sorted$index[i])]),
                  x = i,
                  at = i,
                  violin.width = 0.3,
                  col = "darkorange",
                  add = T,
                  alpha = 0.3,
                  scale.width = FALSE,
                  border.col = "darkorange",
                  horizontal = TRUE)}
  abline(v=0)
  
  dev.off()
  
  # ---- Long ----
  
  #Colours: "darkmagenta", "darkorange", "darkolivegreen4"