rm(list=ls())

########################################################
####          Check results OBSERVER 6           #####
######################################################## 

# Load data

setwd("S:/PhD/Second chapter/Data")
s <- read.csv("sp_trend_dg.csv", sep = ";")
s_good <- as.vector(s$Species[which(s$include_samplesize == 1)])
remove_6 <- c("CACHL", "CAINA", "CIJUN", "COCOT", "COLIV", "LUARB", "LUMEG", "MIMIG", "OEHIS", "ORORI", "PIVIR", "PYRAX", "STUNI", "STVUL", "TUMER", "TUVIS")
s_good <- s_good[-which(s_good %in% remove_6)] # SPECIES THAT CONVERGE FOR MODEL 6


setwd("S:/PhD/Second chapter/Data/Results/TRIM/6temp")
load("spConvergence_light.RData")


#### OBSERVER ####

setwd("S:/PhD/Second chapter/Data/Results/Plots/6temp/sig")
pdf("Obs_1.pdf")

par(mfrow = c(3,2),
    mar = c(2,2,3,2),
    oma = c(2,3,1,2))

for (i in 1:6){
  
  summary <- data.frame(species[[i]][[2]])
  
  x <- c(1:15)
  lci <- summary[grep("sig.obs", rownames(summary)), 3]
  uci <- summary[grep("sig.obs", rownames(summary)), 7]
  
  plot(-21, xlim = c(1,15), ylim = c(min(lci),max(uci)), ylab = "sigma", xlab = "Observer")
  points(summary[grep("sig.obs", rownames(summary)), 1], pch = 16)
  arrows(x,lci,x,uci, code=3, angle=90, length=0.04)
  mtext(s_good[i], side = 3, line = 1, cex = 0.8)
}

mtext("Coefficient", side = 2, line = 1, cex = 0.8, outer = TRUE)
mtext("Observer ID", side = 1, line = 1, cex = 0.8, outer = TRUE)

dev.off()

######
setwd("S:/PhD/Second chapter/Data/Results/Plots/6temp/sig")
pdf("Obs_2.pdf")

par(mfrow = c(3,2),
    mar = c(2,2,3,2),
    oma = c(2,3,1,2))

for (i in 7:12){
  summary <- data.frame(species[[i]][[2]])
  
  x <- c(1:15)
  lci <- summary[grep("sig.obs", rownames(summary)), 3]
  uci <- summary[grep("sig.obs", rownames(summary)), 7]
  
  plot(-21, xlim = c(1,15), ylim = c(min(lci),max(uci)), ylab = "sigma", xlab = "Observer")
  points(summary[grep("sig.obs", rownames(summary)), 1], pch = 16)
  arrows(x,lci,x,uci, code=3, angle=90, length=0.04)
  mtext(s_good[i], side = 3, line = 1, cex = 0.8)
}

mtext("Coefficient", side = 2, line = 1, cex = 0.8, outer = TRUE)
mtext("Observer ID", side = 1, line = 1, cex = 0.8, outer = TRUE)

dev.off()

#####
setwd("S:/PhD/Second chapter/Data/Results/Plots/6temp/sig")
pdf("Obs_3.pdf")

par(mfrow = c(3,2),
    mar = c(2,2,3,2),
    oma = c(2,3,1,2))

for (i in 13:18){
  summary <- data.frame(species[[i]][[2]])
  
  x <- c(1:15)
  lci <- summary[grep("sig.obs", rownames(summary)), 3]
  uci <- summary[grep("sig.obs", rownames(summary)), 7]
  
  plot(-21, xlim = c(1,15), ylim = c(min(lci),max(uci)), ylab = "sigma", xlab = "Observer")
  points(summary[grep("sig.obs", rownames(summary)), 1], pch = 16)
  arrows(x,lci,x,uci, code=3, angle=90, length=0.04)
  mtext(s_good[i], side = 3, line = 1, cex = 0.8)
}

mtext("Coefficient", side = 2, line = 1, cex = 0.8, outer = TRUE)
mtext("Observer ID", side = 1, line = 1, cex = 0.8, outer = TRUE)

dev.off()

#####
setwd("S:/PhD/Second chapter/Data/Results/Plots/6temp/sig")
pdf("Obs_4.pdf")

par(mfrow = c(4,2),
    mar = c(2,2,3,2),
    oma = c(2,3,1,2))

for (i in 19:26){
  summary <- data.frame(species[[i]][[2]])
  
  x <- c(1:15)
  lci <- summary[grep("sig.obs", rownames(summary)), 3]
  uci <- summary[grep("sig.obs", rownames(summary)), 7]
  
  plot(-21, xlim = c(1,15), ylim = c(min(lci),max(uci)), ylab = "sigma", xlab = "Observer")
  points(summary[grep("sig.obs", rownames(summary)), 1], pch = 16)
  arrows(x,lci,x,uci, code=3, angle=90, length=0.04)
  mtext(s_good[i], side = 3, line = 1, cex = 0.8)
}

mtext("Coefficient", side = 2, line = 1, cex = 0.8, outer = TRUE)
mtext("Observer ID", side = 1, line = 1, cex = 0.8, outer = TRUE)

dev.off()

#########################################################################
#######################     YEAR     ####################################
#######################################################################

setwd("S:/PhD/Second chapter/Data/Results/Plots/6temp/sig")
pdf("Yearsig_1.pdf")

par(mfrow = c(3,2),
    mar = c(3,2,3,2),
    oma = c(2,3,1,2))

for (i in 1:6){
  
  summary <- data.frame(species[[i]][[2]])
  
  x <- c(1:9)
  lci <- summary[grep("log.sigma.year", rownames(summary)), 3]
  uci <- summary[grep("log.sigma.year", rownames(summary)), 7]
  
  plot(-21, xlim = c(1,9), ylim = c(min(lci),max(uci)), ylab = "sigma", xlab = "Year", axes = FALSE)
  axis(1, at = c(1, 3, 5, 7, 9), labels = c("2010", "2012", "2014", "2016", "2018"))
  axis(2)
  
  points(summary[grep("log.sigma.year", rownames(summary)), 1], pch = 16) 
  arrows(x,lci,x,uci, code=3, angle=90, length=0.04) 
  mtext(s_good[i], side = 3, line = 1, cex = 0.8)
}

mtext("Coefficient", side = 2, line = 1, cex = 0.8, outer = TRUE) 
mtext("Year", side = 1, line = 1, cex = 0.8, outer = TRUE) 

dev.off()

######
setwd("S:/PhD/Second chapter/Data/Results/Plots/6temp/sig")
pdf("Yearsig_2.pdf")

par(mfrow = c(3,2),
    mar = c(3,2,3,2),
    oma = c(2,3,1,2))

for (i in 7:12){
  summary <- data.frame(species[[i]][[2]])
  
  x <- c(1:9)
  lci <- summary[grep("log.sigma.year", rownames(summary)), 3]
  uci <- summary[grep("log.sigma.year", rownames(summary)), 7]
  
  plot(-21, xlim = c(1,9), ylim = c(min(lci),max(uci)), ylab = "sigma", xlab = "Year", axes = FALSE)
  
  axis(1, at = c(1, 3, 5, 7, 9), labels = c("2010", "2012", "2014", "2016", "2018"))
  axis(2)
  
  points(summary[grep("log.sigma.year", rownames(summary)), 1], pch = 16)
  arrows(x,lci,x,uci, code=3, angle=90, length=0.04)
  mtext(s_good[i], side = 3, line = 1, cex = 0.8)
}

mtext("Coefficient", side = 2, line = 1, cex = 0.8, outer = TRUE)
mtext("Year", side = 1, line = 1, cex = 0.8, outer = TRUE)

dev.off()

#####
setwd("S:/PhD/Second chapter/Data/Results/Plots/6temp/sig")
pdf("Yearsig_3.pdf")

par(mfrow = c(3,2),
    mar = c(3,2,3,2),
    oma = c(2,3,1,2))

for (i in 13:18){
  summary <- data.frame(species[[i]][[2]])
  
  x <- c(1:9)
  lci <- summary[grep("log.sigma.year", rownames(summary)), 3]
  uci <- summary[grep("log.sigma.year", rownames(summary)), 7]
  
  plot(-21, xlim = c(1,9), ylim = c(min(lci),max(uci)), ylab = "sigma", xlab = "Year", axes = FALSE)
  
  axis(1, at = c(1, 3, 5, 7, 9), labels = c("2010", "2012", "2014", "2016", "2018"))
  axis(2)
  
  points(summary[grep("log.sigma.year", rownames(summary)), 1], pch = 16)
  arrows(x,lci,x,uci, code=3, angle=90, length=0.04)
  mtext(s_good[i], side = 3, line = 1, cex = 0.8)
}

mtext("Coefficient", side = 2, line = 1, cex = 0.8, outer = TRUE)
mtext("Year", side = 1, line = 1, cex = 0.8, outer = TRUE)

dev.off()

#####
setwd("S:/PhD/Second chapter/Data/Results/Plots/6temp/sig")
pdf("Yearsig_4.pdf")

par(mfrow = c(4,2),
    mar = c(3,2,3,2),
    oma = c(2,3,1,2))

for (i in 19:26){
  summary <- data.frame(species[[i]][[2]])
  
  x <- c(1:9)
  lci <- summary[grep("log.sigma.year", rownames(summary)), 3]
  uci <- summary[grep("log.sigma.year", rownames(summary)), 7]
  
  plot(-21, xlim = c(1,9), ylim = c(min(lci),max(uci)), ylab = "sigma", xlab = "Year", axes = FALSE)
  
  axis(1, at = c(1, 3, 5, 7, 9), labels = c("2010", "2012", "2014", "2016", "2018"))
  axis(2)
  
  points(summary[grep("log.sigma.year", rownames(summary)), 1], pch = 16)
  arrows(x,lci,x,uci, code=3, angle=90, length=0.04)
  mtext(s_good[i], side = 3, line = 1, cex = 0.8)
}

mtext("Coefficient", side = 2, line = 1, cex = 0.8, outer = TRUE)
mtext("Year", side = 1, line = 1, cex = 0.8, outer = TRUE)

dev.off()

####################################################################
# Check standard deviation of the year random effect in sigma
# So: Check the sig.sig.year (because the mean is centered in 0)

setwd("S:/PhD/Second chapter/Data")
s <- read.csv("sp_trend_dg.csv", sep = ";")
s_good <- as.vector(s$Species[which(s$include_samplesize == 1)])
remove_6 <- c("CACHL", "CAINA", "CIJUN", "COCOT", "COLIV", "LUARB", "LUMEG", "MIMIG", "OEHIS", "ORORI", "PIVIR", "PYRAX", "STUNI", "STVUL", "TUMER", "TUVIS")
s_good <- s_good[-which(s_good %in% remove_6)] # SPECIES THAT CONVERGE FOR MODEL 6

# Create table to store SD and abundance of each species

y_effect <- as.data.frame(matrix(NA, ncol = 2, nrow = length(s_good)))
colnames(y_effect) <- c("species", "year_sd")
y_effect$species <- s_good

for (xxx in 1:length(s_good)){
  
  setwd("S:/PhD/Second chapter/Data/Results/TRIM/6temp")
  load(paste("HDS_",s_good[xxx],".RData", sep = ""))
  
  y_effect[xxx, 2] <- out$mean$sig.sig # sd observer
  
  sum <- as.data.frame(out$summary)
  yearly_abundances <- sum[grep("popindex", rownames(sum)),1]
  obs_ab[xxx, 3] <- mean(yearly_abundances) # average abundance
}

