rm(list=ls())

########################################################
####          Check results OBSERVER 3.2           #####
######################################################## 

# Load data

setwd("S:/PhD/Second chapter/Data")
s <- read.csv("sp_trend_dg.csv", sep = ";")
s_good <- as.vector(s$Species[which(s$include_samplesize == 1)])
remove_3.2 <- c("CACHL", "CAINA", "CIJUN", "COCOT", "COLIV", "LUARB","LUMEG","MIMIG", "OEHIS","ORORI","PIVIR", "PYRAX","SESER","STUNI", "STVUL","TUMER", "TUVIS")
s_good <- s_good[-which(s_good %in% remove_3.2)] # SPECIES THAT CONVERGE FOR MODEL 3.2


setwd("S:/PhD/Second chapter/Data/Results/TRIM/3.2autoreg_simple")
load("spConvergence_light.RData")

# Make s_good with the same order than species[[]]
species <- species[-c(4, 5, 19, 24, 25)]

#### OBSERVER ####

setwd("S:/PhD/Second chapter/Data/Results/Plots/3.2autoreg_simple/sig")
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
setwd("S:/PhD/Second chapter/Data/Results/Plots/3.2autoreg_simple/sig")
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
setwd("S:/PhD/Second chapter/Data/Results/Plots/3.2autoreg_simple/sig")
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
setwd("S:/PhD/Second chapter/Data/Results/Plots/3.2autoreg_simple/sig")
pdf("Obs_4.pdf")

par(mfrow = c(4,2),
    mar = c(2,2,3,2),
    oma = c(2,3,1,2))

for (i in 19:25){
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


#### YEAR ####

setwd("S:/PhD/Second chapter/Data/Results/Plots/3.2autoreg_simple/sig")
pdf("Yearsig_1.pdf")

par(mfrow = c(3,2),
    mar = c(2,2,3,2),
    oma = c(2,3,1,2))

for (i in 1:6){
  
  summary <- data.frame(species[[i]][[2]])

  x <- c(1:9)
  lci <- summary[grep("log.sigma.year", rownames(summary)), 3]
  uci <- summary[grep("log.sigma.year", rownames(summary)), 7]
  
  plot(-21, xlim = c(1,9), ylim = c(min(lci),max(uci)), ylab = "sigma", xlab = "Year")
  points(summary[grep("log.sigma.year", rownames(summary)), 1], pch = 16)
  arrows(x,lci,x,uci, code=3, angle=90, length=0.04)
  mtext(s_good[i], side = 3, line = 1, cex = 0.8)
}

mtext("Coefficient", side = 2, line = 1, cex = 0.8, outer = TRUE)
mtext("Observer ID", side = 1, line = 1, cex = 0.8, outer = TRUE)

dev.off()

######
setwd("S:/PhD/Second chapter/Data/Results/Plots/3.2autoreg_simple/sig")
pdf("Yearsig_2.pdf")

par(mfrow = c(3,2),
    mar = c(2,2,3,2),
    oma = c(2,3,1,2))

for (i in 7:12){
  summary <- data.frame(species[[i]][[2]])
  
  x <- c(1:9)
  lci <- summary[grep("log.sigma.year", rownames(summary)), 3]
  uci <- summary[grep("log.sigma.year", rownames(summary)), 7]
  
  plot(-21, xlim = c(1,9), ylim = c(min(lci),max(uci)), ylab = "sigma", xlab = "Year")
  points(summary[grep("log.sigma.year", rownames(summary)), 1], pch = 16)
  arrows(x,lci,x,uci, code=3, angle=90, length=0.04)
  mtext(s_good[i], side = 3, line = 1, cex = 0.8)
}

mtext("Coefficient", side = 2, line = 1, cex = 0.8, outer = TRUE)
mtext("Observer ID", side = 1, line = 1, cex = 0.8, outer = TRUE)

dev.off()

#####
setwd("S:/PhD/Second chapter/Data/Results/Plots/3.2autoreg_simple/sig")
pdf("Yearsig_3.pdf")

par(mfrow = c(3,2),
    mar = c(2,2,3,2),
    oma = c(2,3,1,2))

for (i in 13:18){
  summary <- data.frame(species[[i]][[2]])
  
  x <- c(1:9)
  lci <- summary[grep("log.sigma.year", rownames(summary)), 3]
  uci <- summary[grep("log.sigma.year", rownames(summary)), 7]
  
  plot(-21, xlim = c(1,9), ylim = c(min(lci),max(uci)), ylab = "sigma", xlab = "Year")
  points(summary[grep("log.sigma.year", rownames(summary)), 1], pch = 16)
  arrows(x,lci,x,uci, code=3, angle=90, length=0.04)
  mtext(s_good[i], side = 3, line = 1, cex = 0.8)
}

mtext("Coefficient", side = 2, line = 1, cex = 0.8, outer = TRUE)
mtext("Observer ID", side = 1, line = 1, cex = 0.8, outer = TRUE)

dev.off()

#####
setwd("S:/PhD/Second chapter/Data/Results/Plots/3.2autoreg_simple/sig")
pdf("Yearsig_4.pdf")

par(mfrow = c(4,2),
    mar = c(2,2,3,2),
    oma = c(2,3,1,2))

for (i in 19:25){
  summary <- data.frame(species[[i]][[2]])
  
  x <- c(1:9)
  lci <- summary[grep("log.sigma.year", rownames(summary)), 3]
  uci <- summary[grep("log.sigma.year", rownames(summary)), 7]
  
  plot(-21, xlim = c(1,9), ylim = c(min(lci),max(uci)), ylab = "sigma", xlab = "Year")
  points(summary[grep("log.sigma.year", rownames(summary)), 1], pch = 16)
  arrows(x,lci,x,uci, code=3, angle=90, length=0.04)
  mtext(s_good[i], side = 3, line = 1, cex = 0.8)
}

mtext("Coefficient", side = 2, line = 1, cex = 0.8, outer = TRUE)
mtext("Observer ID", side = 1, line = 1, cex = 0.8, outer = TRUE)

dev.off()

