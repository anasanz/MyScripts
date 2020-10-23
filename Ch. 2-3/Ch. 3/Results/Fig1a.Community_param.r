
rm(list=ls())

# Community parameters

library(rjags)
library(jagsUI)
library(dplyr)

source("D:/PhD/MyScripts/Ch. 2-3/Ch. 3/Results/Functions/ProcessCodaOutput.R")
source("D:/PhD/MyScripts/Ch. 2-3/Ch. 3/Results/Functions/plot.violins3.r")
source("D:/PhD/MyScripts/Ch. 2-3/Ch. 3/Results/Functions/DoScale.r")

# Load species analyzed in the model

setwd("D:/PhD/Third chapter/Data")
d <- read.csv("DataDS_ch3_15_19_READY_FIXED_LAST_GASSP.csv")
bad_bp <- c("GACRI", "GATHE", "PADOM", "STSSP") # to remove all species with bad bp except MICAL and MECAL
d <- d[-which(d$Species %in% bad_bp), ]

sp <- as.character(unique(d$Species))
sp <- sort(sp)
nSpecies <- length(sp)

# Load the three chains
load("D:/PhD/Third chapter/Data/model/14.2.8_f/14.2.8_f_results.RData")



# Community MEAN
table_mu <- data.frame(matrix(NA,ncol = 4, nrow = 5))
colnames(table_mu) <- c("variable", "est", "lci", "uci")
var_mu <- c("mu_a1", "mu_a2", "mu_a3", "mu_cd", "mu_fs")
var_mu_names <- c("Fallow TFM", "Fallow AES", "Fallow GREEN", "Crop richness", "Field size")

for (i in 1:length(var_mu)){
table_mu[i,1] <- var_mu_names[i]
table_mu[i,2] <- out$mean[names(out$mean) %in% var_mu[i]]
table_mu[i,3] <- out$q2.5[names(out$q2.5) %in% var_mu[i]]
table_mu[i,4] <- out$q97.5[names(out$q97.5) %in% var_mu[i]]
}

# Community SD
table_sd <- data.frame(matrix(NA, ncol = 2, nrow = 5))
colnames(table_mu) <- c("variable", "sd")
var_mu <- c("mu_a1", "mu_a2", "mu_a3", "mu_cd", "mu_fs")
var_mu_names <- c("Fallow TFM", "Fallow AES", "Fallow GREEN", "Crop richness", "Field size")

for (i in 1:length(var_mu)){
  table_sd[i,1] <- var_mu_names[i]
  table_sd[i,2] <- out$sd[names(out$sd) %in% var_mu[i]]
}

# Community SIGMA (spread of results among species)
table_sig <- data.frame(matrix(NA,ncol = 4, nrow = 5))
colnames(table_sig) <- c("variable", "est", "lci", "uci")
var_sig <- c("sig_a1", "sig_a2", "sig_a3", "sig_cd", "sig_fs")
var_sig_names <- c("Fallow TFM", "Fallow AES", "Fallow GREEN", "Crop richness", "Field size")

for (i in 1:length(var_sig)){
  table_sig[i,1] <- var_sig_names[i]
  table_sig[i,2] <- out$mean[names(out$mean) %in% var_sig[i]]
  table_sig[i,3] <- out$q2.5[names(out$q2.5) %in% var_sig[i]]
  table_sig[i,4] <- out$q97.5[names(out$q97.5) %in% var_sig[i]]
}

setwd("D:/PhD/Third chapter/Data/Results_species/14.2/14.2.8_f")
pdf("Fig1_14.2.8_f.pdf",6,4)
par(mfrow = c(1,1),
    mar = c(4,7,3,2))

# Community Mean

plot(511, xlim = c(-0.35, 0.4), ylim = c(0,5.5), yaxs= "i", xaxs= "i", xlab = " ", ylab = " ", axes = FALSE)
axis(1, at = c(-0.35, -0.2, 0,  0.2), labels = c(" ", "-0.2", "0",  "0.2"), cex.axis = 0.85)
mtext(expression(paste("Community mean (",mu[beta],")")), side = 1, line = 2.5, cex = 0.85)

y_at <- c(5,4,3,2,1,0)
axis(2, at = y_at, labels = c(var_sig_names, " "), las = 2, cex.axis = 0.85)

abline(v = 0, col = "red", lty = 2, lwd = 1.5)

arrows(table_mu$lci, y_at[-6], table_mu$uci, y_at[-6], code = 3, angle = 90, length = 0.02, col = "black")
points(table_mu$est, y_at[-6], pch = 19 , col = "black", cex = 0.6, bg = "white")


dev.off()
