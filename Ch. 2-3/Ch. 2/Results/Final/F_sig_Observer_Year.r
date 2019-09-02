rm(list=ls())

########################################################
####          Check results OBSERVER 6           #####
######################################################## 

# Load data

s_good <- c("ALRUF","BUOED","CACAR","COOEN","COPAL","GACRI","GATHE","MEAPI","MECAL","PAMAJ","SESER","STSSP","SYCAN","SYMEL","TERAX","UPEPO",
            "MICAL","HIRUS","PADOM","PIPIC","PAMON", "COMON") 

setwd("C:/Users/Ana/Documents/PhD/PhD_21_Junio_2019/Second chapter/Data/Results/TRIM/6temp/Final")
load("spConvergence_light_FINAL.RData")


#### OBSERVER ####

setwd("S:/PhD/Second chapter/Data/Results/Plots/6temp/sig")
setwd("C:/Users/Ana/Documents/PhD/PhD_21_Junio_2019/Second chapter/Data/Results/Final")

pdf("Obs_random_1.pdf")

par(mfrow = c(4,2),
    mar = c(2,2,3,2),
    oma = c(2,3,1,2))

for (i in 1:8){
  
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
setwd("C:/Users/Ana/Documents/PhD/PhD_21_Junio_2019/Second chapter/Data/Results/Final")

pdf("Obs_random_2.pdf")

par(mfrow = c(4,2),
    mar = c(2,2,3,2),
    oma = c(2,3,1,2))

for (i in 9:16){
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

setwd("C:/Users/Ana/Documents/PhD/PhD_21_Junio_2019/Second chapter/Data/Results/Final")
pdf("Obs_random_3.pdf")

par(mfrow = c(3,2),
    mar = c(2,2,3,2),
    oma = c(2,3,1,2))

for (i in 17:22){
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
setwd("C:/Users/Ana/Documents/PhD/PhD_21_Junio_2019/Second chapter/Data/Results/Final")

pdf("Year_random_1.pdf")

par(mfrow = c(4,2),
    mar = c(3,2,3,2),
    oma = c(2,3,1,2))

for (i in 1:8){
  
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

setwd("C:/Users/Ana/Documents/PhD/PhD_21_Junio_2019/Second chapter/Data/Results/Final")
pdf("Year_random_2.pdf")

par(mfrow = c(4,2),
    mar = c(3,2,3,2),
    oma = c(2,3,1,2))

for (i in 9:16){
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

setwd("C:/Users/Ana/Documents/PhD/PhD_21_Junio_2019/Second chapter/Data/Results/Final")
pdf("Year_random_3.pdf")

par(mfrow = c(3,2),
    mar = c(3,2,3,2),
    oma = c(2,3,1,2))

for (i in 17:22){
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
library(dplyr)

s_good <- c("ALRUF","BUOED","CACAR","COOEN","COPAL","GACRI","GATHE","MEAPI","MECAL","PAMAJ","SESER","STSSP","SYCAN","SYMEL","TERAX","UPEPO",
            "MICAL","HIRUS","PADOM","PIPIC","PAMON", "COMON") 
s_good <- sort(s_good)

# Create table to store sd for observer and year (mean estimate of sigma), and also the mode
# Sigma estimates for year and observer in table 2 is high, but in the figure S3 and S5 the CI are overlapping
# Estimate the MODE of sigma appart from the mean (and/or plot the posteriors?)

year_obs <- as.data.frame(matrix(NA, ncol = 8, nrow = length(s_good)))
colnames(year_obs) <- c("species", "mean_year_sd", "95CI_mean_year", "mode_year_sd", "mean_observer_sd", "95CI_mean_obs", "mode_obs_sd", "mean_ab")
year_obs$species <- s_good



for (xxx in 1:length(s_good)){
  
  setwd("S:/PhD/Second chapter/Data/Results/TRIM/compiled_final")
  load(paste("HDS_",s_good[xxx],".RData", sep = ""))
  outall <- do.call(rbind,out$samples) # 3 chains together (for calculating the mode)
  
  sum <- as.data.frame(out$summary)
  sum <- round(sum,3)
  
  # YEAR
  # Mean of the year sd and its associated 95% BCI
  sigyear <- sum[which(rownames(sum) == "sig.sig.year"), ]
  year_obs[xxx, 2] <- sigyear$mean
  year_obs[xxx, 3] <- paste("[", sigyear$`2.5%`, " - ", sigyear$`97.5%`, "]", sep = "")
  # Mode of the year sd
  dens_year <- density(outall[ ,which(colnames(outall) == "sig.sig.year")]) # Density of iterations for sig.obs
  # the output of density has an x and a y value, where x corresponds to the values in your chains, 
  # and y is the corresponding density of a given value (ie, how frequently it occurs in your chains). 
  mode_year <- dens_year$x[dens_year$y == max(dens_year$y)]
  # you can extract the value of x that corresponds to the highest density (value of y): that is your MODE
  year_obs[xxx, 4] <- mode_year
  
  # OBSERVER
  # Mean of the observer sd and its associated 95% BCI
  sigobs <- sum[which(rownames(sum) == "sig.sig"), ]
  year_obs[xxx, 5] <- sigobs$mean
  year_obs[xxx, 6] <- paste("[", sigobs$`2.5%`, " - ", sigobs$`97.5%`, "]", sep = "")
  # Mode of the observer sd
  dens_obs <- density(outall[ ,which(colnames(outall) == "sig.sig")]) # Density of iterations for sig.obs
  # the output of density has an x and a y value, where x corresponds to the values in your chains, 
  # and y is the corresponding density of a given value (ie, how frequently it occurs in your chains). 
  mode_obs <- dens_obs$x[dens_obs$y == max(dens_obs$y)]
  # you can extract the value of x that corresponds to the highest density (value of y): that is your MODE
  year_obs[xxx, 7] <- mode_obs
  
  yearly_abundances <- sum[grep("popindex", rownames(sum)),1]
  year_obs[xxx, 8] <- mean(yearly_abundances) # average abundance
}
year_obs$mean_ab <- round(year_obs$mean_ab,3)
year_obs$mode_year_sd <- round(year_obs$mode_year_sd,3)
year_obs$mode_obs_sd <- round(year_obs$mode_obs_sd,3)


# Join year_obs with legend for titles of the plots
setwd("S:/PhD/Second chapter/Data")
leg <- read.csv("leg_species.csv", sep = ";")
leg <- arrange(leg,codiEspecie)
colnames(leg)[1] <- "species"

year_obs <- left_join(year_obs, leg, by = "species")

setwd("S:/PhD/Second chapter/Data/Results/Paper")
write.csv(year_obs, "sigma_random_mean_mode.csv")

# Delete COMON because of bad bayesian p-values
year_obs <- year_obs[-which(year_obs$species == "COMON"), ]



# Summary statistics


obs_mean <- arrange(year_obs, mean_observer_sd) # PIPIC, COPAL, COOEN, PAMAJ, STSSP(>3q)
 order_sp_obs <- obs_mean$species
 order_sp_obs_legend <- obs_mean$English
obs_mode <- arrange(year_obs, mode_obs_sd) # PIPIC, COPAL, STSSP, SESER, PAMAJ(>3q)

summary <- as.data.frame(matrix(NA, nrow = 4, ncol = 8))
colnames(summary) <- c("Variable", "SD estimate", "Min", "1st Quantile", "Median", "Mean", "3rd Quantile", "Max")
summary$Variable <- c("Observer", "Observer", "Year", "Year")
summary$`SD estimate` <- c("Mean", "Mode", "Mean", "Mode")
summary[1,c(3:8)] <- summary(year_obs$mean_observer_sd)
summary[2,c(3:8)] <- summary(year_obs$mode_obs_sd)

year_mean <- arrange(year_obs, mean_year_sd) # COOEN,PAMAJ,SESER,PIPIC,BUOED (>3q)
  order_sp_year <- year_mean$species
  order_sp_year_legend <- year_mean$English
year_mode <- arrange(year_obs, mode_year_sd) # COOEN,PAMAJ,SESER,ALRUF,PIPIC,   SYMEL(>3q)
summary[3,c(3:8)] <- summary(year_obs$mean_year_sd)
summary[4,c(3:8)] <- summary(year_obs$mode_year_sd)

summary <- cbind(summary[ ,c(1,2)], round(summary[ ,c(3:8)], 3))

setwd("S:/PhD/Second chapter/Data/Results/Paper")
write.csv(summary, "summary_species_SD.csv")



#### Plot mean and mode and CI for species with higher mean and mode ####

#OBSERVER


setwd("S:/PhD/Second chapter/Data/Results/Final")
pdf("Posterior_observerSD.pdf")

sp_obs <- c( "PIPIC", "COPAL", "COOEN", "PAMAJ", "STSSP", "SESER")

par(mfrow = c(3,2))

# Posterior distribution for standard deviation of year random effect (highest mean and mode)
for (xxx in 1:length(sp_obs)){
  
  setwd("S:/PhD/Second chapter/Data/Results/TRIM/compiled_final")
  load(paste("HDS_",sp_obs[xxx],".RData", sep = ""))
  outall <- do.call(rbind,out$samples) # 3 chains together
  
  # Posteriors and mode observers
  dens_obs <- density(outall[ ,which(colnames(outall) == "sig.sig")]) # Density of iterations for sig.obs
  mode_obs <- dens_obs$x[dens_obs$y == max(dens_obs$y)]
  mean_obs <- mean(outall[ ,which(colnames(outall) == "sig.sig")])
  
  plot(dens_obs, xlim = c(0, quantile(dens_obs$x,probs = 0.75)), xlab = " ", ylab = " ", main = sp_obs[xxx]) # POSTERIOR (x_lim is third quantile)
  abline( v = mode_obs, col = "blue", lwd = 2)
  abline( v = mean_obs, col = "red", lwd = 2)
}
mtext("Density", line = -1.5, side = 2, outer = TRUE) 
mtext("Observer SD estimate ", line = -1.5, side = 1, outer = TRUE) 

dev.off()

# YEAR

setwd("S:/PhD/Second chapter/Data/Results/Final")
pdf("Posterior_yearSD.pdf")

sp_year <- c("COOEN", "PAMAJ", "SESER", "PIPIC", "ALRUF", "BUOED")

par(mfrow = c(3,2))

# Posterior distribution for standard deviation of year random effect (highest mean and mode)
for (xxx in 1:length(sp_year)){
  
  setwd("S:/PhD/Second chapter/Data/Results/TRIM/compiled_final")
  load(paste("HDS_",sp_year[xxx],".RData", sep = ""))
  outall <- do.call(rbind,out$samples) # 3 chains together
  
  # Posteriors and mode year
  dens_year <- density(outall[ ,which(colnames(outall) == "sig.sig.year")]) # Density of iterations for sig.year
  mode_year <- dens_year$x[dens_year$y == max(dens_year$y)]
  mean_year <- mean(outall[ ,which(colnames(outall) == "sig.sig.year")])
  
  plot(dens_year, xlim = c(0, quantile(dens_year$x,probs = 0.75)), xlab = " ", ylab = " ", main = sp_year[xxx]) # POSTERIOR (x_lim is third quantile)
  abline( v = mode_year, col = "blue", lwd = 2)
  abline( v = mean_year, col = "red", lwd = 2)
}
mtext("Density", line = -1.5, side = 2, outer = TRUE) 
mtext("Year SD estimate ", line = -1.5, side = 1, outer = TRUE) 

dev.off()

############################################################################################
#### Plot mean and mode and CI for ALL species  ####

#OBSERVER


setwd("S:/PhD/Second chapter/Data/Results/Final")
pdf("Posterior_observerSD_allsp1.pdf")

order_sp_obs

par(mfrow = c(4,3),
    oma = c(3, 3, 3, 3),
    mar = c(3, 2, 2, 1) + 0.1)

# Posterior distribution for standard deviation of year random effect (highest mean and mode)
for (xxx in 1:12){
  
  setwd("S:/PhD/Second chapter/Data/Results/TRIM/compiled_final")
  load(paste("HDS_",order_sp_obs[xxx],".RData", sep = ""))
  outall <- do.call(rbind,out$samples) # 3 chains together
  
  # Posteriors and mode observers
  dens_obs <- density(outall[ ,which(colnames(outall) == "sig.sig")]) # Density of iterations for sig.obs
  mode_obs <- dens_obs$x[dens_obs$y == max(dens_obs$y)]
  mean_obs <- mean(outall[ ,which(colnames(outall) == "sig.sig")])
  
  #2plot(dens_obs, xlim = c(0, quantile(dens_obs$x,probs = 0.75)), xlab = " ", ylab = " ", main = " ") # POSTERIOR (x_lim is third quantile)
  plot(dens_obs, xlim = c(0, 1.5), xlab = " ", ylab = " ", main = " ") # POSTERIOR (x_lim is third quantile)
  mtext(order_sp_obs_legend[xxx], side = 3, line = 1, cex = 0.8)
  abline( v = mode_obs, col = "blue", lwd = 1.5)
  abline( v = mean_obs, col = "red", lwd = 1.5)
}
mtext("Density", line = 1, side = 2, outer = TRUE) 
mtext("Observer SD estimate ", line = 1, side = 1, outer = TRUE) 

dev.off()

setwd("S:/PhD/Second chapter/Data/Results/Final")
pdf("Posterior_observerSD_allsp2.pdf")

order_sp_obs

par(mfrow = c(4,3),
    oma = c(3, 3, 3, 3),
    mar = c(3, 2, 2, 1) + 0.1)

for (xxx in 13:22){
  
  setwd("S:/PhD/Second chapter/Data/Results/TRIM/compiled_final")
  load(paste("HDS_",order_sp_obs[xxx],".RData", sep = ""))
  outall <- do.call(rbind,out$samples) # 3 chains together
  
  # Posteriors and mode observers
  dens_obs <- density(outall[ ,which(colnames(outall) == "sig.sig")]) # Density of iterations for sig.obs
  mode_obs <- dens_obs$x[dens_obs$y == max(dens_obs$y)]
  mean_obs <- mean(outall[ ,which(colnames(outall) == "sig.sig")])
  
  #plot(dens_obs, xlim = c(0, quantile(dens_obs$x,probs = 0.75)), xlab = " ", ylab = " ", main = " ") # POSTERIOR (x_lim is third quantile)
  plot(dens_obs, xlim = c(0, 5), xlab = " ", ylab = " ", main = " ") # POSTERIOR (x_lim is third quantile)
  mtext(order_sp_obs_legend[xxx], side = 3, line = 1, cex = 0.8)
  abline( v = mode_obs, col = "blue", lwd = 1.5)
  abline( v = mean_obs, col = "red", lwd = 1.5)
}
mtext("Density", line = 1, side = 2, outer = TRUE) 
mtext("Observer SD estimate ", line = 1, side = 1, outer = TRUE) 

dev.off()

# YEAR

setwd("S:/PhD/Second chapter/Data/Results/Final")
pdf("Posterior_yearSD_allsp1.pdf")

par(mfrow = c(4,3),
    oma = c(3, 3, 3, 3),
    mar = c(3, 2, 2, 1) + 0.1)


# Posterior distribution for standard deviation of year random effect (highest mean and mode)
for (xxx in 1:12){
  
  setwd("S:/PhD/Second chapter/Data/Results/TRIM/compiled_final")
  load(paste("HDS_",order_sp_year[xxx],".RData", sep = ""))
  outall <- do.call(rbind,out$samples) # 3 chains together
  
  # Posteriors and mode year
  dens_year <- density(outall[ ,which(colnames(outall) == "sig.sig.year")]) # Density of iterations for sig.year
  mode_year <- dens_year$x[dens_year$y == max(dens_year$y)]
  mean_year <- mean(outall[ ,which(colnames(outall) == "sig.sig.year")])
  
  #plot(dens_year, xlim = c(0, quantile(dens_year$x,probs = 0.75)), xlab = " ", ylab = " ", main = " ") # POSTERIOR (x_lim is third quantile)
  plot(dens_year, xlim = c(0, 0.6), xlab = " ", ylab = " ", main = " ") # POSTERIOR (x_lim is third quantile)
  mtext(order_sp_year_legend[xxx], side = 3, line = 1, cex = 0.8)
  abline( v = mode_year, col = "blue", lwd = 2)
  abline( v = mean_year, col = "red", lwd = 2)
}
mtext("Density", line = 1, side = 2, outer = TRUE) 
mtext("Year SD estimate ", line = 1, side = 1, outer = TRUE) 

dev.off()

setwd("S:/PhD/Second chapter/Data/Results/Final")
pdf("Posterior_yearSD_allsp2.pdf")

par(mfrow = c(4,3),
    oma = c(3, 3, 3, 3),
    mar = c(3, 2, 2, 1) + 0.1)


# Posterior distribution for standard deviation of year random effect (highest mean and mode)
for (xxx in 13:22){
  
  setwd("S:/PhD/Second chapter/Data/Results/TRIM/compiled_final")
  load(paste("HDS_",order_sp_year[xxx],".RData", sep = ""))
  outall <- do.call(rbind,out$samples) # 3 chains together
  
  # Posteriors and mode year
  dens_year <- density(outall[ ,which(colnames(outall) == "sig.sig.year")]) # Density of iterations for sig.year
  mode_year <- dens_year$x[dens_year$y == max(dens_year$y)]
  mean_year <- mean(outall[ ,which(colnames(outall) == "sig.sig.year")])
  
  #plot(dens_year, xlim = c(0, quantile(dens_year$x,probs = 0.75)), xlab = " ", ylab = " ", main = " ") # POSTERIOR (x_lim is third quantile)
  plot(dens_year, xlim = c(0, 5), xlab = " ", ylab = " ", main = " ") # POSTERIOR (x_lim is third quantile)
  mtext(order_sp_year_legend[xxx], side = 3, line = 1, cex = 0.8)
  abline( v = mode_year, col = "blue", lwd = 2)
  abline( v = mean_year, col = "red", lwd = 2)
}
mtext("Density", line = 1, side = 2, outer = TRUE) 
mtext("Year SD estimate ", line = 1, side = 1, outer = TRUE) 

dev.off()





############################################################################################
#### ONLY ONE Plot mean and mode and CI for ALL species  ####

#OBSERVER

setwd("S:/PhD/Second chapter/Data/Results/Final")
pdf("Posterior_observerSD_allsp.pdf")

par(mfrow = c(7,3),
    oma = c(2, 2, 1, 2),
    mar = c(1.5, 1, 0.5, 0.5) + 0.1)

# Posterior distribution for standard deviation of year random effect (highest mean and mode)
for (xxx in 1:18){
  
  setwd("S:/PhD/Second chapter/Data/Results/TRIM/compiled_final")
  load(paste("HDS_",order_sp_obs[xxx],".RData", sep = ""))
  outall <- do.call(rbind,out$samples) # 3 chains together
  
  # Posteriors and mode observers
  dens_obs <- density(outall[ ,which(colnames(outall) == "sig.sig")]) # Density of iterations for sig.obs
  mode_obs <- dens_obs$x[dens_obs$y == max(dens_obs$y)]
  mean_obs <- mean(outall[ ,which(colnames(outall) == "sig.sig")])
  
  # Plots no x-axis
  plot(dens_obs, xlim = c(0, 4), xlab = " ", ylab = " ", main = " ", axes = FALSE) # POSTERIOR (x_lim is third quantile)
  #axis(2, labels = FALSE, lwd.ticks = 0)
  axis(1, labels = FALSE, lwd.ticks = 0)
  mtext(order_sp_obs_legend[xxx], side = 3, line = 0.5, cex = 0.8)
  abline( v = mode_obs, col = "blue", lwd = 1.2)
  abline( v = mean_obs, col = "red", lwd = 1.2)
}

# Posterior distribution for standard deviation of year random effect (highest mean and mode)
for (xxx in 19:21){
  
  setwd("S:/PhD/Second chapter/Data/Results/TRIM/compiled_final")
  load(paste("HDS_",order_sp_obs[xxx],".RData", sep = ""))
  outall <- do.call(rbind,out$samples) # 3 chains together
  
  # Posteriors and mode observers
  dens_obs <- density(outall[ ,which(colnames(outall) == "sig.sig")]) # Density of iterations for sig.obs
  mode_obs <- dens_obs$x[dens_obs$y == max(dens_obs$y)]
  mean_obs <- mean(outall[ ,which(colnames(outall) == "sig.sig")])
  
  # Plots no x-axis
  plot(dens_obs, xlim = c(0, 4), xlab = " ", ylab = " ", main = " ", axes = FALSE) # POSTERIOR (x_lim is third quantile)
  #axis(2, labels = FALSE, lwd.ticks = 0) 
  axis(1)
  mtext(order_sp_obs_legend[xxx], side = 3, line = 0.5, cex = 0.8)
  abline( v = mode_obs, col = "blue", lwd = 1.2)
  abline( v = mean_obs, col = "red", lwd = 1.2)
}

mtext("Density", line = 0.6, side = 2, outer = TRUE, cex = 0.8) 
mtext("Observer SD estimate ", line = 1, side = 1, outer = TRUE, cex = 0.8) 

dev.off()

# YEAR

setwd("S:/PhD/Second chapter/Data/Results/Final")
pdf("Posterior_yearSD_allsp.pdf")

par(mfrow = c(7,3),
    oma = c(2, 2, 1, 2),
    mar = c(1.5, 1, 0.5, 0.5) + 0.1)

# Posterior distribution for standard deviation of year random effect (highest mean and mode)
for (xxx in 1:18){
  
  setwd("S:/PhD/Second chapter/Data/Results/TRIM/compiled_final")
  load(paste("HDS_",order_sp_year[xxx],".RData", sep = ""))
  outall <- do.call(rbind,out$samples) # 3 chains together
  
  # Posteriors and mode observers
  dens_year <- density(outall[ ,which(colnames(outall) == "sig.sig.year")]) # Density of iterations for sig.obs
  mode_year <- dens_year$x[dens_year$y == max(dens_year$y)]
  mean_year <- mean(outall[ ,which(colnames(outall) == "sig.sig.year")])
  
  # Plots no x-axis
  plot(dens_year, xlim = c(0, 4), xlab = " ", ylab = " ", main = " ", axes = FALSE) # POSTERIOR (x_lim is third quantile)
  #axis(2, labels = FALSE, lwd.ticks = 0)
  axis(1, labels = FALSE, lwd.ticks = 0)
  mtext(order_sp_year_legend[xxx], side = 3, line = 0.5, cex = 0.8)
  abline( v = mode_year, col = "blue", lwd = 1.2)
  abline( v = mean_year, col = "red", lwd = 1.2)
}

# Posterior distribution for standard deviation of year random effect (highest mean and mode)
for (xxx in 19:21){
  
  setwd("S:/PhD/Second chapter/Data/Results/TRIM/compiled_final")
  load(paste("HDS_",order_sp_year[xxx],".RData", sep = ""))
  outall <- do.call(rbind,out$samples) # 3 chains together
  
  # Posteriors and mode yearervers
  dens_year <- density(outall[ ,which(colnames(outall) == "sig.sig.year")]) # Density of iterations for sig.year
  mode_year <- dens_year$x[dens_year$y == max(dens_year$y)]
  mean_year <- mean(outall[ ,which(colnames(outall) == "sig.sig.year")])
  
  # Plots no x-axis
  plot(dens_year, xlim = c(0, 4), xlab = " ", ylab = " ", main = " ", axes = FALSE) # POSTERIOR (x_lim is third quantile)
  #axis(2, labels = FALSE, lwd.ticks = 0) 
  axis(1)
  mtext(order_sp_year_legend[xxx], side = 3, line = 0.5, cex = 0.8)
  abline( v = mode_year, col = "blue", lwd = 1.2)
  abline( v = mean_year, col = "red", lwd = 1.2)
}

mtext("Density", line = 0.6, side = 2, outer = TRUE, cex = 0.8) 
mtext("Year SD estimate ", line = 1, side = 1, outer = TRUE, cex = 0.8) 

dev.off()



