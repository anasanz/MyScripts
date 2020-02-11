
rm(list=ls())

### PLOT POSTERIORS AND PROBABILITY OF DECLINE ###

# Load data (example for when I have the final results compiled in "spConvergence_light.....RData"))


s_good <- c("SYCAN", "CACHL", "LASEN", "ALRUF", "PAMAJ", "ALARV", "CABRA", "CACAR", "COPAL", "PIPIC",
            "COOEN", "HIRUS", "PYRAX", "CAINA", "COMON", "PADOM", "PAMON", "FATIN", "SESER", "TUMER", "GATHE",
            "BUOED", "SYMEL", "UPEPO", "GACRI", "STSSP", "MICAL", "TERAX", "MEAPI", "MECAL") # This has to be in the same order than sp_convergence

s_good <- sort(s_good)
bad_bp <- c("GACRI", "STSSP", "MICAL", "TERAX", "MEAPI", "MECAL")
s_good <- s_good[-which(s_good %in% bad_bp)]

# Legend

library(dplyr)
setwd("D:/PhD/Second chapter/Data")
leg <- read.csv("leg_species.csv", sep = ";")
leg <- arrange(leg,codiEspecie)
colnames(leg)[1] <- "species"

# Remove bad bp from legend
leg <- leg[-which(leg$species %in% bad_bp),]

# Calculate first prob. of decline to sort it out (decreasing)

leg$PD <- NA

for (xxx in 1:length(s_good)){
  setwd("D:/PhD/Second chapter/Resubmission/Results/Compiled_FINAL")
  
  load(paste("HDS_",s_good[xxx],".RData", sep = ""))
  outall <- do.call(rbind,out$samples) # 3 chains together
  
  # Probability of decline: 
  # Using the posterior samples from the trend parameter: % of all posterior samples < 0
  df.outall <- as.data.frame(outall)
  total.samples <- nrow(df.outall)
  declining <- df.outall$bYear.lam[which(df.outall$bYear.lam < 0)]
  prob_declining <- (length(declining)/total.samples)*100
  leg$PD[leg$species == s_good[xxx]] <- prob_declining
}

leg_order_dec <- arrange(leg, desc(PD))
order_sp_obs_legend <- leg_order_dec$English
s_good_order <- leg_order_dec$species

setwd("D:/PhD/Second chapter/Resubmission/Results/Final")
pdf("Fig 5_2_PosteriorYear_Final.pdf")

par(mfrow = c(8,3),
    oma = c(2, 2, 1, 2),
    mar = c(1.5, 1, 1, 1) + 0.1)

for (xxx in 1:length(s_good_order)){
  setwd("D:/PhD/Second chapter/Resubmission/Results/Compiled_FINAL")
  
  load(paste("HDS_",s_good_order[xxx],".RData", sep = ""))
  outall <- do.call(rbind,out$samples) # 3 chains together
  
  # Posteriors and mode year
  dens_obs <- density(outall[ ,which(colnames(outall) == "bYear.lam")]) # Density of iterations for sig.obs
  mode_obs <- dens_obs$x[dens_obs$y == max(dens_obs$y)]
  mean_obs <- mean(outall[ ,which(colnames(outall) == "bYear.lam")])
  
  # Probability of decline: 
  # Using the posterior samples from the trend parameter: % of all posterior samples < 0
  
  prob_declining <- leg_order_dec$PD[xxx]
  
  #2plot(dens_obs, xlim = c(0, quantile(dens_obs$x,probs = 0.75)), xlab = " ", ylab = " ", main = " ") # POSTERIOR (x_lim is third quantile)
  plot(dens_obs, xlim = c(-0.5, 0.5), xlab = " ", ylab = " ", main = " ", axes = FALSE, col = "grey") # POSTERIOR (x_lim is third quantile)
  axis(1, pos = 0, tck = -0.05, cex.axis = 0.9, mgp = c(3, 0.2, 0))
  mtext(order_sp_obs_legend[xxx], side = 3, line = 0, cex = 0.8, adj = 0)
  mtext(paste("PD =", round(prob_declining,2)), side = 3, line = -1, cex = 0.7, adj = 0)
  
  polygon(c(dens_obs$x[dens_obs$x < 0], 0), c(dens_obs$y[dens_obs$x < 0],0 ), col="grey") # ?? I still don't know if its right
  #polygon(c(dens_obs$x[dens_obs$x < 0], min(dens_obs$x)), c(dens_obs$y[dens_obs$x < 0],0 ), col="grey") # ?? I still don't know if its right
  #abline( v = mode_obs, col = "blue", lwd = 1.5) # Remove mean and mode for visual purposes
  #abline( v = mean_obs, col = "red", lwd = 1.5)
  abline( v = 0, col = "black", lwd = 1)
}
mtext("Density", line = 0, side = 2, outer = TRUE) 
mtext("Trend estimate ", line = 0, side = 1, outer = TRUE) 

dev.off()