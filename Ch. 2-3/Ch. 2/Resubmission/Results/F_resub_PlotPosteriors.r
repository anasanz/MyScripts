
rm(list=ls())

### PLOT POSTERIORS ###

# Load data (example for when I have the final results compiled in "spConvergence_light.....RData"))


hr <- c("TERAX", "BUOED", "TUMER","CACAR","COOEN","COPAL","GACRI","GATHE","MEAPI","MECAL","SESER","STSSP","SYMEL","UPEPO",
        "MICAL","HIRUS","PADOM","PIPIC","PAMON", "COMON", "FATIN", "LUARB", "COGAR", "PYRAX", "CAINA") # This has to be in the same order than sp_convergence

sp <- hr[1:12]

# I need to load species from compiled folder


setwd("C:/Users/ana.sanz/Documents/PhD/Second chapter/Resubmission/Results")
pdf("Posterior_year_prueba.pdf")

par(mfrow = c(4,3),
    oma = c(3, 3, 3, 3),
    mar = c(3, 2, 2, 1) + 0.1)

for (xxx in 1:12){
  
  setwd("S:/Results/chapter2/HR")

  load(paste("HDS_",sp[xxx],".RData", sep = ""))
  outall <- do.call(rbind,out$samples) # 3 chains together
  
  # Posteriors and mode observers
  dens_obs <- density(outall[ ,which(colnames(outall) == "bYear.lam")]) # Density of iterations for sig.obs
  mode_obs <- dens_obs$x[dens_obs$y == max(dens_obs$y)]
  mean_obs <- mean(outall[ ,which(colnames(outall) == "bYear.lam")])
  
  #2plot(dens_obs, xlim = c(0, quantile(dens_obs$x,probs = 0.75)), xlab = " ", ylab = " ", main = " ") # POSTERIOR (x_lim is third quantile)
  plot(dens_obs, xlim = c(-0.5, 0.5), xlab = " ", ylab = " ", main = " ") # POSTERIOR (x_lim is third quantile)
  mtext(sp[xxx], side = 3, line = 1, cex = 0.8)
  abline( v = mode_obs, col = "blue", lwd = 1.5)
  abline( v = mean_obs, col = "red", lwd = 1.5)
}
mtext("Density", line = 1, side = 2, outer = TRUE) 
mtext("Trend estimate ", line = 1, side = 1, outer = TRUE) 

dev.off()