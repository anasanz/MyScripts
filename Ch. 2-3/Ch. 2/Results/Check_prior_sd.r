rm(list=ls())

library(dplyr)


s_good <- c("ALRUF","BUOED","CACAR","COOEN","COPAL","GACRI","GATHE","MEAPI","MECAL","PAMAJ","SESER","STSSP","SYCAN","SYMEL","TERAX","UPEPO",
            "MICAL","HIRUS","PADOM","PIPIC","PAMON", "COMON") # It has to be in this order because the list is in this order

par(mfrow = c(2,3))
for (xxx in 19:22){
  
  setwd("S:/PhD/Second chapter/Data/Results/TRIM/compiled_final")
  load(paste("HDS_",s_good[xxx],".RData", sep = ""))
  outall <- do.call(rbind,out$samples) # 3 chains together
  
  # Posteriors and mode year
  dens_year <- density(outall[ ,which(colnames(outall) == "sd")]) # Density of iterations for sig.year
  mode_year <- dens_year$x[dens_year$y == max(dens_year$y)]
  mean_year <- mean(outall[ ,which(colnames(outall) == "sd")])
  
  plot(dens_year, xlim = c(0, quantile(dens_year$x,probs = 0.75)), xlab = " ", ylab = " ", main = s_good[xxx]) # POSTERIOR (x_lim is third quantile)
  abline( v = mode_year, col = "blue", lwd = 2)
  abline( v = mean_year, col = "red", lwd = 2)
}
