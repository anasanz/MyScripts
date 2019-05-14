
#####################################################################################
##################  Relation observer differences - species abundance  ###############
#####################################################################################

rm(list=ls())

library(dplyr)
library(tidyr)
library(stats)

setwd("S:/PhD/Second chapter/Data")
s <- read.csv("sp_trend_dg.csv", sep = ";")
s_good <- as.vector(s$Species[which(s$include_samplesize == 1)])
remove_6 <- c("CACHL", "CAINA", "CIJUN", "COCOT", "COLIV", "LUARB", "LUMEG", "MIMIG", "OEHIS", "ORORI", "PIVIR", "PYRAX", "STUNI", "STVUL", "TUMER", "TUVIS")
s_good <- s_good[-which(s_good %in% remove_6)] # SPECIES THAT CONVERGE FOR MODEL 6

#for (xxx in 1:length(s_good)){
  xxx = 5
  setwd("S:/PhD/Second chapter/Data/Results/TRIM/6temp")
  load(paste("HDS_",s_good[xxx],".RData", sep = ""))
  
  outall <- do.call(rbind,out$samples) # All iterations from MCMC
  outall_obs <- as.data.frame(outall[ , grep("sig.obs", colnames(outall))])
  
  obs <- gather(outall_obs, key = "observer", value = "sigma", factor_key = TRUE)
  
  #ANOVA
  aov_obs <- aov(sigma ~ observer, data = obs)
  summary(aov_obs) # There are significant differences among groups. Which groups?
  TukeyHSD(aov_obs)
  
  # Does the data meet the assumptions of ANOVA??
  # 1. Homoneneity of variances
  plot(aov_obs, 1)#Flat line
  test_homog <- leveneTest(sigma ~ observer, data = obs) # NOT HOMOGENEOUS ACCORDING TO THE TEST (p<0.05)
  # 2. Normality
  plot(aov_obs, 2) # YES

  # Test that relaxes the equal variances assumption:
  pairwise_homog <- pairwise.t.test(obs$sigma, obs$observer, p.adjust.method = "BH", pool.sd = FALSE) 
  #### There is a lot of groups with significative differences among their means, but it is weird because in the 
  #### boxplot, it seems like the means and 95% CI overlap.
  
  #Boxplot (Same as Figure S1)
  boxplot(sigma ~ observer, data = obs,
          xlab = "Observer", ylab = "Sigma",
          frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))
  
  # Summary statistics
  group_by(obs, observer) %>%
    summarise(
      count = n(),
      mean = mean(sigma, na.rm = TRUE),
      sd = sd(sigma, na.rm = TRUE)
    )
  
  
  