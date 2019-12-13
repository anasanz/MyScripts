
rm(list=ls())


###### Table site-specific (SPA) abundance estimates ######

# Load data (example for when I have the final results compiled in "spConvergence_light.....RData"))


hr <- c("TERAX", "BUOED", "TUMER","CACAR","COOEN","COPAL","GACRI","GATHE","MEAPI","MECAL","SESER","STSSP","SYMEL","UPEPO",
        "MICAL","HIRUS","PADOM","PIPIC","PAMON", "COMON", "FATIN", "LUARB", "COGAR", "PYRAX", "CAINA") # This has to be in the same order than sp_convergence

setwd("C:/Users/ana.sanz/Documents/PhD/Second chapter/Resubmission")
load("spConvergence_light_HR_resub.RData")

ab_zepa_sp <- list()

nzepas <- 8
nyears <- 9
ab_zepa <- matrix(NA, nrow = nzepas, ncol = nyears)
rownames(ab_zepa) <- c("AF", "AL", "BA", "BE", "BM", "GR", "NoZepa", "SI")
colnames(ab_zepa) <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)


for (s in 1:length(hr)){
  
sum <- species[[s]][[2]]
sum <- round(sum, 2)
sum_zepa <- sum[grep("popindex_zepa", rownames(sum)), ]

  for (t in 1:nyears){
  
  sum_zepa_mean <- sum_zepa[grep(paste(t,"]", sep = ""), rownames(sum_zepa)), 1] # To get the mean
  sum_zepa_sd <- sum_zepa[grep(paste(t,"]", sep = ""), rownames(sum_zepa)), 2] # To get the sd
  
  ab_zepa[,t] <- as.character(paste(sum_zepa_mean, "+/-", sum_zepa_sd))
  }

ab_zepa_sp[[s]] <- list(hr[s],ab_zepa)

}
