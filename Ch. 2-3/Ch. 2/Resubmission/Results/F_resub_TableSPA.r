
rm(list=ls())


###### Table site-specific (SPA) abundance estimates ######

# Load data (example for when I have the final results compiled in "spConvergence_light.....RData"))


hr <- c("SYCAN", "CACHL", "LASEN", "ALRUF", "PAMAJ", "ALARV", "CABRA", "CACAR", "COPAL", "PIPIC",
            "COOEN", "HIRUS", "PYRAX", "CAINA", "COMON", "PADOM", "PAMON", "FATIN", "SESER", "TUMER", "GATHE",
            "BUOED", "SYMEL", "UPEPO", "GACRI", "STSSP", "MICAL", "MEAPI", "TERAX", "MECAL") # Species in order of appearance in the list

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Resubmission/Results/Compiled_FINAL")
load("spConvergence_light_resub.RData")

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

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Resubmission/Results/Final")

# Take the 2 species with highest (ALRUF, BUOED) and 2 species with lowest probability of decline (COMON, PADOM)

for (i in 1:length(hr)){ # to check that s_good and species are in the same order
  print(species[[i]][[1]])
} # 4,22,15,16

alruf <- ab_zepa_sp[[4]][[2]]
buoed <- ab_zepa_sp[[22]][[2]]
comon <- ab_zepa_sp[[15]][[2]]
padom <- ab_zepa_sp[[16]][[2]]


table <- rbind(alruf, buoed, comon, padom)

write.csv(table,"ab_spa.csv")
