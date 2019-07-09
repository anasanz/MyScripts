
rm(list=ls())


##################################################################################################
###############     DETECTION CURVE PER OBSERVER IN SIGNIFICANT SPECIES     ######################
##################################################################################################

# 1. Load data and select significant species

setwd("S:/PhD/Second chapter/Data")

d <- read.csv("DataDS_ready_ALL.csv")
colnames(d)[which(colnames(d) == "Count")] <- "Cluster" 
s_good <- c("MICAL", "PADOM", "MEAPI", "TERAX")

c <- palette(rainbow(15)) 

# 2. Define detection function
g <- function(x, sig) exp(-x^2/(2*sig^2)) 
x <- runif(40, 0, 200)

setwd("S:/PhD/Second chapter/Data/Results/Final")

pdf("detectfunc_observer_s.pdf")

par(mfrow = c(4,2),
    mar = c(2,3,1,2),
    oma = c(2,4,1,1))

for (xxx in 1:length(s_good)){
  
  # Select distance data (for bars of histogram per observer)
  sp <- d[which(d$Species == s_good[xxx]), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "Observer", "Cluster", "Temp", "distance"))] # Select species spAL and all years
  
  #par(mfrow = c(1,2))
  hist(sp$distance, breaks = c(0,25,50,99,200), main = " ", col = "grey") # Frequency ALL distances (for one species, all observers)
  mtext("Frequency", side = 2, line = 3, cex = 0.7)
  mtext(s_good[xxx], side = 2, line = 5, cex = 1.1)
  
  ob <- unique(sp$Observer)
  
  # Load model (for different sigma mean per observer)
  setwd("S:/PhD/Second chapter/Data/Results/TRIM/compiled_final")
  load(paste("HDS_",s_good[xxx],".RData", sep = ""))
  sum <- out$summary
  
  sigmas <- sum[grep("sig.obs", rownames(sum)), 1] # Sigmas are the log of the sigmas in reality?
  exp.sig <- exp(sigmas) # Back transform to obtain sigma in the natural scale?????
  
  curve(g(x, sig=exp.sig[1]), 0, 200, xlab="Distance (x)", ylab="Detection prob.", lwd = 2, frame = F, col = c[1], ylim = c(0,1)) 
  mtext("Detection probability", side = 2, line = 3, cex = 0.7)
  
  for (i in 2:length(ob)){ # For each species
    curve(g(x, sig=exp.sig[i]), 0, 200, add=TRUE, lwd = 2, col = c[i])
  }
}
mtext("Distance (m)", side = 1, line = 1, cex = 0.8, outer = TRUE)
dev.off()


# FOR NON-SIGNIFICANT SPECIES #

s_good <- c("ALRUF","BUOED","CACAR","COOEN","COPAL","GACRI","GATHE","MEAPI","MECAL","PAMAJ","SESER","STSSP","SYCAN","SYMEL","TERAX","UPEPO",
            "MICAL","HIRUS","PADOM","PIPIC","PAMON", "COMON") 
s_good <- sort(s_good)
s_good <- s_good[-which(s_good %in% c("MICAL", "PADOM", "MEAPI", "TERAX"))]

setwd("S:/PhD/Second chapter/Data/Results/Final")

pdf("detectfunc_observer_ns1.pdf")

par(mfrow = c(5,2),
    mar = c(2,3,1,2),
    oma = c(2,4,1,1))

for (xxx in 1:10){
  
  # Select distance data (for bars of histogram per observer)
  sp <- d[which(d$Species == s_good[xxx]), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "Observer", "Cluster", "Temp", "distance"))] # Select species spAL and all years
  
  ob <- unique(sp$Observer)
  
  # Load model (for different sigma mean per observer)
  setwd("S:/PhD/Second chapter/Data/Results/TRIM/compiled_final")
  load(paste("HDS_",s_good[xxx],".RData", sep = ""))
  sum <- out$summary
  
  sigmas <- sum[grep("sig.obs", rownames(sum)), 1] # Sigmas are the log of the sigmas in reality?
  exp.sig <- exp(sigmas) # Back transform to obtain sigma in the natural scale?????
  
  curve(g(x, sig=exp.sig[1]), 0, 200, xlab="Distance (x)", ylab= " ", lwd = 2, frame = F, col = c[1], ylim = c(0,1)) 
  mtext(s_good[xxx], side = 2, line = 3, cex = 0.7)
  
  for (i in 2:length(ob)){ # For each species
    curve(g(x, sig=exp.sig[i]), 0, 200, add=TRUE, lwd = 2, col = c[i])
  }
}
mtext("Distance (m)", side = 1, line = 1, cex = 0.8, outer = TRUE)
mtext("Detection probability", side = 2, line = 2, cex = 0.8, outer = TRUE)

dev.off()



setwd("S:/PhD/Second chapter/Data/Results/Final")

pdf("detectfunc_observer_ns2.pdf")

par(mfrow = c(4,2),
    mar = c(2,3,1,2),
    oma = c(2,4,1,1))

for (xxx in 11:18){
  
  # Select distance data (for bars of histogram per observer)
  sp <- d[which(d$Species == s_good[xxx]), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "Observer", "Cluster", "Temp", "distance"))] # Select species spAL and all years
  
  ob <- unique(sp$Observer)
  
  # Load model (for different sigma mean per observer)
  setwd("S:/PhD/Second chapter/Data/Results/TRIM/compiled_final")
  load(paste("HDS_",s_good[xxx],".RData", sep = ""))
  sum <- out$summary
  
  sigmas <- sum[grep("sig.obs", rownames(sum)), 1] # Sigmas are the log of the sigmas in reality?
  exp.sig <- exp(sigmas) # Back transform to obtain sigma in the natural scale?????
  
  curve(g(x, sig=exp.sig[1]), 0, 200, xlab="Distance (x)", ylab= " ", lwd = 2, frame = F, col = c[1], ylim = c(0,1)) 
  mtext(s_good[xxx], side = 2, line = 3, cex = 0.7)
  
  for (i in 2:length(ob)){ # For each species
    curve(g(x, sig=exp.sig[i]), 0, 200, add=TRUE, lwd = 2, col = c[i])
  }
}
mtext("Distance (m)", side = 1, line = 1, cex = 0.8, outer = TRUE)
mtext("Detection probability", side = 2, line = 2, cex = 0.8, outer = TRUE)

dev.off()



