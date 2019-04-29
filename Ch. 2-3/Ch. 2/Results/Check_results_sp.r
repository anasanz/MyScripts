
library(dplyr)



########################################################
####                  Check convergence           #####
########################################################


rm(list=ls())

setwd("S:/PhD/Second chapter/Data")

d <- read.csv("DataDS_ready_ALL.csv")
colnames(d)[which(colnames(d) == "Count")] <- "Cluster" 

library(rtrim)
library(dplyr)

setwd("S:/PhD/Second chapter/Data")

# Load species names
s <- read.csv("sp_trend_dg.csv", sep = ";")
s_good <- as.vector(s$Species[which(s$include_samplesize == 1)])
remove_6 <- c("CACHL", "CAINA", "CIJUN", "COCOT", "COLIV", "LUARB", "LUMEG", "MIMIG", "OEHIS", "ORORI", "PIVIR", "PYRAX", "STUNI", "STVUL", "TUMER", "TUVIS")

s_good <- s_good[-which(s_good %in% remove_6)]


setwd("S:/PhD/Second chapter/Data/Results/TRIM/6temp")


# HEAVY (WITH ALL THE MCMC OUTPUT)
species <- list()
at <- list()

for (xxx in 1:length(s_good)){
  
  load(paste("HDS_",s_good[xxx],".RData", sep = ""))
  
  # SPECIES
  at[[1]] <- paste("HDS_",s_good[xxx], sep = "")
  
  # JAGS OUTPUT
  at[[2]] <- assign(paste("out_",s_good[xxx], sep = ""), out)

  # CONVERGENCE
  summary <- as.data.frame(out$summary)
  at[[3]] <- summary[which(summary$Rhat > 1.1), ]
  
  # COUNTS PER YEAR
  d_tr <- d[ ,which(colnames(d) %in% c("Species",  "T_Y", "Observer"))]
  d_tr_all <- data.frame(T_Y = unique(d_tr$T_Y), id = NA)
  
  d_tr$Observer <- as.character(d_tr$Observer) 
  d_tr_all_obs <- left_join(d_tr_all, d_tr)
  d_tr_all_obs <- d_tr_all_obs[ ,c(1,4)]
  d_tr_all_obs <- d_tr_all_obs[which(!duplicated(d_tr_all_obs)), ] # Table with all sampled fields and which observer sampled it

  sp <- d[which(d$Species == s_good[xxx]), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "Observer", "Cluster"))] # Select species spAL and all years
  sp <- arrange(sp, Year, transectID) #Ordered
  sp_detec_transectID <- unique(sp$transectID)
  sp$Observer <- as.character(sp$Observer) 
  
  absent <- anti_join(d_tr_all,sp) # Transects with 0 abundance, add to sp.
  colnames(absent)[2] <- "Banda" # Format it to add the rows to sp
  absent$T_Y <- as.character(absent$T_Y)
  absent$Species <- s_good[xxx]
  absent$Cluster <- NA
  absent <- left_join(absent, d_tr_all_obs)
  
  for (i in 1:nrow(absent)){ # Format to join absent - detections
    cent <- substr(absent$T_Y[i], 10,10) # To include SI102 (cents)
    cent <- as.numeric(cent)
    if(is.na(cent)){
      
      absent$Year[i] <- substr(absent$T_Y[i], 6,9)
      absent$transectID[i] <- substr(absent$T_Y[i], 1,4)
      
    } else { absent$Year[i] <- substr(absent$T_Y[i], 7,10)
    absent$transectID[i] <- substr(absent$T_Y[i], 1,5)}
  }
  absent$count <- 0
  sp$count <- 1
  all_sp <- rbind(sp,absent) # Include transects with abundance 0
  all_sp <- arrange(all_sp, Year, transectID) # Ordered
  
  absent$count <- 0
  
  yrs <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018) # I HAVE TO CONVERT THIS FROM 0-7 (but nyrs is still 8!)
  nyrs <- length(yrs)
  
  all.sites <- unique(all_sp$transectID)
  all.sites <- sort(all.sites,descreasing = TRUE)
  max.sites <- length(all.sites)
  
  m <- matrix(NA, nrow = length(all.sites), ncol = nyrs)
  rownames(m) <- all.sites
  colnames(m) <- yrs
  
  count <- aggregate(Species ~ Year + transectID, FUN = length, data = sp)
  
  for (i in 1:nrow(count)){ # Add counts > 0
    m[which(rownames(m) %in% count$transectID[i]), which(colnames(m) %in% count$Year[i])] <- count$Species[i]}
      
  for (i in 1:nrow(absent)){ # Add absences (0)
    m[which(rownames(m) %in% absent$transectID[i]), which(colnames(m) %in% absent$Year[i])] <- absent$count[i]}
      
  
  at[[4]] <- colSums(m,na.rm = TRUE)
  
  species[[xxx]] <- at
  print(xxx)
}

save(species, "spConvergence.RData")


# LIGHT (WITH ONLY SUMMARY)

species <- list()
at <- list()

for (xxx in 1:length(s_good)){
  
  load(paste("HDS_",s_good[xxx],".RData", sep = ""))
  
  # SPECIES
  at[[1]] <- paste("HDS_",s_good[xxx], sep = "")
  
  # JAGS SUMMARY
  summary <- as.data.frame(out$summary)
  at[[2]] <- summary
  
  # CONVERGENCE
  at[[3]] <- summary[which(summary$Rhat > 1.1), ]
  
  # COUNTS PER YEAR
  d_tr <- d[ ,which(colnames(d) %in% c("Species",  "T_Y", "Observer"))]
  d_tr_all <- data.frame(T_Y = unique(d_tr$T_Y), id = NA)
  
  d_tr$Observer <- as.character(d_tr$Observer) 
  d_tr_all_obs <- left_join(d_tr_all, d_tr)
  d_tr_all_obs <- d_tr_all_obs[ ,c(1,4)]
  d_tr_all_obs <- d_tr_all_obs[which(!duplicated(d_tr_all_obs)), ] # Table with all sampled fields and which observer sampled it
  
  sp <- d[which(d$Species == s_good[xxx]), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "Observer", "Cluster"))] # Select species spAL and all years
  sp <- arrange(sp, Year, transectID) #Ordered
  sp_detec_transectID <- unique(sp$transectID)
  sp$Observer <- as.character(sp$Observer) 
  
  absent <- anti_join(d_tr_all,sp) # Transects with 0 abundance, add to sp.
  colnames(absent)[2] <- "Banda" # Format it to add the rows to sp
  absent$T_Y <- as.character(absent$T_Y)
  absent$Species <- s_good[xxx]
  absent$Cluster <- NA
  absent <- left_join(absent, d_tr_all_obs)
  
  for (i in 1:nrow(absent)){ # Format to join absent - detections
    cent <- substr(absent$T_Y[i], 10,10) # To include SI102 (cents)
    cent <- as.numeric(cent)
    if(is.na(cent)){
      
      absent$Year[i] <- substr(absent$T_Y[i], 6,9)
      absent$transectID[i] <- substr(absent$T_Y[i], 1,4)
      
    } else { absent$Year[i] <- substr(absent$T_Y[i], 7,10)
    absent$transectID[i] <- substr(absent$T_Y[i], 1,5)}
  }
  absent$count <- 0
  sp$count <- 1
  all_sp <- rbind(sp,absent) # Include transects with abundance 0
  all_sp <- arrange(all_sp, Year, transectID) # Ordered
  
  absent$count <- 0
  
  yrs <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018) # I HAVE TO CONVERT THIS FROM 0-7 (but nyrs is still 8!)
  nyrs <- length(yrs)
  
  all.sites <- unique(all_sp$transectID)
  all.sites <- sort(all.sites,descreasing = TRUE)
  max.sites <- length(all.sites)
  
  m <- matrix(NA, nrow = length(all.sites), ncol = nyrs)
  rownames(m) <- all.sites
  colnames(m) <- yrs
  
  count <- aggregate(Species ~ Year + transectID, FUN = length, data = sp)
  
  for (i in 1:nrow(count)){ # Add counts > 0
    m[which(rownames(m) %in% count$transectID[i]), which(colnames(m) %in% count$Year[i])] <- count$Species[i]}
  
  for (i in 1:nrow(absent)){ # Add absences (0)
    m[which(rownames(m) %in% absent$transectID[i]), which(colnames(m) %in% absent$Year[i])] <- absent$count[i]}
  
  
  at[[4]] <- colSums(m,na.rm = TRUE)
  
  species[[xxx]] <- at
  print(xxx)
}

save(species, file = "spConvergence_light.RData")



# LIGHT (WITH ONLY SUMMARY) FOR DOUBT

s_doubt <- as.vector(s$Species[which(s$Doubt_samplesize == 1)])
s_doubt <- s_doubt[-which(s_doubt == "PTALC")]

species <- list()
at <- list()

for (xxx in 1:length(s_doubt)){
  
  load(paste("5HDS_doubt_",s_doubt[xxx],".RData", sep = ""))
  
  # SPECIES
  at[[1]] <- paste("5HDS_doubt_",s_doubt[xxx], sep = "")
  
  # JAGS SUMMARY
  summary <- as.data.frame(out$summary)
  at[[2]] <- summary
  
  # CONVERGENCE
  at[[3]] <- summary[which(summary$Rhat > 1.1), ]
  
  # COUNTS PER YEAR
  d_tr <- d[ ,which(colnames(d) %in% c("Species",  "T_Y", "Observer"))]
  d_tr_all <- data.frame(T_Y = unique(d_tr$T_Y), id = NA)
  
  d_tr$Observer <- as.character(d_tr$Observer) 
  d_tr_all_obs <- left_join(d_tr_all, d_tr)
  d_tr_all_obs <- d_tr_all_obs[ ,c(1,4)]
  d_tr_all_obs <- d_tr_all_obs[which(!duplicated(d_tr_all_obs)), ] # Table with all sampled fields and which observer sampled it
  
  sp <- d[which(d$Species == s_doubt[xxx]), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "Observer", "Cluster"))] # Select species spAL and all years
  sp <- arrange(sp, Year, transectID) #Ordered
  sp_detec_transectID <- unique(sp$transectID)
  sp$Observer <- as.character(sp$Observer) 
  
  absent <- anti_join(d_tr_all,sp) # Transects with 0 abundance, add to sp.
  colnames(absent)[2] <- "Banda" # Format it to add the rows to sp
  absent$T_Y <- as.character(absent$T_Y)
  absent$Species <- s_doubt[xxx]
  absent$Cluster <- NA
  absent <- left_join(absent, d_tr_all_obs)
  
  for (i in 1:nrow(absent)){ # Format to join absent - detections
    cent <- substr(absent$T_Y[i], 10,10) # To include SI102 (cents)
    cent <- as.numeric(cent)
    if(is.na(cent)){
      
      absent$Year[i] <- substr(absent$T_Y[i], 6,9)
      absent$transectID[i] <- substr(absent$T_Y[i], 1,4)
      
    } else { absent$Year[i] <- substr(absent$T_Y[i], 7,10)
    absent$transectID[i] <- substr(absent$T_Y[i], 1,5)}
  }
  absent$count <- 0
  sp$count <- 1
  all_sp <- rbind(sp,absent) # Include transects with abundance 0
  all_sp <- arrange(all_sp, Year, transectID) # Ordered
  
  absent$count <- 0
  
  yrs <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018) # I HAVE TO CONVERT THIS FROM 0-7 (but nyrs is still 8!)
  nyrs <- length(yrs)
  
  all.sites <- unique(all_sp$transectID)
  all.sites <- sort(all.sites,descreasing = TRUE)
  max.sites <- length(all.sites)
  
  m <- matrix(NA, nrow = length(all.sites), ncol = nyrs)
  rownames(m) <- all.sites
  colnames(m) <- yrs
  
  count <- aggregate(Species ~ Year + transectID, FUN = length, data = sp)
  
  for (i in 1:nrow(count)){ # Add counts > 0
    m[which(rownames(m) %in% count$transectID[i]), which(colnames(m) %in% count$Year[i])] <- count$Species[i]}
  
  for (i in 1:nrow(absent)){ # Add absences (0)
    m[which(rownames(m) %in% absent$transectID[i]), which(colnames(m) %in% absent$Year[i])] <- absent$count[i]}
  
  
  at[[4]] <- colSums(m,na.rm = TRUE)
  
  species[[xxx]] <- at
  print(xxx)
}

save(species, file = "spConvergence_light_doubt.RData")



