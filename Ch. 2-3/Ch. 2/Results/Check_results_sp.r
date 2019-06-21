
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


setwd("S:/PhD/Second chapter/Data/Results/TRIM/6temp/Final")


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

# s_good <- c("BUOED", "CIAER", "COMON", "COPAL", "FATIN", "MECAL", "PAMAJ")
which(s_good == "PAMAJ")

species[[27]][[3]]

s_good

##############################################################################################################
# Check results for dataset with species with another detection function

setwd("S:/PhD/Second chapter/Data/Results/TRIM/6temp/HR_df/++iter_burn")

s_good <- c("CIAER", "COGAR", "COMON", "FATIN", "HIRUS", "LASEN", "MICAL", "PADOM", "PAMON", "PIPIC")
s_good <- c("COGAR", "COMON", "LASEN", "PAMON")


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
species[[4]]
save(species, file = "spConvergence_light.RData")

load("spConvergence_light.RData")


##########################################################################################################
############################           COMPILE FINAL RESULTS            ##################################
##########################################################################################################

rm(list=ls())

setwd("S:/PhD/Second chapter/Data")

d <- read.csv("DataDS_ready_ALL.csv")
colnames(d)[which(colnames(d) == "Count")] <- "Cluster" 

library(rtrim)
library(dplyr)

setwd("S:/PhD/Second chapter/Data")

### 1. Species from half normal detection function ###

# Load species names
s <- read.csv("sp_trend_dg.csv", sep = ";")
s_good <- as.vector(s$Species[which(s$include_samplesize == 1)])
remove_6 <- c("CACHL", "CAINA", "CIJUN", "COCOT", "COLIV", "LUARB", "LUMEG", "MIMIG", "OEHIS", "ORORI", "PIVIR", "PYRAX", "STUNI", "STVUL", "TUMER", "TUVIS")

s_good <- s_good[-which(s_good %in% remove_6)] # All species
s_hr <- c("CIAER", "COGAR", "COMON", "FATIN", "HIRUS", "LASEN", "MICAL", "PADOM", "PAMON", "PIPIC") # species hazard rate (bad pvalues)

sp_hn <- s_good[-which(s_good %in% s_hr)] #Remove species hazard rate. 
#These are only species with half-normal function. 16 species:
#   160000 iterations all of them 
#     - except COPAL, MECAL, PAMAJ, STSSP, (SESER, SYCAN?), that have 170000
#     - except SESER, SYCAN that have 190,000

setwd("S:/PhD/Second chapter/Data/Results/TRIM/6temp/Final")

# LIGHT (WITH ONLY SUMMARY)

species <- list()
at <- list()

for (xxx in 1:length(sp_hn)){
  
  load(paste("HDS_",sp_hn[xxx],".RData", sep = ""))
  
  # SPECIES
  at[[1]] <- paste("HDS_",sp_hn[xxx], sep = "")
  
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
  
  sp <- d[which(d$Species == sp_hn[xxx]), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "Observer", "Cluster"))] # Select species spAL and all years
  sp <- arrange(sp, Year, transectID) #Ordered
  sp_detec_transectID <- unique(sp$transectID)
  sp$Observer <- as.character(sp$Observer) 
  
  absent <- anti_join(d_tr_all,sp) # Transects with 0 abundance, add to sp.
  colnames(absent)[2] <- "Banda" # Format it to add the rows to sp
  absent$T_Y <- as.character(absent$T_Y)
  absent$Species <- sp_hn[xxx]
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
species_halfnormal <- species


### 2. Species from hazard rate detection function ###

# Species names

# Removed from the analyses:
# Because of bad distribution of distances: CIAER, FATIN
# Because of bad Bayesian p-value (BpObs = 0.094): COGAR
# Because it does not converge ni pa atrás: LASEN


# Characteristics of each species model:
#    MICAL: 190,000 iterations + 100,000 burn-in (In HR_df/+iter folder)
#    HIRUS, PADOM, PIPIC:  250,000 iterations + 100,000 burn-in (In HR_df/++iter_burn folder)
#    COMON: 400,000 iterations + 100,000 burn-in; Scaled temperature and restricted prior in b (In HR_df folder)
#    PAMON: 450,000 iterations + 100,000 burn-in (In HR_df/sp folder)



sp_hr <- c("COMON", "HIRUS", "MICAL", "PADOM", "PAMON", "PIPIC") # species hazard rate (bad pvalues)


# 2.1. MICAL

setwd("S:/PhD/Second chapter/Data/Results/TRIM/6temp/HR_df/+iter")

sp_hr1 <- c("MICAL") # species hazard rate (bad pvalues)
# LIGHT (WITH ONLY SUMMARY)

species <- list()
at <- list()

for (xxx in 1:length(sp_hr1)){
  
  load(paste("HDS_",sp_hr1[xxx],".RData", sep = ""))
  
  # SPECIES
  at[[1]] <- paste("HDS_",sp_hr1[xxx], sep = "")
  
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
  
  sp <- d[which(d$Species == sp_hr1[xxx]), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "Observer", "Cluster"))] # Select species spAL and all years
  sp <- arrange(sp, Year, transectID) #Ordered
  sp_detec_transectID <- unique(sp$transectID)
  sp$Observer <- as.character(sp$Observer) 
  
  absent <- anti_join(d_tr_all,sp) # Transects with 0 abundance, add to sp.
  colnames(absent)[2] <- "Banda" # Format it to add the rows to sp
  absent$T_Y <- as.character(absent$T_Y)
  absent$Species <- sp_hr1[xxx]
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
species_hazardrate1 <- species

# 2.2. HIRUS, PADOM, PIPIC

setwd("S:/PhD/Second chapter/Data/Results/TRIM/6temp/HR_df/++iter_burn")

sp_hr2 <- c("HIRUS", "PADOM", "PIPIC") # species hazard rate (bad pvalues)
# LIGHT (WITH ONLY SUMMARY)

species <- list()
at <- list()

for (xxx in 1:length(sp_hr2)){
  
  load(paste("HDS_",sp_hr2[xxx],".RData", sep = ""))
  
  # SPECIES
  at[[1]] <- paste("HDS_",sp_hr2[xxx], sep = "")
  
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
  
  sp <- d[which(d$Species == sp_hr2[xxx]), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "Observer", "Cluster"))] # Select species spAL and all years
  sp <- arrange(sp, Year, transectID) #Ordered
  sp_detec_transectID <- unique(sp$transectID)
  sp$Observer <- as.character(sp$Observer) 
  
  absent <- anti_join(d_tr_all,sp) # Transects with 0 abundance, add to sp.
  colnames(absent)[2] <- "Banda" # Format it to add the rows to sp
  absent$T_Y <- as.character(absent$T_Y)
  absent$Species <- sp_hr2[xxx]
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
species_hazardrate2 <- species

# 2.3. PAMON

setwd("S:/PhD/Second chapter/Data/Results/TRIM/6temp/HR_df/sp")

sp_hr3 <- c("PAMON") 

# LIGHT (WITH ONLY SUMMARY)

species <- list()
at <- list()

for (xxx in 1:length(sp_hr3)){
  
  load(paste("HDS_",sp_hr3[xxx],".RData", sep = ""))
  
  # SPECIES
  at[[1]] <- paste("HDS_",sp_hr3[xxx], sep = "")
  
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
  
  sp <- d[which(d$Species == sp_hr3[xxx]), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "Observer", "Cluster"))] # Select species spAL and all years
  sp <- arrange(sp, Year, transectID) #Ordered
  sp_detec_transectID <- unique(sp$transectID)
  sp$Observer <- as.character(sp$Observer) 
  
  absent <- anti_join(d_tr_all,sp) # Transects with 0 abundance, add to sp.
  colnames(absent)[2] <- "Banda" # Format it to add the rows to sp
  absent$T_Y <- as.character(absent$T_Y)
  absent$Species <- sp_hr3[xxx]
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
species_hazardrate3 <- species


# 2.4. COMON

setwd("S:/PhD/Second chapter/Data/Results/TRIM/6temp/HR_df")

sp_hr4 <- c("COMON") 

# LIGHT (WITH ONLY SUMMARY)

species <- list()
at <- list()

for (xxx in 1:length(sp_hr4)){
  
  load(paste("HDS_",sp_hr4[xxx],".RData", sep = ""))
  
  # SPECIES
  at[[1]] <- paste("HDS_",sp_hr4[xxx], sep = "")
  
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
  
  sp <- d[which(d$Species == sp_hr4[xxx]), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "Observer", "Cluster"))] # Select species spAL and all years
  sp <- arrange(sp, Year, transectID) #Ordered
  sp_detec_transectID <- unique(sp$transectID)
  sp$Observer <- as.character(sp$Observer) 
  
  absent <- anti_join(d_tr_all,sp) # Transects with 0 abundance, add to sp.
  colnames(absent)[2] <- "Banda" # Format it to add the rows to sp
  absent$T_Y <- as.character(absent$T_Y)
  absent$Species <- sp_hr4[xxx]
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
species_hazardrate4 <- species

all_sp <- c(species_halfnormal, species_hazardrate1, species_hazardrate2, species_hazardrate3, species_hazardrate4)
species <- all_sp # To make it fit with all scripts afterwards

setwd("S:/PhD/Second chapter/Data/Results/TRIM/6temp/Final")
save(species, file = "spConvergence_light_FINAL.RData")

# Vector with all the species:

s_good <- c("ALRUF","BUOED","CACAR","COOEN","COPAL","GACRI","GATHE","MEAPI","MECAL","PAMAJ","SESER","STSSP","SYCAN","SYMEL","TERAX","UPEPO",
            "MICAL","HIRUS","PADOM","PIPIC","PAMON", "COMON")

s_good <- sort(s_good)
save(s_good, file = "speciesnames_FINAL.RData")


