

##########################################################################################################
############################           COMPILE FINAL RESULTS            ##################################
##########################################################################################################

rm(list=ls())

setwd("C:/Users/ana.sanz/Documents/PhD/Second chapter/Resubmission")

d <- read.csv("DataDS_ready_ALL_revch2.csv")

colnames(d)[which(colnames(d) == "Count")] <- "Cluster" 

library(rtrim)
library(dplyr)



#### 1. Species from half normal detection function ####

#### 170.000 iter + 5.000 burn in ####

setwd("S:/Results/chapter2/HN")

sp_hn <- c("SYCAN", "CACHL", "LASEN")

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
species_halfnormal_170 <- species


#### 400.000 iter + 100.000 burn in ####

setwd("S:/Results/chapter2/HN")

sp_hn2 <- c("ALRUF", "PAMAJ", "ALARV")

# LIGHT (WITH ONLY SUMMARY)

species <- list()
at <- list()

for (xxx in 1:length(sp_hn2)){
  
  load(paste("HDS_",sp_hn2[xxx],".RData", sep = ""))
  
  # SPECIES
  at[[1]] <- paste("HDS_",sp_hn2[xxx], sep = "")
  
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
  
  sp <- d[which(d$Species == sp_hn2[xxx]), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "Observer", "Cluster"))] # Select species spAL and all years
  sp <- arrange(sp, Year, transectID) #Ordered
  sp_detec_transectID <- unique(sp$transectID)
  sp$Observer <- as.character(sp$Observer) 
  
  absent <- anti_join(d_tr_all,sp) # Transects with 0 abundance, add to sp.
  colnames(absent)[2] <- "Banda" # Format it to add the rows to sp
  absent$T_Y <- as.character(absent$T_Y)
  absent$Species <- sp_hn2[xxx]
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
species_halfnormal_400 <- species





#####
#### 2. Species from hazard rate detection function ####

#### 200.000 iter + 5.000 burn in ####

setwd("S:/Results/chapter2/HR")

sp_hr1 <- c("CACAR", "COPAL", "PIPIC") # species hazard rate (bad pvalues)

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

#### 400.000 iter + 100.000 burn in + Parameters changed ####

setwd("S:/Results/chapter2/HR/Changed_params_400000")

sp_hr2 <- c("COOEN", "HIRUS", "PYRAX", "CAINA") # species hazard rate (bad pvalues)

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


#####
#### 3. Species from ONLY SEEN (previous analysis) ####
#### Bad bp HR and HN (Maybe because of the new bin) ####

# 3.1.1. MICAL

setwd("C:/Users/ana.sanz/Documents/PhD/Second chapter/Data/Results/TRIM/6temp/HR_df/+iter")

sp_seen1_hr <- c("MICAL")

# LIGHT (WITH ONLY SUMMARY)

species <- list()
at <- list()

for (xxx in 1:length(sp_seen1)){
  
  load(paste("HDS_",sp_seen1[xxx],".RData", sep = ""))
  
  # SPECIES
  at[[1]] <- paste("HDS_",sp_seen1[xxx], sep = "")
  
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
  
  sp <- d[which(d$Species == sp_seen1[xxx]), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "Observer", "Cluster"))] # Select species spAL and all years
  sp <- arrange(sp, Year, transectID) #Ordered
  sp_detec_transectID <- unique(sp$transectID)
  sp$Observer <- as.character(sp$Observer) 
  
  absent <- anti_join(d_tr_all,sp) # Transects with 0 abundance, add to sp.
  colnames(absent)[2] <- "Banda" # Format it to add the rows to sp
  absent$T_Y <- as.character(absent$T_Y)
  absent$Species <- sp_seen1[xxx]
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
species_seen1_hr <- species

# 3.1.2. GACRI & STSSP

setwd("C:/Users/ana.sanz/Documents/PhD/Second chapter/Data/Results/TRIM/6temp/Final")

sp_seen2_hn <- c("GACRI", "STSSP")

species <- list()
at <- list()

for (xxx in 1:length(sp_seen2)){
  
  load(paste("HDS_",sp_seen2[xxx],".RData", sep = ""))
  
  # SPECIES
  at[[1]] <- paste("HDS_",sp_seen2[xxx], sep = "")
  
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
  
  sp <- d[which(d$Species == sp_seen2[xxx]), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "Observer", "Cluster"))] # Select species spAL and all years
  sp <- arrange(sp, Year, transectID) #Ordered
  sp_detec_transectID <- unique(sp$transectID)
  sp$Observer <- as.character(sp$Observer) 
  
  absent <- anti_join(d_tr_all,sp) # Transects with 0 abundance, add to sp.
  colnames(absent)[2] <- "Banda" # Format it to add the rows to sp
  absent$T_Y <- as.character(absent$T_Y)
  absent$Species <- sp_seen2[xxx]
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

species_seen2_hn <- species


#### Bad convergence ####
# 3.2.1. Converge very bad (For sure I will include them as seen, detection curves are bad with heard+seen)

setwd("C:/Users/ana.sanz/Documents/PhD/Second chapter/Data/Results/TRIM/6temp/Final")

sp_seen3_hn <- c("SYMEL", "SESER")

# LIGHT (WITH ONLY SUMMARY)

species <- list()
at <- list()

for (xxx in 1:length(sp_seen3_hn)){
  
  load(paste("HDS_",sp_seen3_hn[xxx],".RData", sep = ""))
  
  # SPECIES
  at[[1]] <- paste("HDS_",sp_seen3_hn[xxx], sep = "")
  
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
  
  sp <- d[which(d$Species == sp_seen3_hn[xxx]), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "Observer", "Cluster"))] # Select species spAL and all years
  sp <- arrange(sp, Year, transectID) #Ordered
  sp_detec_transectID <- unique(sp$transectID)
  sp$Observer <- as.character(sp$Observer) 
  
  absent <- anti_join(d_tr_all,sp) # Transects with 0 abundance, add to sp.
  colnames(absent)[2] <- "Banda" # Format it to add the rows to sp
  absent$T_Y <- as.character(absent$T_Y)
  absent$Species <- sp_seen3_hn[xxx]
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
species_seen3_hn <- species


# 3.2.2. Converge bad (but I can still try, ASK RAHEL)

sp_seen4_hn <- c("BUOED", "MECAL", "PADOM", "UPEPO") # FATIN also(ask), but I didnt include it before

# LIGHT (WITH ONLY SUMMARY)

species <- list()
at <- list()

for (xxx in 1:length(sp_seen4_hn)){
  
  load(paste("HDS_",sp_seen4_hn[xxx],".RData", sep = ""))
  
  # SPECIES
  at[[1]] <- paste("HDS_",sp_seen4_hn[xxx], sep = "")
  
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
  
  sp <- d[which(d$Species == sp_seen4_hn[xxx]), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "Observer", "Cluster"))] # Select species spAL and all years
  sp <- arrange(sp, Year, transectID) #Ordered
  sp_detec_transectID <- unique(sp$transectID)
  sp$Observer <- as.character(sp$Observer) 
  
  absent <- anti_join(d_tr_all,sp) # Transects with 0 abundance, add to sp.
  colnames(absent)[2] <- "Banda" # Format it to add the rows to sp
  absent$T_Y <- as.character(absent$T_Y)
  absent$Species <- sp_seen4_hn[xxx]
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
species_seen4_hn <- species


#### COMPILE ####

all_sp <- c(species_halfnormal_170, species_halfnormal_400, 
            species_hazardrate1, species_hazardrate2,
            species_seen1_hr, species_seen2_hn, species_seen3_hn, species_seen4_hn)

species <- all_sp # To make it fit with all scripts afterwards

setwd("C:/Users/ana.sanz/Documents/PhD/Second chapter/Resubmission/Results")
save(species, file = "spConvergence_light_resub_FINAL.RData")

# Vector with all the species:

#s_good <- c("ALRUF","BUOED","CACAR","COOEN","COPAL","GACRI","GATHE","MEAPI","MECAL","PAMAJ","SESER","STSSP","SYCAN","SYMEL","TERAX","UPEPO",
#            "MICAL","HIRUS","PADOM","PIPIC","PAMON", "COMON")
s_good <- c("SYCAN", "CACHL", "LASEN", "ALRUF", "PAMAJ", "ALARV","CACAR", "COPAL", "PIPIC",
            "COOEN", "HIRUS", "PYRAX", "CAINA", "MICAL", "GACRI", "STSSP")
            
s_good <- sort(s_good)
save(s_good, file = "speciesnames_resub_FINAL.RData")