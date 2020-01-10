

########################################################
####                  Check convergence           #####
########################################################


rm(list=ls())

setwd("C:/Users/ana.sanz/Documents/PhD/Second chapter/Resubmission")

d <- read.csv("DataDS_ready_ALL_revch2.csv")

colnames(d)[which(colnames(d) == "Count")] <- "Cluster" 

library(rtrim)
library(dplyr)

##################### HN ##############################
# 170000 iter; 10000 burnin

# Load species names

all <- c("TERAX", "BUOED", "TUMER","ALRUF","CACAR","COOEN","COPAL","GACRI","GATHE","MEAPI","MECAL","PAMAJ","SESER","STSSP","SYCAN","SYMEL","UPEPO",
         "MICAL","HIRUS","PADOM","PIPIC","PAMON", "COMON", "FATIN", "LUARB", "COGAR", "CACHL", "PYRAX", "LASEN", "CAINA", "ALARV", "CABRA") 
s_good <- all

# LIGHT (WITH ONLY SUMMARY)

setwd("S:/Results/chapter2/HN")

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

save(species, file = "spConvergence_light_HN_resub.RData")

# Check

bp.obs <- list()
bp.n <- list()
for(i in 1:length(s_good)){
bp.obs[i] <- paste(species[[i]][[1]], "=", species[[i]][[2]][which(rownames(species[[i]][[2]]) %in% c("Bp.Obs")), 1])
bp.n[i] <- species[[i]][[2]][which(rownames(species[[i]][[2]]) %in% c("Bp.N")), 1]
}

conv <- list()

species[[]]
for(i in 1:length(s_good)){
}

##################### HR ##############################
# 200000 iter; 10000 burnin

# Load species HR

hr <- c("TERAX", "BUOED", "TUMER","CACAR","COOEN","COPAL","GACRI","GATHE","MEAPI","MECAL","SESER","STSSP","SYMEL","UPEPO",
        "MICAL","HIRUS","PADOM","PIPIC","PAMON", "COMON", "FATIN", "LUARB", "COGAR", "PYRAX", "CAINA")

s_good <- hr

s_good <- c("TERAX_F", "TERAX_M", "TUMER", "GATHE", "MEAPI")

# LIGHT (WITH ONLY SUMMARY)

setwd("S:/Results/chapter2/HR")

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

setwd("C:/Users/ana.sanz/Documents/PhD/Second chapter/Resubmission")

save(species, file = "spConvergence_light_HR_resub.RData")

#####

rm(list=ls())

setwd("C:/Users/ana.sanz/Documents/PhD/Second chapter/Resubmission")

d <- read.csv("DataDS_ready_ALL_revch2.csv")

colnames(d)[which(colnames(d) == "Count")] <- "Cluster" 


setwd("S:/Results/chapter2/HR")
load("spConvergence_light_HR_resub.RData")

hr <- c("TERAX", "BUOED", "TUMER","CACAR","COOEN","COPAL","GACRI","GATHE","MEAPI","MECAL","SESER","STSSP","SYMEL","UPEPO",
        "MICAL","HIRUS","PADOM","PIPIC","PAMON", "COMON", "FATIN", "LUARB", "COGAR", "PYRAX", "CAINA")
s_good <- hr
# Check

# Bayesian p-values

bp.obs <- list()
bp.n <- list()
for(i in 1:length(s_good)){
  bp.obs[i] <- paste(species[[i]][[1]], "=", species[[i]][[2]][which(rownames(species[[i]][[2]]) %in% c("Bp.Obs")), 1])
  bp.n[i] <- species[[i]][[2]][which(rownames(species[[i]][[2]]) %in% c("Bp.N")), 1]
}

# Convergence

# Structural parameters
params <- c( "mu.sig", "sig.sig", "bTemp.sig", "b", 
             "mu.lam.site", "sig.lam.site", "sig.lam.year", "bYear.lam", 
             "sd", "rho",'Bp.Obs', 'Bp.N', "sig.sig.year")

conv <- list()

for(i in 1:length(s_good)){
  sp <- s_good[i]
  c <- species[[i]][[3]]
  c <- c[which(rownames(c) %in% params), ]
  conv_sp <- list(sp, c)
  conv[[i]] <- conv_sp
}


hr
conv[[23]]

# Load results last submission with bad convergence to see comments from Rahel
setwd("C:/Users/ana.sanz/Documents/PhD/Second chapter/Data/Results/TRIM/6temp")
spec_ra <- load("spConvergence_light_badconvergences.RData")
species[[4]]


#### Check TERAX_F, TERAX_M, COGAR, MEAPI ####

setwd("C:/Users/ana.sanz/Documents/PhD/Second chapter/Resubmission")

d <- read.csv("DataDS_ready_ALL_revch2.csv")

colnames(d)[which(colnames(d) == "Count")] <- "Cluster" 

# ONLY for TERAX_F and TERAX_M
d$Species <- d$Species2
d <- d[,-27]


s_good <- c("TERAX_F", "TERAX_M", "TUMER", "GATHE", "MEAPI")
s_good <- c("COGAR")
s_good <- c("MEAPI")
s_good <- c("PAMON")

# LIGHT (WITH ONLY SUMMARY)

setwd("S:/Results/chapter2/HR/Changed_params_400000")

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

# Bayesian p-values

bp.obs <- list()
bp.n <- list()
for(i in 1:length(s_good)){
  bp.obs[i] <- paste(species[[i]][[1]], "=", species[[i]][[2]][which(rownames(species[[i]][[2]]) %in% c("Bp.Obs")), 1])
  bp.n[i] <- species[[i]][[2]][which(rownames(species[[i]][[2]]) %in% c("Bp.N")), 1]
}

# Convergence

# Structural parameters
params <- c( "mu.sig", "sig.sig", "bTemp.sig", "b", 
             "mu.lam.site", "sig.lam.site", "sig.lam.year", "bYear.lam", 
             "sd", "rho",'Bp.Obs', 'Bp.N', "sig.sig.year")

conv <- list()

for(i in 1:length(s_good)){
  sp <- s_good[i]
  c <- species[[i]][[3]]
  c <- c[which(rownames(c) %in% params), ]
  conv_sp <- list(sp, c)
  conv[[i]] <- conv_sp
}

s_good
conv[[2]]
