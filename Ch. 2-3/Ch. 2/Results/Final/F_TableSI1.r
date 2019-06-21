# Model 6

##############################################################################################################
################################           CI-WALD m6              ##########################################
###########################################################################################################

library(rjags)
library(jagsUI)
library(dplyr)
library(rtrim)

s_good <- c("ALRUF","BUOED","CACAR","COOEN","COPAL","GACRI","GATHE","MEAPI","MECAL","PAMAJ","SESER","STSSP","SYCAN","SYMEL","TERAX","UPEPO",
            "MICAL","HIRUS","PADOM","PIPIC","PAMON", "COMON") # It has to be in this order because the list is in this order

##### Value for CI #####

# Load data

setwd("S:/PhD/Second chapter/Data/Results/TRIM/6temp/Final")
load("spConvergence_light_FINAL.RData")

# Create data frame to plot each method

hds <- data.frame(matrix(NA,ncol = 5,nrow = length(s_good)))
colnames(hds) <- c("method", "sp", "est", "lci", "uci")
hds$method <- "hds"
trim <- data.frame(matrix(NA, ncol = 5,nrow = length(s_good)))
colnames(trim) <- c("method", "sp", "est", "lci", "uci")
trim$method <- "trim"


for (i in 1:length(s_good)){
  #HDS
  summary <- data.frame(species[[i]][[2]]) # Here it is wrong when the length of s_good and species differ
  est_HDS <- summary[which(rownames(summary) %in% "bYear.lam"), c(1,3,7)]
  hds[i,2] <- s_good[i]
  hds[i,c(3:5)] <- est_HDS 
  #TRIM
  est_TRIM <- read.csv(paste("res_trim",s_good[i],".csv", sep = ""))
  trim[i,2] <- s_good[i]
  trim[i,c(3:5)] <- est_TRIM[ ,c(2:4)]
}

table <- bind_cols(hds,trim)
table <- table[ ,-7]
table$p_wald <- NA
#### Value for Wald TRIM ####

setwd("S:/PhD/Second chapter/Data")

d <- read.csv("DataDS_ready_ALL.csv")
colnames(d)[which(colnames(d) == "Count")] <- "Cluster" 

for (xxx in 1:length(s_good)){
  
  d_tr <- d[ ,which(colnames(d) %in% c("Species",  "T_Y", "Observer", "Temp"))]
  d_tr_all <- data.frame(T_Y = unique(d_tr$T_Y), id = NA)
  
  d_tr$Observer <- as.character(d_tr$Observer) 
  d_tr_all_obs <- left_join(d_tr_all, d_tr)
  d_tr_all_obs <- d_tr_all_obs[ ,c(1,4,5)]
  d_tr_all_obs <- d_tr_all_obs[which(!duplicated(d_tr_all_obs)), ] # Table with all sampled fields, which observer sampled it and wind and temperature
  
  sp <- d[which(d$Species == s_good[xxx]), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "Observer", "Cluster", "Temp"))] # Select species spAL and all years
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
  
  
  
  ###################################################################
  ##                       HDS ANALYSIS                           ###
  ###################################################################
  
  # ---- Information: bins, years, sites ----
  
  strip.width <- 200 				
  dist.breaks <- c(0,25,50,100,200)
  int.w <- diff(dist.breaks) # width of distance categories (v)
  midpt <- diff(dist.breaks)/2+dist.breaks[-5]
  nG <- length(dist.breaks)-1
  
  yrs <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018) # I HAVE TO CONVERT THIS FROM 0-7 (but nyrs is still 8!)
  nyrs <- length(yrs)
  
  # ---- Distance observations ----
  
  # Format
  all.sites <- unique(all_sp$transectID)
  all.sites <- sort(all.sites,descreasing = TRUE)
  max.sites <- length(all.sites)
  
  m <- matrix(NA, nrow = length(all.sites), ncol = nyrs)
  rownames(m) <- all.sites
  colnames(m) <- yrs
  
  # Add counts > 0
  count <- aggregate(Species ~ Year + transectID, FUN = length, data = sp)
  
  for (i in 1:nrow(count)){
    m[which(rownames(m) %in% count$transectID[i]), which(colnames(m) %in% count$Year[i])] <- count$Species[i]
  }
  
  # Add absences (0)
  for (i in 1:nrow(absent)){
    m[which(rownames(m) %in% absent$transectID[i]), which(colnames(m) %in% absent$Year[i])] <- absent$count[i]
  }
  
  # Only to check: Count of individuals per year
  count.year <- colSums(m,na.rm = TRUE)
  
  # Count of individuals per year corrected by cluster size
  average_clus <- mean(sp$Cluster) # TO INCLUDE IN THE MODEL
  count.year_clus <- count.year*average_clus
  
  
  # Year
  yrs2 <- c(0, 1, 2, 3, 4, 5, 6, 7, 8) # To make it as a continuous variable, otherwise it doesnt work
  year <- matrix(NA,nrow = max.sites, ncol = nyrs)
  colnames(year) <- yrs
  for (i in 1:nyrs){
    year[ ,which(colnames(year) %in% yrs[i])] <- rep(yrs2[i], max.sites)
  }
  
  # Observer 
  # Format
  obs <- matrix(NA, nrow = max.sites, ncol = nyrs)
  rownames(obs) <- all.sites
  colnames(obs) <- yrs
  
  # Add observers for fields with counts > 0
  for (i in 1:nrow(sp)){
    obs[which(rownames(obs) %in% sp$transectID[i]), which(colnames(obs) %in% sp$Year[i])] <- sp$Observer[i]
  }
  
  # Add observers for fields with absences (0)
  for (i in 1:nrow(absent)){
    obs[which(rownames(obs) %in% absent$transectID[i]), which(colnames(obs) %in% absent$Year[i])] <- absent$Observer[i]
  }
  unique(obs)
  # Temperature
  # Format
  temp <- matrix(NA, nrow = max.sites, ncol = nyrs)
  rownames(temp) <- all.sites
  colnames(temp) <- yrs
  
  # Add temper for fields with counts > 0
  for (i in 1:nrow(sp)){
    temp[which(rownames(temp) %in% sp$transectID[i]), which(colnames(temp) %in% sp$Year[i])] <- sp$Temp[i]
  }
  
  # Add temper for fields with absences (0)
  for (i in 1:nrow(absent)){
    temp[which(rownames(temp) %in% absent$transectID[i]), which(colnames(temp) %in% absent$Year[i])] <- absent$Temp[i]
  }
  
  
  # ---- Specify data in JAGS format ----
  
  # Distance class and ind
  nind <- nrow(sp)
  dclass <- sp$Banda
  
  m  # Counts per year and site
  
  # Co-variates
  
  yrs <- 1:9 
  year_number <- 0:8
  
  
  # Matrix with observers
  ob <- matrix(as.numeric(factor(obs)), nrow = max.sites, ncol = nyrs) # JAGS doesn't accept categorical variables
  unique(factor(ob))
  obs_id <- unique(factor(ob))[-1]
  ob[which(is.na(ob))] <- sample(obs_id, length(which(is.na(ob))), replace = TRUE) # No NA in covariate
  
  nobs <- length(unique(factor(ob)))
  
  # Matrix with temperature (put random values where NA)
  unique(factor(temp))
  temp_id <- unique(factor(temp))[-1]
  temp[which(is.na(temp))] <- sample(temp_id, length(which(is.na(temp))), replace = TRUE) # No NA in covariate
  
  #temp_mean <- mean(temp)
  #temp_sd <- sd(temp)
  #temp_sc <- (temp - temp_mean) / temp_sd
  
  # Index for random effects
  site <- c(1:max.sites)
  year <- c(1:nyrs)
  
  sitesYears <- NULL
  for (i in 1:nyrs){
    sitesYears <- c(sitesYears,c(1:length(all.sites)))}
  
  # Fixed index to map dclass onto site and year 
  # For the index, create a matrix m where NA are 0 (because I need the same length)
  
  m_index <- m
  m_index[which(is.na(m_index))] <- 0
  
  site.dclass <- year.dclass <- NULL
  
  for (t in 1:nyrs){ # sites has to be nested on years because dclass first indexes the sites on the same year
    for (j in 1:max.sites){
      site.dclass <- c(site.dclass, rep(j, m_index[j,t]))
      year.dclass <- c(year.dclass, rep(t, m_index[j,t]))
    } }
  
  
  ###################################################################
  ##                       TRIM ANALYSIS                          ###
  ###################################################################
  
  # ---- Subset for trim analysis ----
  sp <- all_sp[, which(colnames(all_sp) %in% c("Year", "transectID", "count"))] # Select species MECAL and all years
  colnames(sp)[which(colnames(sp) %in% "transectID")] <- "site"
  colnames(sp)[which(colnames(sp) %in% "Year")] <- "year"
  sp$year <- as.integer(sp$year)
  
  g <- aggregate(count ~ year, FUN = sum, data = sp)
  sp <- aggregate(count ~ year + site, FUN = sum, data = sp)
  
  check_observations(sp, model = 2)
  
  
  # ---- MODEL 3 ----
  m3 <- trim(count ~ site + year, data = sp, model = 3)
  i3 <- index(m3, which="both")
  
  
  #Extract the coefficients
  coef <- coefficients(m3, representation = c("trend"))
  sig_dev <- wald(m3) 
  sig <- overall(m3) # The p-value of this is the significant value for the overall trend in m3, = p value of the slope of m2 with all change points
  table$p_wald[xxx] <- sig$slope$p
  
}

table[,c(3,4,5,7,8,9)] <- round(table[,c(3,4,5,7,8,9)] , digits = 3)
table[,c(10)] <- round(table[,c(10)] , digits = 3)

setwd("S:/PhD/Second chapter/Data/Results/Paper")
write.csv(table, file = "ci_wald.csv")
