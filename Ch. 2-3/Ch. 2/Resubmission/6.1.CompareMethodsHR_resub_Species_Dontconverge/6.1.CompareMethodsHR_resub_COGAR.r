# Load packages

rm(list=ls())

library(rjags)
library(jagsUI)
library(dplyr)
library(rtrim)

# Compare methods using the HDS model 6. Same as comparemethods6 but using the HAZARD RATE detection function on the
# species that have bad bayesian p-values

###################################################################
##                       Prepare data                           ###
###################################################################

#setwd("C:/Users/ana.sanz/Documents/PhD/Second chapter/Resubmission")
setwd("D:/ANA/Data/chapter2")
d <- read.csv("DataDS_ready_ALL_revch2.csv")

colnames(d)[which(colnames(d) == "Count")] <- "Cluster" 

#setwd("C:/Users/ana.sanz/Documents/PhD/Second chapter/Resubmission")
setwd("D:/ANA/Data/chapter2")

zep <- read.csv("zepa.csv")

# Load species names

all <- c("TERAX", "BUOED", "TUMER","ALRUF","CACAR","COOEN","COPAL","GACRI","GATHE","MEAPI","MECAL","PAMAJ","SESER","STSSP","SYCAN","SYMEL","UPEPO",
         "MICAL","HIRUS","PADOM","PIPIC","PAMON", "COMON", "FATIN", "LUARB", "COGAR", "CACHL", "PYRAX", "LASEN", "CAINA", "ALARV", "CABRA") 
s_good <- c("COGAR")

# ONLY for TERAX_F and TERAX_M
#d$Species <- d$Species2
#d <- d[,-27]

# Start loop
for (xxx in 1:length(s_good)){
  # To take into account transects with abundance 0
  # 1. Select all transects IDs from all species observations
  # 2. Join the observations of MECAL (for example) with all transects so that they remain with NA if the
  # species was there but it wasnt sampled
  
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
  
  strip.width <- 500 				# strip half-width, w (in this example only one side of the line transect is surveyed)
  dist.breaks <- c(0,25,50,100,200,500)
  int.w <- diff(dist.breaks) # width of distance categories (v)
  midpt <- (int.w/2) + dist.breaks[-6]
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
  
  # Create one matrix for indexing SITE (SPA) when calculating abundance per year and spa in JAGS (works for all species)
  # Vector where SPA = numbers
  sites_df <- data.frame(all.sites)
  colnames(sites_df)[1] <- "transectID"
  zepas <- left_join(sites_df, zep)
  
  # Missing AL21 and AL4 (not digitalized, I add them manually here)
  zepas[zepas$transectID == "AL04", c(3,4)] <- c("no", "AL")
  zepas[zepas$transectID == "AL21", c(3,4)] <- c("no", "AL")
  
  zepas$Zepa <- as.character(zepas$Zepa)
  zepas$SECTOR <- as.character(zepas$SECTOR)
  
  zepas$index <- zepas$SECTOR # Add category "no zepa"
  zepas$index[which(zepas$Zepa == "no")] <- "NZ"
  allzepas <- rep(zepas$index,nyrs)
  
  
  a <- data.frame(allzepas = zepas$index)
  a$allzepas <- as.factor(a$allzepas)
  indexZepas <- model.matrix(~ allzepas-1, data = a)
  
  nspa <- length(unique(zepas$index))
  
  # Check (conteo de individuos por zepa. EN modelo se hace lo mismo pero con la abundancia)
  m_index <- m
  m_index[is.na(m_index)] <- 0
  
  pop_zepa <- matrix(NA, nrow = nspa, ncol = nyrs)
  rownames(pop_zepa) <- colnames(indexZepas)
  colnames(pop_zepa) <- yrs
  
  
  for(t in 1:nyrs){
    for(s in 1:nspa){
      pop_zepa[s,t] <- sum(m_index[,t]*indexZepas[,s])
    }}
  
  
  ####
  # ---- Compile data for JAGS model ----
  
  data1 <- list(nyears = nyrs, nsites = max.sites, nG=nG, int.w=int.w, strip.width = strip.width, midpt = midpt, db = dist.breaks,
                year.dclass = year.dclass, site.dclass = site.dclass, y = m, nind=nind, dclass=dclass,
                tempCov = temp, ob = ob, nobs = nobs, year1 = year_number, site = site, year_index = yrs,  nspa = nspa, indexSPA = indexZepas)
  
  # ---- JAGS model ----
  
  #setwd("C:/Users/ana.sanz/Documents/PhD/Second chapter/Data/Model")
  setwd("D:/ANA/Model")
  
  cat("model{
      
      # PRIORS
      
      # PRIORS FOR LAMBDA
      rho ~ dunif(-1,1) # Autorregresive parameter (serial AC)
      tau <- pow(sd, -2) # Prior for overdispersion in eps
      sd ~ dunif(0, 3)
      
      bYear.lam ~ dnorm(0,0.1)# MORE RESTRICTIVE # Prior for the trend
      
      # Random effects for lambda per site
      mu.lam.site ~ dnorm(0,0.1) # MORE RESTRICTIVE
      sig.lam.site ~ dunif(0, 10)
      tau.lam.site <- 1/(sig.lam.site*sig.lam.site)
      
      for (j in 1:nsites){
      log.lambda.site[j] ~ dnorm(mu.lam.site, tau.lam.site)
      }
      
      # Random effects for lambda per year
      
      sig.lam.year ~ dunif(0, 10) 
      tau.lam.year <- 1/(sig.lam.year*sig.lam.year)
      
      log.lambda.year[1] <- 0
      for (t in 2:nyears){
      log.lambda.year[t] ~ dnorm(0, tau.lam.year)
      }
      
      
      # PRIORS FOR SIGMA
      bTemp.sig ~ dnorm(0,0.1)# MORE RESTRICTIVE
      
      mu.sig ~ dnorm(0,0.1)# MORE RESTRICTIVE # Random effects for sigma per observer
      tau.sig ~ dgamma(0.1, 0.1) # Prior in tau.sig rather in sig.sig
      # sig.sig as a derived quantity
      
      # Random observer effect for sigma
      for (o in 1:nobs){
      sig.obs[o] ~ dnorm(mu.sig, tau.sig)
      }
      
      # Random effects for sigma per year
      
      tau.sig.year ~ dgamma(0.1, 0.1) # Prior in tau.sig.year rather in sig.sig.year
      # sig.sig.year as a derived quantity
      
      for (t in 1:nyears){
      log.sigma.year[t] ~ dnorm(0, tau.sig.year)
      }
      
      
      # PRIOR FOR BETA
      b ~ dgamma(0.1, 0.1) # MORE RESTRICTIVE PRIOR
      
      
      for(i in 1:nind){
      dclass[i] ~ dcat(fct[site.dclass[i], year.dclass[i], 1:nG]) 
      
      # Bayesian p-value for detection component (Bp.Obs)
      
      dclassnew[i] ~ dcat(fct[site.dclass[i], year.dclass[i], 1:nG]) # generate new observations
      Tobsp[i]<- pow(1- sqrt(fct[site.dclass[i], year.dclass[i],dclass[i]]),2) # Test for observed data
      Tobspnew[i]<- pow(1- sqrt(fct[site.dclass[i], year.dclass[i],dclassnew[i]]),2) # Test for new data
      }
      
      Bp.Obs <- sum(Tobspnew[1:nind]) > sum(Tobsp[1:nind])
      
      # LIKELIHOOD
      
      # FIRST YEAR
      for(j in 1:nsites){ 
      
      sigma[j,1] <- exp(sig.obs[ob[j,1]] + bTemp.sig*tempCov[j,1] + log.sigma.year[year_index[1]])
      
      # Construct cell probabilities for nG multinomial cells (distance categories) PER SITE
      
      for(k in 1:nG){ 
      
      p[j,1,k]<-1-exp(-(midpt[k]/sigma[j,1])^-b)
      pi[j,1,k] <- int.w[k] / strip.width 
      fc[j,1,k]<- p[j,1,k] * pi[j,1,k]                 ## pi=percent area of k; drops out if constant
      fct[j,1,k]<-fc[j,1,k]/pcap[j,1] 
      }
      
      pcap[j,1] <- sum(fc[j,1, 1:nG]) # Different per site and year (sum over all bins)
      
      
      y[j,1] ~ dbin(pcap[j,1], N[j,1]) 
      N[j,1] ~ dpois(lambda[j,1]) 
      
      lambda[j,1] <- exp(log.lambda.site[site[j]] + log.lambda.year[year_index[1]] + bYear.lam*year1[1] + w[j,1]) # year1 is t-1; year_index is t (to index properly the random effect)
      w[j,1] <- eps[j,1] / sqrt(1 - rho * rho)
      eps[j,1] ~ dnorm(0, tau)
      
      # Bayesian p-value on abundance component 
      
      Nnew[j,1]~dpois(lambda[j,1]) ##Create replicate abundances for year 1
      
      FT1[j,1]<-pow(sqrt(N[j,1])-sqrt(lambda[j,1]),2) ### residuals for 'observed' and new abundances in year 1
      FT1new[j,1]<-pow(sqrt(Nnew[j,1])-sqrt(lambda[j,1]),2)
      }
      
      #############
      # LATER YEARS
      for(j in 1:nsites){ 
      for (t in 2:nyears){
      
      sigma[j,t] <- exp(sig.obs[ob[j,t]] + bTemp.sig*tempCov[j,t] + log.sigma.year[year_index[t]])
      
      # Construct cell probabilities for nG multinomial cells (distance categories) PER SITE
      
      for(k in 1:nG){ 
      
      p[j,t,k]<-1-exp(-(midpt[k]/sigma[j,t])^-b)
      pi[j,t,k] <- int.w[k] / strip.width 
      fc[j,t,k]<- p[j,t,k] * pi[j,t,k]                 ## pi=percent area of k; drops out if constant
      fct[j,t,k]<-fc[j,t,k]/pcap[j,t] 
      }
      
      pcap[j,t] <- sum(fc[j,t, 1:nG]) # Different per site and year (sum over all bins)
      
      
      y[j,t] ~ dbin(pcap[j,t], N[j,t]) 
      N[j,t] ~ dpois(lambda[j,t]) 
      
      lambda[j,t] <- exp(log.lambda.site[site[j]] + log.lambda.year[year_index[t]] + bYear.lam*year1[t] + w[j,t])
      w[j,t] <- rho * w[j,t-1] + eps[j,t]
      eps[j,t] ~ dnorm(0, tau)
      
      # Bayesian p-value on abundance component (rest of years)
      
      Nnew[j,t]~dpois(lambda[j,t]) # create replicate abundances for rest of the years
      
      FT1[j,t]<-pow(sqrt(N[j,t])-sqrt(lambda[j,t]),2) # residuals for 'observed' and new abundances for the rest of the years
      FT1new[j,t]<-pow(sqrt(Nnew[j,t])-sqrt(lambda[j,t]),2)
      }
      }
      
      T1p <- sum(FT1[1:nsites,1:nyears]) #Sum of squared residuals for actual data set (RSS test)
      T1newp <- sum(FT1new[1:nsites,1:nyears]) # Sum of squared residuals for new data set (RSS test)
      
      # Bayesian p-value
      Bp.N <- T1newp > T1p
      
      # Derived parameters
      
      for(t in 1:nyears){
      popindex[t] <- sum(lambda[,t])
      }
      
      for(t in 1:nyears){
      for(s in 1:nspa){
      popindex_zepa[s,t] <- sum(lambda[,t]*indexSPA[,s])
      }}
      
      # Expected abundance per year inside model
      
      lam.tot[1] <- popindex[1] # Expected abundance in year 1
      for (i in 2:nyears){
      lam.tot[i] <- lam.tot[i-1] * # Here I add the starting population size as a baseline for the trend 
      exp(bYear.lam)}
      
      # sig.sig (because I used tau.sig as prior)
      sig.sig.year <- sqrt(1/tau.sig.year)
      sig.sig <- sqrt(1/tau.sig)
      
      
}",fill=TRUE, file = "s_sigma_beta(HRdetect)[obs(o,j,t)_covTemp(j,t)_year.random(t)]_lambda[alpha.site.random(j)_year.random(t)_beta.year(j)_w]_BayesP.txt")

  
  
  # Inits
  Nst <- m + 1
  inits <- function(){list(mu.sig = runif(1, log(30), log(50)), tau.sig = runif(1), b = runif(1),
                           mu.lam.site = runif(1), sig.lam.site = 0.2, sig.lam.year = 0.3, bYear.lam = runif(1),
                           N = Nst)} 
  
  # Params
  params <- c( "mu.sig", "sig.sig", "bTemp.sig", "sig.obs", "log.sigma.year", "b", 
               "mu.lam.site", "sig.lam.site", "sig.lam.year", "bYear.lam", "log.lambda.year", 
               "popindex", "sd", "rho", "lam.tot",'Bp.Obs', 'Bp.N', "sig.sig.year", "popindex_zepa",
               "tau.sig.year", "tau.sig"
  )
  
  # MCMC settings
  nc <- 3 ; ni <- 400000 ; nb <- 100000 ; nt <- 5
  
  # With jagsUI 
  out <- jags(data1, inits, params, "s_sigma_beta(HRdetect)[obs(o,j,t)_covTemp(j,t)_year.random(t)]_lambda[alpha.site.random(j)_year.random(t)_beta.year(j)_w]_BayesP.txt", n.chain = nc,
              n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
  summary <- out$summary
  print(out)
  
  #setwd("C:/Users/ana.sanz/Documents/PhD/Second chapter/Resubmission")
  setwd("D:/ANA/Results/chapter2/HR/Changed_params_400000")
  
  save(out, file = paste("HDS_",s_good[xxx],".RData", sep = ""))
  
  
  # ---- Results ----
  
  #setwd("C:/Users/ana.sanz/Documents/PhD/Second chapter/Resubmission")
  setwd("D:/ANA/Results/chapter2/HR/Changed_params_400000")
  
  load(paste("HDS_",s_good[xxx],".RData", sep = ""))
  
  
  summary <- as.data.frame(as.matrix(out$summary))
  
  results <- summary[which(rownames(summary) %in% c("popindex[1]", "popindex[2]", "popindex[3]", "popindex[4]", "popindex[5]", "popindex[6]", "popindex[7]", "popindex[8]", "popindex[9]",
                                                    "mu.lam.site", "sig.lam.site", "bYear.lam")), ]
  
  ## ---- POPULATION TREND PLOT ---- ##
  
  # Based on expected N
  
  yrs2 <- c(0, 1, 2, 3, 4, 5, 6, 7, 8) 
  
  # 1. Calculate predictions for both zones
  
  outall <- do.call(rbind,out$samples) 
  
  # Total population (since both follow the same trend)
  pred.exp <- matrix(NA, dim(outall)[1], length(yrs2))
  for(i in 1:dim(outall)[1]){ 
    ##calculate population, year 1
    pred.exp[i,1] <- as.vector(outall[i,"lam.tot[1]"])
    ##calculate populations, year 2-8, based on beta(Year)
    for (t in 2:length(yrs2)){
      pred.exp[i,t] <- pred.exp[i,(t-1)] * # Here I add the starting population size as a baseline for the trend 
        exp(outall[i,"bYear.lam"])
    }
  }
  
  predall.exp <- pred.exp
  lci.exp <- uci.exp <- mean.pred.exp <- 0 
  
  for(i in 1:length(yrs2)){
    lci.exp[i]  <- quantile(predall.exp[,i],probs = 0.025) 
    uci.exp[i]  <- quantile(predall.exp[,i],probs = 0.975)
    mean.pred.exp[i]  <- mean(predall.exp[,i])
  }
  
  
  # 2. Plot
  
  #setwd("S:/PhD/Second chapter/Data/Results/Plots/6temp/Final")
  setwd("D:/ANA/Results/chapter2/Plots/HR/Changed_params_400000")
  
  
  pdf(paste(s_good[xxx],"_TrimComp6.pdf", sep = ""), height = 5, width = 9)
  
  par(mfrow = c(1,2))
  
  plot(-15, xlim=c(0,8), ylim=c(0,max(uci.exp)+20), main = " ", xlab = "Year", ylab = "Abundance")
  mtext("HDS", side = 3, line = 1, cex = 1.2)
  
  
  polygon( x = c(yrs2, rev(yrs2)),
           y = c(lci.exp, rev(uci.exp)), 
           col = adjustcolor(c("grey"),alpha.f = 0.6),
           border = NA)
  points(mean.pred.exp ~ yrs2, type="l", col = "red")
  
  ##add in actual abundance estimates to check
  
  pop <- out$summary[grep("popindex", rownames(out$summary)),1]
  points(yrs2, pop[1:9], pch = 19, type = "l", col = "blue")
  points(yrs2, pop[1:9], pch = 19)
  
  # Print estimate
  est <- round(results[3,1],2)
  
  significance_est <- ifelse(results[3,10] == 0, 
                             paste(est,"*"), 
                             est)
  col_est <- ifelse(est>0, "blue", "red")
  
  text(7.5,1.5, significance_est, col = col_est)
  
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
  
  # Calculate 95% CI from se
  lci <- coef$add - 2*coef$se_add
  uci <- coef$add + 2*coef$se_add
  cont_zero <- between(0,lci,uci)
  
  # Save deviations
  #setwd("S:/PhD/Second chapter/Data/Results/TRIM/6temp/Final")
  setwd("D:/ANA/Results/chapter2/HR/Changed_params_400000")
  
  coef_dev <- coefficients(m3, representation = c("deviations"))
  write.csv(coef_dev, file = paste("coef_dev",s_good[xxx],".csv", sep = ""))
  
  
  #Plot with overall slope
  
  plot(overall(m3))
  mtext("TRIM", side = 3, line = 1, cex = 1.2)
  
  # Print estimate
  est <- round(coef$add[1], 2)
  
  significance_est_ci <- ifelse(cont_zero == FALSE, 
                                paste(est,"*"), 
                                est)
  
  significance_est_waldM3 <- ifelse(sig$slope$p < 0.05, 
                                    paste(est,"*"), 
                                    est)
  
  col_est <- ifelse(est > 0, "blue", "red")
  
  text(2017.5,1.5, significance_est_ci, col = col_est) # Significance for the ci in the right and 
  text(2011,1.5, significance_est_waldM3, col = col_est) # significance for the wald test of m3 in the left
  
  title(s_good[xxx], line = -1, cex = 2, outer = TRUE)
  
  dev.off()
  
  # Save TRIM estimate + CI
  #setwd("S:/PhD/Second chapter/Data/Results/TRIM/6temp/Final")
  setwd("D:/ANA/Results/chapter2/HR/Changed_params_400000")
  
  results_TRIM <- matrix (c(est, lci, uci, cont_zero), ncol = 4, nrow = 1)
  colnames(results_TRIM) <- c("Estimate", "LCI", "UCI", "Sig")
  write.csv(results_TRIM, file = paste("res_trim",s_good[xxx],".csv", sep = ""))
  
  print(s_good[xxx])
  
  }
