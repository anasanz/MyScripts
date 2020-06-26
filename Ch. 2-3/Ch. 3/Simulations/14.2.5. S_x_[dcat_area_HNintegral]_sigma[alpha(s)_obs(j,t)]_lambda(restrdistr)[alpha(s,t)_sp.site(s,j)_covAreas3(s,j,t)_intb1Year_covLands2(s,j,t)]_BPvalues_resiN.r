rm(list=ls())

library(rjags)
library(jagsUI)
library(plyr)

set.seed(2013)

# Model 14.1 (2 random coefficient crop div.&fsize and sp-site), and add to this an extra random coefficient (a3)
# Add df with 0 and 1 to define distribution of species

# ---- Data simulation ----

# 30 species
# 5 years (unbalanced number of transects per year) 
# Add calculation of bp values based on the total number of observations (resiN)
# Add interaction between Year*A1

# Observation model calculated with Half Normal detection function
# Sigma site-year specific
### Random sp intercept 
### Random effect in observer (site-year)

# Lambda site-year specific
### Random sp-year intercept (include different baseline abundance per species and also per year)
### 2 areas variables (area 1 sp-specific)

# Detection function: HN
g <- function(x, sig) exp(-x^2/(2*sig^2))

# Number of transects per year (unbalanced)
nSites <- seq(74,106, by = 8)				# number of line transect surveys (DIFFERENT BY YEAR)
max.sites <- max(nSites)            # Maximun number of sites is the last year
total.sites <- sum(nSites)  

strip.width <- 500 				# strip half-width, w (in this example only one side of the line transect is surveyed)
dist.breaks <- c(0,25,50,100,200,500)
int.w <- diff(dist.breaks) # width of distance categories (v)
midpt <- (int.w/2) + dist.breaks[-6]

nG <- length(dist.breaks)-1	

# Year effect 
yrs <- 1:5 # eight years
nyrs <- length(yrs)

# Number of species
nSpecies <- 30


#################################
# ---- Detection component ----

# RANDOM INTERCEPT PER SPECIES

mu.sig.sp <- log(50)					# Mean of species-level random effect on intercept of sigma
sig.sig.sp <- 0.25				# SD of species-level random effect on intercept of sigma
s.alpha <- rnorm(nSpecies, mu.sig.sp, sig.sig.sp)

# Look at distribution of sigma intercepts (to see if I chose reasonable)
hist(exp(rnorm(1000, mu.sig.sp, sig.sig.sp)))

# RANDOM EFFECT IN OBSERVER
obs <- 1:9
nobs <- length(obs)
sig.sig.obs <- 0.25

# Observer effect in sigma
sig.obs <- rnorm(length(obs), 0, sig.sig.obs) # Mean is 0 because is adding noise around the mean

# Observer covariate
ob.id <- matrix(sample(1:9, max.sites*nyrs, replace = TRUE), nrow = max.sites, ncol = nyrs) # Matix with IDs
ob <- matrix(sig.obs[ob.id],  nrow = max.sites, ncol = nyrs) # Matrix with intercept for simulating data


#SIGMA: sigma[j,t,s]
sigma <- exp(array(rep(s.alpha, each = max.sites*nyrs), c(max.sites, nyrs, nSpecies)) 
             + replicate(nSpecies,ob))


# ----  Abundance component: random effect accross sites, zone covariate and 2 area covariates

# RANDOM INTERCEPT PER SPECIES-YEAR
# Mean abundance and sd per species and year
mu.lam.alpha.spyear <- log(1.5)
sig.lam.alpha.spyear <- 0.5
# Intercept
lam.alpha.spyear <- rnorm(nyrs*nSpecies, mu.lam.alpha.spyear, sig.lam.alpha.spyear) 

lam.alpha.spyear_data <- array(rep(lam.alpha.spyear, each = max.sites), c(max.sites, nyrs, nSpecies))

# RANDOM EFFECT IN SPECIES-SITE (Independent of year)
sig.lam.spsite <- 0.3
lam.spsite <- rnorm(nSpecies*max.sites, 0, sig.lam.spsite) 
ar <- array(rep(lam.spsite, each = nyrs), c(nyrs, max.sites, nSpecies)) # I need to make it in wide format and transpose it to long
ar1 <- list() 
for (i in 1:nSpecies){
  df <- as.data.frame(ar[,,i])
  df <- t(df)
  df <- as.matrix(df)
  ar1[[i]] <- df
}

lam.spsite_data <- array(as.numeric(unlist(ar1)), c(max.sites, nyrs, nSpecies))

#AREA COVARIATE (SITE AND YEAR)
#Coefficients

mu.a1 <- 0.2
sig.a1 <- 0.8
b.a1 <- rnorm(nSpecies, mu.a1, sig.a1)

mu.a1.int <- 0.5
sig.a1.int <- 0.2
b.a1.int <- rnorm(nSpecies, mu.a1.int, sig.a1.int)

mu.a2 <- -0.5
sig.a2 <- 0.3
b.a2 <- rnorm(nSpecies, mu.a2, sig.a2)

mu.a3 <- 0
sig.a3 <- 0.5
b.a3 <- rnorm(nSpecies, mu.a3, sig.a3)

#Covariates
a1 <- abs(rnorm(max.sites*nyrs, 10, 5)) # Although it makes sense to make them positive, it wouldnt matter (you put them on the exp)
a2 <- abs(rnorm(max.sites*nyrs, 5, 2.5))
a3 <- abs(rnorm(max.sites*nyrs, 2, 1))

#SCALED
area1_mean <- mean(a1)
area1_sd <- sd(a1)
area1_sc <- (a1 - area1_mean) / area1_sd

area2_mean <- mean(a2)
area2_sd <- sd(a2)
area2_sc <- (a2 - area2_mean) / area2_sd

area3_mean <- mean(a3)
area3_sd <- sd(a3)
area3_sc <- (a3 - area3_mean) / area3_sd


# LANDSCAPE COVARIATES

# Coefficients
mu.cd <- 1
sig.cd <- 0.2
bCropdiv <- rnorm(nSpecies, mu.cd, sig.cd)

mu.fs <- -0.8
sig.fs <- 0.5
bFieldsize <- rnorm(nSpecies, mu.fs, sig.fs)

# Covariates
crop_diversity <- round(abs(rnorm(max.sites*nyrs, 7, 3)),0)
field_size <- abs(rnorm(max.sites*nyrs, 2, 0.5))


#SCALED
cropdiv_mean <- mean(crop_diversity)
cropdiv_sd <- sd(crop_diversity)
cropdiv_sc <- (crop_diversity - cropdiv_mean) / cropdiv_sd

fieldsize_mean <- mean(field_size)
fieldsize_sd <- sd(field_size)
fieldsize_sc <- (field_size - fieldsize_mean) / fieldsize_sd

# Year co-variate for interaction

mu.y <- 0.3
sig.y <- 0.5
b.y <- rnorm(nSpecies, mu.y, sig.y)

years <- NULL 
for (i in 1:nyrs){
  years <- c(years,rep(yrs[i],max.sites))
}


lam <- exp(lam.alpha.spyear_data + 
             lam.spsite_data +
             array(rep(b.a1,each=max.sites*nyrs)*area1_sc, c(max.sites, nyrs, nSpecies)) +
             array(rep(b.y,each=max.sites*nyrs)*years, c(max.sites, nyrs, nSpecies)) +
             array(rep(b.a1.int,each=max.sites*nyrs)*area1_sc*years, c(max.sites, nyrs, nSpecies)) +
             array(rep(b.a2,each=max.sites*nyrs)*area2_sc, c(max.sites, nyrs, nSpecies)) +
             array(rep(b.a3,each=max.sites*nyrs)*area3_sc, c(max.sites, nyrs, nSpecies)) +
             array(rep(bCropdiv,each=max.sites*nyrs)*cropdiv_sc, c(max.sites, nyrs, nSpecies)) +
             array(rep(bFieldsize,each=max.sites*nyrs)*fieldsize_sc, c(max.sites, nyrs, nSpecies))) 


# Restrict the presence of some species to certain transects
# Vector of 1 or 0 indicating the transects
restrict_zone <- array(1, c(max.sites, nyrs, nSpecies)) # Only 1
restrict_zone[1:50,,2] <- 0 # The species 2, 7 and 15 are not present in transects 1:50
restrict_zone[1:50,,7] <- 0
restrict_zone[1:50,,15] <- 0

# Create lambda.eff which is the effective lambda (excluding species out of their range)
lam.eff <- lam*restrict_zone

# Abundance per site year (different abundance per species): N.sysp[j,t,s] 
N <- list()
N.sysp <- list() 

for (s in 1:nSpecies){
  for (t in 1:nyrs){
    N[[t]] <- rpois(nSites[t],lam.eff[1:nSites[t], t, s]) 
  } 
  NLong <- ldply(N,cbind) # 1 long vector with all abundances per site and year
  N3 <- ldply(N,rbind)
  N.sitesYears <- t(N3) # N per site and year stored in a matrix with columns
  N.sysp[[s]] <- N.sitesYears
}

N.sysp <- array(as.numeric(unlist(N.sysp)), c(max.sites,nyrs,nSpecies))

# Total number of individuals in all sampled transects per year
N.tot <- lapply(N,sum) # I only need this list for now, is the same abundance


# ---- Simulate continuous distance data ----


# Create list to store the counts:
yList <- list()

for (i in 1:nyrs){
  yList[[i]] <- array(0, c(nSites[i], nG, nSpecies)) }


# EXPLANATION DIMENSIONS: yList[[t]][[j,k,s]]
yList[[1]][,,1] # This is the counts for the year 1 for species 1
yList[[2]][,3,1] # This is the counts for the year 1 for species 1 in bin 3
yList[[1]][,,1]
N.sysp[,,1] # This is the real number of individuals per stite and year of species 1
sigma[,,1] # And this is sigma per site and year for species 1

for (s in 1:nSpecies){
  for (t in 1:nyrs){
    for(j in 1:max.sites) {
      if(N.sysp[j,t,s] == 0 | is.na(N.sysp[j,t,s]))
        next
      # Distance from observer to the individual
      d <- runif(N.sysp[j,t,s], 0, strip.width) 		# Uniform distribution of animals
      # Simulates one distance for each individual in the site (N[j])
      p <- g(x=d, sig=sigma[j,t,s])   		# Detection probability. Sigma is site-time specific
      seen <- rbinom(N.sysp[j,t,s], 1, p)
      if(all(seen == 0))
        next
      d1 <- d[seen==1] 				# The distance data for seen individuals
      counts <- table(cut(d1, dist.breaks, include.lowest=TRUE))
      yList[[t]][j,,s] <- counts 				# The number of detections in each distance interval per year and species
    }}}

y.sum.sysp <- list()
y.sum.sites <- list()

for (t in 1:nyrs){
  
  for(s in 1:nSpecies){
    y.sum.sysp[[s]] <- rowSums(yList[[t]][,,s]) # List with counts per site in a given year t. Each element is one species
  }
  y.sum.sites[[t]] <- y.sum.sysp # Stored by years (y.sum.sites[[t]][[s]]): 8 elements with 15 subelements each
}

# Check things in loop because I dont want to do a 3diiimensional mistake
rowSums(yList[[2]][,,2])
h <- yList[[2]]
class(h)
rowSums(h[,,1])

# Arrange it by species (so that each element of the list is a species)
y.sum.sites.sp <- list()
for (s in 1:nSpecies){
  y.sum.sites.sp[[s]] <- sapply(y.sum.sites, function(x) x[s]) }

# From here I do the same than in script 8.1 but applied to the list of lists
y.sum <- list()

for (s in 1:nSpecies){
  store <- unlist(y.sum.sites.sp[s], recursive = F) # Convert it into single list to use ldply later
  y.sum.sites2 <- ldply(store,rbind) # Put all together (in rows) 
  y.sum[[s]] <- t(y.sum.sites2)} 

# y.sum is a list of species counts.
# Contains y per site and year stored in a matrix with columns.


#############################################

# ---- Convert data to JAGS format ----

nind.sp <- list()
for (s in 1:nSpecies){
  nind.year.sp <- lapply(y.sum.sites.sp[[s]],sum)
  nind.sp[[s]] <- sum(unlist(nind.year.sp, use.names = F)) # Just to know, but jags only wants the sum
}
nind <- do.call(sum, nind.sp)


# Get one long matrix with counts and sites per species (columns)
yLong.sp <- matrix(NA, nrow = total.sites, ncol = nSpecies)
for (s in 1:nSpecies){
  yLong.na <- unlist(as.data.frame(y.sum[[s]]), use.names = F) # With NA included (useful if I ever make a model estimating abundance in sites with no information)
  yLong.sp[,s] <- yLong.na[complete.cases(yLong.na)]
}

# Get matrix in format sites*species for restrict distribution zone

y.sum.prov <- array(unlist(y.sum), c(max.sites, nyrs, nSpecies)) # ssame but array
restrict_zone[is.na(y.sum.prov)] <- NA # First include NA where restrict_zone to make it match

restrict.sp <- matrix(NA, nrow = total.sites, ncol = nSpecies)
for (s in 1:nSpecies){
  restrict.sp.na <- unlist(as.data.frame(restrict_zone[,,s]), use.names = F) 
  restrict.sp[,s] <- restrict.sp.na[complete.cases(restrict.sp.na)] 
}


# All this index and variables are site-speficic (not species specific) so they stay like this
sitesYears <- NULL # I did that loop but the normal version actually works, since is an index per site-year
for (i in 1:nyrs){
  sitesYears <- c(sitesYears,c(1:nSites[i]))
}


# Create one long vector with covariate values
a1.m <- matrix(area1_sc, nrow = max.sites, ncol = nyrs, byrow = F) # I need to make it from the same matrix
a2.m <- matrix(area2_sc, nrow = max.sites, ncol = nyrs, byrow = F)
a3.m <- matrix(area3_sc, nrow = max.sites, ncol = nyrs, byrow = F)
cropdiv.m <- matrix(cropdiv_sc, nrow = max.sites, ncol = nyrs, byrow = F)
fieldsize.m <- matrix(fieldsize_sc, nrow = max.sites, ncol = nyrs, byrow = F)


area1 <- NULL
for (i in 1:nyrs){
  area1 <- c(area1,a1.m[1:nSites[i],i])
}

area2 <- NULL
for (i in 1:nyrs){
  area2 <- c(area2,a2.m[1:nSites[i],i])
}

area3 <- NULL
for (i in 1:nyrs){
  area3 <- c(area3,a3.m[1:nSites[i],i])
}

cdiv <- NULL
for (i in 1:nyrs){
  cdiv <- c(cdiv, cropdiv.m[1:nSites[i],i])
}

fsiz <- NULL
for (i in 1:nyrs){
  fsiz <- c(fsiz, fieldsize.m[1:nSites[i],i])
}

ob <- NULL
for (i in 1:nyrs){
  ob <- c(ob,ob.id[1:nSites[i], i])
}

# Get one long vector with years, distance category and site
site <- dclass <- year <- NULL

for (s in 1:nSpecies){
  for (t in 1:nyrs){
    for(j in 1:max.sites){
      if (y.sum[[s]][j,t] == 0 | is.na(y.sum[[s]][j,t])) 
        next
      site <- c(site, rep(j, y.sum[[s]][j,t])) # site index: repeat the site as many times as counts in that site (for multi model??)
      
      # vector of sites through years (disregarding distance class)
      year <- c(year, rep(t, y.sum[[s]][j,t]))
      
      for (k in 1:nG){
        if (yList[[t]][j,k,s] == 0) # Refers for the ditance classes to the list with years and bins
          next 
        dclass <- c(dclass, rep(k, yList[[t]][j,k,s]))}	# Distance category index
    }}}

# Get one long vector for each site-year combination of each dclass observation
# (so, at which j, or siteyear is every observation or dclass corresponding?)

n.allSiteYear <- sum(nSites)
siteYear.dclass <- NULL

###RS: Fixed index to map dclass onto site-year combinations (all species together)

for (s in 1:nSpecies){
  for (i in 1:n.allSiteYear){
    siteYear.dclass <- c(siteYear.dclass,rep(i, yLong.sp[i,s]))}
}

# Fixed index to map dclass in species (so that it matches with the dimensions (s,j,K))

sp.dclass <- NULL
for (s in 1:nSpecies){
  for (i in 1:n.allSiteYear){
    sp.dclass <- c(sp.dclass,rep(s, yLong.sp[i,s]))}
}

# Create one matrix for indexing year when calculating abundance per year in JAGS (works for all species)

allyears <- NULL 
for (i in 1:nyrs){
  allyears <- c(allyears,rep(yrs[i],nSites[i]))
}
m <- data.frame(allyears = allyears)
m$allyears <- as.factor(m$allyears)
indexYears <- model.matrix(~ allyears-1, data = m)


# ---- Compile data for JAGS model ----

data <- list(nyears = nyrs, max.sites = max.sites, nG=nG, siteYear.dclass = siteYear.dclass, int.w=int.w, strip.width = strip.width,
              y = yLong.sp, n.allSiteYear = n.allSiteYear, nind=nind, dclass=dclass, sitesYears = sitesYears, indexYears = indexYears, allyears = allyears,
              area1 = area1, area2 = area2, area3 = area3, cdiv = cdiv, fsiz = fsiz, ob = ob, nobs = nobs, db = dist.breaks,
              nSpecies = nSpecies, sp.dclass = sp.dclass, nyrs = nyrs, restrict.sp = restrict.sp)

# ---- JAGS model ----

setwd("D:/PhD/Third chapter/Data/model/14.2.5_SIM_2")
cat("model{
    
    # PRIORS
    
    # SPECIES SPECIFIC PARAMETERS (random effects)
    
    for (s in 1:nSpecies){              # Random intercept for sigma (dif detection per species)
    asig[s] ~ dnorm(mu_s, tau_s)
    b.a1[s] ~ dnorm(mu_a1, tau_a1)
    b.y[s] ~ dnorm(mu_y, tau_y)
    b.a1.int[s] ~ dnorm(mu_a1_int, tau_a1_int)
    b.a2[s] ~ dnorm(mu_a2, tau_a2)
    b.a3[s] ~ dnorm(mu_a3, tau_a3)
    bCropdiv[s] ~ dnorm(mu_cd, tau_cd)
    bFieldsize[s] ~ dnorm(mu_fs, tau_fs)
    }
    
    
    for(s in 1:nSpecies){              # Random intercept for lambda (dif abundance per species and year)
    for(t in 1:nyrs){
    alam[s,t] ~ dnorm(mu_l,tau_l)}}
    
    for (s in 1:nSpecies){             # Random effect for lambda (dif abundance per species and site)
    for (i in 1:max.sites){
    spsite[s,i] ~ dnorm(0, tau_spsite) }}  
    
    # Hyperparameters of species level random effects
    
    mu_s ~ dnorm(0,0.01) # Hyperparameters for sigma intercept
    tau_s <- 1/(sig_s*sig_s)
    sig_s ~ dunif(0,500)
    
    mu_l ~ dnorm(0,0.01) # Hyperparameters for lambda intercept
    tau_l <- 1/(sig_l*sig_l)
    sig_l ~ dunif(0,500)
    
    mu_a1 ~ dnorm(0,0.01) # Hyperparameters for beta coefficient area1
    tau_a1 <- 1/(sig_a1*sig_a1)
    sig_a1 ~ dunif(0,500)

    mu_y ~ dnorm(0,0.01) # Hyperparameters for beta coefficient year
    tau_y <- 1/(sig_y*sig_y)
    sig_y ~ dunif(0,500)

    mu_a1_int ~ dnorm(0,0.01) # Hyperparameters for beta coefficient year*area1
    tau_a1_int <- 1/(sig_a1_int*sig_a1_int)
    sig_a1_int ~ dunif(0,500)
    
    mu_a2 ~ dnorm(0,0.01) # Hyperparameters for beta coefficient area2
    tau_a2 <- 1/(sig_a2*sig_a2)
    sig_a2 ~ dunif(0,500)
    
    mu_a3 ~ dnorm(0,0.01) # Hyperparameters for beta coefficient area3
    tau_a3 <- 1/(sig_a3*sig_a3)
    sig_a3 ~ dunif(0,500)
    
    mu_cd ~ dnorm(0,0.01) # Hyperparameters for beta coefficient crop diversity
    tau_cd <- 1/(sig_cd*sig_cd)
    sig_cd ~ dunif(0,500)
    
    mu_fs ~ dnorm(0,0.01) # Hyperparameters for beta coefficient field size
    tau_fs <- 1/(sig_fs*sig_fs)
    sig_fs ~ dunif(0,500)
    
    tau_spsite <- 1/(sig_spsite*sig_spsite) # Hyperparameter for site random effect in lambda
    sig_spsite ~ dunif(0,500)
    
    
    # PRIORS FOR SIGMA
    
    sig.sig.ob ~ dunif(0, 10) # Random effects for sigma per observer
    tau.sig.ob <- 1/(sig.sig.ob*sig.sig.ob)
    
    #Random observer effect for sigma
    
    for (o in 1:nobs){
    sig.obs[o] ~ dnorm(0, tau.sig.ob)
    }
    
    for(i in 1:nind){
    dclass[i] ~ dcat(fct[sp.dclass[i],siteYear.dclass[i], 1:nG])
    }
    
    for (s in 1:nSpecies){
    
    for(j in 1:n.allSiteYear){ 
    
    sigma[s,j] <- exp(asig[s] + sig.obs[ob[j]])
    
    f.0[s,j] <- 2 * dnorm(0,0, 1/sigma[s,j]^2)
    
    # Construct cell probabilities for nG multinomial cells (distance categories) PER SITE
    
    for(k in 1:nG){ 
    
    up[s,j,k]<-pnorm(db[k+1], 0, 1/sigma[s,j]^2) ##db are distance bin limits
    low[s,j,k]<-pnorm(db[k], 0, 1/sigma[s,j]^2) 
    p[s,j,k]<- 2 * (up[s,j,k] - low[s,j,k])
    pi[s,j,k] <- int.w[k] / strip.width 
    f[s,j,k]<- p[s,j,k]/f.0[s,j]/int.w[k]                   ## detection prob. in distance category k                      
    fc[s,j,k]<- f[s,j,k] * pi[s,j,k]                 ## pi=percent area of k; drops out if constant
    fct[s,j,k]<-fc[s,j,k]/pcap[s,j] 
    }
    
    pcap[s,j] <- sum(fc[s,j,1:nG]) # Different per site and year (sum over all bins)
    
    y[j,s] ~ dbin(pcap[s,j], N[j,s]) 
    N[j,s] ~ dpois(lambda.eff[j,s]) 
    
    lambda[j,s] <- exp(alam[s,allyears[j]] + spsite[s,sitesYears[j]] 
    + b.a1[s]*area1[j] + b.y[s]*allyears[j] + b.a1.int[s]*area1[j]*allyears[j] + b.a2[s]*area2[j] + b.a3[s]*area3[j] + bCropdiv[s]*cdiv[j] + bFieldsize[s]*fsiz[j] ) 
    
    lambda.eff[j,s] <- lambda[j,s] * restrict.sp[j,s]
    
    # FOR BP.OBS
    # Create a new Y (detections)
    y.new[j,s]~ dbin(pcap[s,j], N[j,s])
    
    # Calculate residuals residuals: look at the total number of individuals detected instead 
    Tobsp[j,s] <- pow(  sqrt(y[j,s]) - sqrt(pcap[s,j] * N[j,s]) ,2)
    Tobsnewp[j,s] <- pow(  sqrt(y.new[j,s]) - sqrt(pcap[s,j] * N[j,s]) ,2)
    
    
    # FOR BP.N
    # Create replicate abundances (new observations) for Bayesian p-value on abundance component
    Nnew[j,s]~dpois(lambda.eff[j,s])
    
    # Residuals for 'observed' and new abundances: species and site specific residuals
    FT1[j,s] <- pow(sqrt(N[j,s]) - sqrt(lambda.eff[j,s]),2)
    FT1new[j,s] <- pow(sqrt(Nnew[j,s]) - sqrt(lambda.eff[j,s]),2)
    }
    
    # FOR BP.OBS:
    # Sum residuals over sites and years to get sp-specific bp.obs.values
    T1obsp[s]<-sum(Tobsp[1:n.allSiteYear,s])
    T1obsnewp[s]<-sum(Tobsnewp[1:n.allSiteYear,s])
    
    # SP-SPECIFIC BP.OBS
    Bp.Obs.sp[s] <-  T1obsp[s] > T1obsnewp[s] 
    
    
    # FOR BP.N
    # Sum residuals over sites and years to get sp-specific bp.N.values
    T1p[s]<-sum(FT1[1:n.allSiteYear,s])
    T1newp[s]<-sum(FT1new[1:n.allSiteYear,s])
    
    # SP-SPECIFIC BP.N
    Bp.N.sp[s] <- T1p[s] > T1newp[s]
    }
    
    # COMMUNITY BP.OBS
    Bp.Obs <- sum(T1obsnewp[1:nSpecies]) > sum(T1obsp[1:nSpecies])
    
    
    # COMMUNITY BP.N
    Bp.N <- sum(T1newp[1:nSpecies]) > sum(T1p[1:nSpecies])
    
    # Derived parameters
    
    #for (i in 1:nyears){
    #Ntotal[i] <- sum(N[s]*indexYears[,i]) 
    #}
    
    for (s in 1:nSpecies){
    for (i in 1:nyears){
    Ntotal[i,s] <- sum(N[,s]*indexYears[,i]) }}
    
    }", fill=TRUE, 
    file = "model14.2.5_SIM_2.txt")

# Inits
Nst <- (yLong.sp + 1)*restrict.sp
inits <- function(){list(mu_l = runif(1), sig_l = 0.2, sig_spsite = runif(1),
                         N=Nst,
                         mu_a1 = runif(1), sig_a1 = runif(1), mu_a1_int = runif(1), sig_a1_int = runif(1),mu_y = runif(1), sig_y = runif(1),
                         mu_a2 = runif(1), sig_a2 = runif(1),
                         mu_cd = runif(1), sig_cd = runif(1), mu_fs = runif(1), sig_fs = runif(1),
                         sig.sig.ob = runif(1),
                         mu_s = runif(1, log(30), log(50)) , sig_s = runif(1)
)}


# Params
params <- c( "mu_l", "sig_l", "sig_spsite",
             "mu_a1", "sig_a1", "mu_a1_int", "sig_a1_int", "mu_y", "sig_y",
             "mu_a2", "sig_a2", "mu_a3", "sig_a3",
             "mu_cd", "sig_cd", "mu_fs", "sig_fs",
             "sig.sig.ob", "Bp.N", "Bp.Obs",
             "mu_s", "sig_s")

####################################################
####################################################################
# Save to tun in the server of cyril
# MCMC settings
n.chain <- 1
n.iter <- 32000
n.burnin <- 30000
n.thin <- 2
model.file <- "model14.2.5_SIM_2.txt"

setwd("D:/PhD/Third chapter/Data/model/14.2.5_SIM_2")
save(data, Nst, inits, params, n.chain, n.thin, n.iter, n.burnin, model.file, file="14.2.5_SIM_2.RData")

# With jagsUI 
out <- jags(data, inits, params, model.file = "model14.2.5_SIM.txt", n.chain,
            n.thin, n.iter, n.burnin, parallel = TRUE)


# To compare:
data_comp <- list(mu.a1 = mu.a1, sig.a1 = sig.a1, mu.a2 = mu.a2, sig.a2 = sig.a2, mu.a3 = mu.a3, sig.a3 = sig.a3,
                  mu.cd = mu.cd, sig.cd = sig.cd, mu.fs = mu.fs, sig.fs = sig.fs,
                  mu.lam.alpha.spyear = mu.lam.alpha.spyear, sig.lam.spsite = sig.lam.spsite,
                  sig.lam.alpha.spyear = sig.lam.alpha.spyear,
                  sig.sig.obs = sig.sig.obs,
                  mu.sig.sp = mu.sig.sp,
                  sig.sig.sp = sig.sig.sp
)

traceplot(out, parameters = c("mu_l", "sig_l", "sig_spsite",
                              "mu_a1", "sig_a1", "mu_a2", "sig_a2", "mu_a3", "sig_a3",
                              "mu_cd", "sig_cd", "mu_fs", "sig_fs",
                              "sig.sig.ob", "Bp.N", "Bp.Obs",
                              "mu_s", "sig_s"))

###########################################################################################