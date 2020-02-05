rm(list=ls())

library(rjags)
library(jagsUI)
library(plyr)

set.seed(2013)

# 10.1 BUT RE: REMOVE SP-SITE RANDOM EFFECT
# TRY HN INSTEAD OF HR DETECTION FUNCTION

# ---- Data simulation ----

# 30 species
# 5 years (unbalanced number of transects per year) 

# Observation model calculated with Hazard Rate detection function (to make b differ per species as well)
# Sigma site-year specific
### Random sp intercept 
### Random effect in observer (site-year)

# Lambda site-year specific
### Random sp-year intercept (include different baseline abundance per species and also per year)
### 2 areas variables

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

#AREA COVARIATE (SITE AND YEAR)
#Coefficients
b.a1 <- 0.9
b.a2 <- 1.2
#Covariates
a1 <- abs(rnorm(max.sites*nyrs, 10, 5)) # Although it makes sense to make them positive, it wouldnt matter (you put them on the exp)
a2 <- abs(rnorm(max.sites*nyrs, 5, 2.5))

#SCALED
area1_mean <- mean(a1)
area1_sd <- sd(a1)
area1_sc <- (a1 - area1_mean) / area1_sd

area2_mean <- mean(a2)
area2_sd <- sd(a2)
area2_sc <- (a2 - area2_mean) / area2_sd

#ARRANGED INTO AN ARRAY

area1_sc_data <- array(area1_sc, c(max.sites, nyrs, nSpecies))
area2_sc_data <- array(area2_sc, c(max.sites, nyrs, nSpecies))

lam <- exp(lam.alpha.spyear_data + 
             array(b.a1*area1_sc, c(max.sites, nyrs, nSpecies)) +
             array(b.a2*area2_sc, c(max.sites, nyrs, nSpecies)) ) # I had to multiply the coefficients INSIDE lambda (otherwise it doesn't retrieve the a1 and a2 estimates)


# Abundance per site year (different abundance per species): N.sysp[j,t,s] 
N <- list()
N.sysp <- list() 

for (s in 1:nSpecies){
  for (t in 1:nyrs){
    N[[t]] <- rpois(nSites[t],lam[1:nSites[t], t, s]) 
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
      p <- g(x=d, sig=sigma[j,t,s])     		# Detection probability. Sigma is site-time specific
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


# Create one long vector with covariate values
a1.m <- matrix(area1_sc, nrow = max.sites, ncol = nyrs, byrow = F) # I need to make it from the same matrix
a2.m <- matrix(area2_sc, nrow = max.sites, ncol = nyrs, byrow = F)# from which I created lambda, to make it fit!

area1 <- NULL
for (i in 1:nyrs){
  area1 <- c(area1,a1.m[1:nSites[i],i])
}

area2 <- NULL
for (i in 1:nyrs){
  area2 <- c(area2,a2.m[1:nSites[i],i])
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

data1 <- list(nyears = nyrs, max.sites = max.sites, nG=nG, siteYear.dclass = siteYear.dclass, int.w=int.w, strip.width = strip.width, 
              y = yLong.sp, n.allSiteYear = n.allSiteYear, nind=nind, dclass=dclass, indexYears = indexYears, allyears = allyears,
              area1 = area1, area2 = area2, ob = ob, nobs = nobs, db = dist.breaks,
              nSpecies = nSpecies, sp.dclass = sp.dclass, nyrs = nyrs)

# ---- JAGS model ----

setwd("D:/PhD/Third chapter/Data/Model")
cat("model{
    
    # PRIORS
    
    # SPECIES SPECIFIC PARAMETERS (random effects)
    
    for (s in 1:nSpecies){              # Random intercept for sigma (dif detection per species)
    asig[s] ~ dnorm(mu_s, tau_s)}
    
    
    for(s in 1:nSpecies){              # Random intercept for lambda (dif abundance per species and year)
    for(t in 1:nyrs){
    alam[s,t] ~ dnorm(mu_l,tau_l)}}
    
    
    # Hyperparameters of species level random effects
    
    mu_s ~ dnorm(0,0.01) # Hyperparameters for sigma intercept
    tau_s <- 1/(sig_s*sig_s)
    sig_s ~ dunif(0,500)
    
    
    mu_l ~ dnorm(0,0.01) # Hyperparameters for lambda intercept
    tau_l <- 1/(sig_l*sig_l)
    sig_l ~ dunif(0,500)
    
    
    # PRIORS FOR LAMBDA
    
    ba1.lam ~ dnorm(0, 0.001)
    ba2.lam ~  dnorm(0, 0.001)
    
    
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
    N[j,s] ~ dpois(lambda[j,s]) 

    lambda[j,s] <- exp(alam[s,allyears[j]] + ba1.lam*area1[j] + ba2.lam*area2[j]) 
    } }
    # Derived parameters
    
    #for (i in 1:nyears){
    #Ntotal[i] <- sum(N[s]*indexYears[,i]) 
    #}
    
    for (s in 1:nSpecies){
    for (i in 1:nyears){
    Ntotal[i,s] <- sum(N[,s]*indexYears[,i]) }}
    
    }", fill=TRUE, 
    file = "s_HNintegral_sigma[alpha(s)_obs(j,t)]_lambda[alpha(s,t)_covArea(j,t)].txt")

# Inits
Nst <- yLong.sp + 1
inits <- function(){list(mu_l = runif(1), sig_l = 0.2, 
                         N=Nst,
                         ba1.lam = runif(1), ba2.lam = runif(1),
                         sig.sig.ob = runif(1),
                         mu_s = runif(1, log(30), log(50)) , sig_s = runif(1))
                         }


# Params
params <- c("mu_l", "sig_l", 
  "ba1.lam", "ba2.lam",
  "sig.sig.ob",
  "mu_s", "sig_s"
)

# MCMC settings
nc <- 3 ; ni <- 200000 ; nb <- 20000 ; nt <- 10

# With jagsUI 
out <- jags(data1, inits, params, "s_HNintegral_sigma[alpha(s)_obs(j,t)]_lambda[alpha(s,t)_covArea(j,t)].txt", n.chain = nc,
            n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)

save(out, file = "RE4_10.1.5_S.RData")

print(out)

summary <- as.data.frame(as.matrix(out$summary))

# To compare:
data_comp <- list(N.tot = N.tot, b.a1 = b.a1, b.a2 = b.a2,
                  mu.lam.alpha.spyear = mu.lam.alpha.spyear,
                  sig.lam.alpha.spyear = sig.lam.alpha.spyear,
                  sig.sig.obs = sig.sig.obs,
                  mu.sig.sp = mu.sig.sp,
                  sig.sig.sp = sig.sig.sp
)

traceplot(out, parameters = c("mu_l", "sig_l",
                              "ba1.lam", "ba2.lam",
                              "sig.sig.ob",
                              "mu_s", "sig_s"))

###########################################################################################


