
rm(list=ls())

library(rjags)
library(jagsUI)
library(plyr)


set.seed(2013)

# ---- Data simulation ----

# 9 years of data
# Balanced number of transects per year (NA in not sampled ones)
# TRY TO ACCOUNT FOR OVERDISPERSION BY USING A POISSON-GAMMA MIXTURE DISTRIBUTION 
# Model:

# y[jt] ~ bin(p(sigma),N[jt])
# Sigma[jt] <- obs[jt] + zone[jt]

# N[jt] ~ Pois(lambda[jt])
# log(lambda[jt]) <- site[j] + year[t] + beta*yr[t]

######################################################################################
# ---- Distance sampling data ----

# Half-normal detection function
g <- function(x, sig) exp(-x^2/(2*sig^2))

# Number of transects per year (unbalanced)
nSites <- rep(100,9)			# Same number of transects by year
max.sites <- max(nSites)            # Maximun number of sites is the last year

strip.width <- 200 				# strip half-width, w (in this example only one side of the line transect is surveyed)
dist.breaks <- c(0,25,50,100,200)
int.w <- diff(dist.breaks) # width of distance categories (v)
midpt <- diff(dist.breaks)/2+dist.breaks[-5]
nG <- length(dist.breaks)-1	

# Year effect 
yrs <- 1:9 
nyrs <- length(yrs)
year_number <- 0:8 # (RS: start from 0)

#################################
# ---- Detection component ----

# RANDOM EFFECT IN OBSERVER
obs <- 1:9
nobs <- length(obs)
mu.sig.obs <- log(50)
sig.sig.obs <- 0.25
# Observer effect in sigma
sig.obs <- rnorm(length(obs), mu.sig.obs, sig.sig.obs) 
# Observer covariate

ob.id <- matrix(sample(1:9, max.sites*nyrs, replace = TRUE), nrow = max.sites, ncol = nyrs) # Matix with IDs

ob <- matrix(sig.obs[ob.id],  nrow = max.sites, ncol = nyrs) # Matrix with intercept for simulating data


#ZONE COVARIATE (SITE)
b.sig.zoneB <- 0.7
# Site specific binary co-variate
z <- data.frame(var = sample(c("A", "B"), max.sites, replace = TRUE))
z$var <- as.factor(z$var)
zone <- model.matrix(~ var-1, z)

#SIGMA
sigma <- exp(ob + matrix(b.sig.zoneB*zone[,2], nrow = max.sites, ncol = nyrs, byrow = FALSE) )# HERE IT SHOULD BE FALSE :o!


######################################
# ----  Abundance component: Site effect, Year effect and Year trend

# Site effect (FIXED): If I dont add spatial autocorrelation, add a random effect here instead
lam.site <- rnorm(max.sites, 0, 0.5) 

# Year effect (FIXED)
lam.year <- rnorm(nyrs, 0, 0.5) 


#TIME CO-VARIATE (YEAR)
b.lam.year <- 0.3
year <- matrix(NA,nrow = max.sites, ncol = nyrs)
colnames(year) <- yrs
for (i in 0:nyrs){
  year[ ,yrs[i]] <- rep(year_number[i], max.sites)
}

lam <- exp(matrix(lam.site, nrow = max.sites, ncol = nyrs, byrow = F) + # By row has to be false for site covariates that dont change with year!
             matrix(rep(lam.year,each = max.sites), nrow = max.sites, ncol = nyrs) + 
             b.lam.year*year) 


#######################################
# ---- Generate ABUNDANCE per site and year: Negative binomial written as a poisson-gamma mixture ----

# Dispersion parameter rho from gamma distribution with r=shape=scale
r <- 2 
rho <- list()
for (t in 1:nyrs){
  rho[[t]] <- rgamma(nSites[t], rate = r, shape = r) # Outside jags, it is the scale parameter is RATE
}
rhoLong <- ldply(rho,cbind) 
rho3 <- ldply(rho,rbind)
rho.sitesYears <- t(rho3)

lam.star <- lam*rho.sitesYears

# Abundance
N <- list()
for (t in 1:nyrs){
  N[[t]] <- rpois(nSites[t],lam.star[1:nSites[t], t])
} 

NLong <- ldply(N,cbind) # 1 long vector with all abundances per site and year
N3 <- ldply(N,rbind)
N.sitesYears <- t(N3) # N per site and year stored in a matrix with columns
N.tot <- lapply(N,sum)

# Introduce NA (not sampled)
vec <- seq(1,length(N.sitesYears))
na <- sample(vec, 100)
N.sitesYears[na]<-NA

# Cluster size (to correct) per site and year
clus <- list()
for (t in 1:nyrs){
  clus[[t]] <- rpois(nSites[t], 1.5)
} 
clusLong <- ldply(clus,cbind) # 1 long vector with all abundances per site and year
clus3 <- ldply(clus,rbind)
clus_size <- t(clus3) # CLUSTER SIZE per site and year stored in a matrix with columns (this is not really necessary to do, with inventing an average would be fine)
average_clus <- mean(clus_size, na.rm = TRUE)

# Correct N with average cluster size
Nclus <- N.sitesYears * average_clus
Nclus.tot <- colSums(Nclus,na.rm = TRUE) # Total pop.abundance corrected by cluster size


####
# ---- Simulate continuous distance data ----

# Nc = count of individuals detected in each distance interval
yList <- list()
for (i in 1:nyrs){
  yList[[i]] <- array(0, c(nSites[i], length(dist.breaks)-1))
}

for (t in 1:nyrs){
  for(j in 1:max.sites) {
    if(N.sitesYears[j,t] == 0 | is.na(N.sitesYears[j,t]))
      next
    # Distance from observer to the individual
    d <- runif(N.sitesYears[j,t], 0, strip.width) 		# Uniform distribution of animals
    # Simulates one distance for each individual in the site (N[j])
    p <- g(x=d, sig=sigma[j,t])   		# Detection probability. Sigma is site-time specific
    seen <- rbinom(N.sitesYears[j,t], 1, p)
    if(all(seen == 0))
      next
    d1 <- d[seen==1] 				# The distance data for seen individuals
    counts <- table(cut(d1, dist.breaks, include.lowest=TRUE))
    yList[[t]][j,] <- counts 				# The number of detections in each distance interval
  }}

y.sum.sites <- lapply(yList, function(x) rowSums(x)) # Total count per site each year
y.sum.sites2 <- ldply(y.sum.sites,rbind)
y.sum <- t(y.sum.sites2) # y per site and year stored in a matrix with columns
y.sum[na] <- NA # Add what are real NA generated from Na in N.sitesYears (not 0)


####
# ---- Convert data to JAGS format ----

nind.year <- lapply(yList,sum)
nind <- sum(unlist(nind.year, use.names = F))

y.sum # Matrix with counts 

# Co-variates

zB <- as.vector(zone[,2])
ob.id # Matrix with observers
year_number # Vector with year variable

# Get one long vector with years, distance category and site

site <- dclass <- year <- NULL
site.dclass <- year.dclass <- NULL # Fixed index to map dclass onto site and year 

for (t in 1:nyrs){
  for(j in 1:max.sites){
    if (y.sum[j,t] == 0 | is.na(y.sum[j,t])) 
      next
    site <- c(site, rep(j, y.sum[j,t])) # site index: repeat the site as many times as counts in that site (for multi model??)
    # vector of sites through years (disregarding distance class)
    year <- c(year, rep(t, y.sum[j,t]))
    
    for (k in 1:nG){
      if (yList[[t]][j,k] == 0) # Refers for the ditance classes to the list with years and bins
        next 
      dclass <- c(dclass, rep(k, yList[[t]][j,k]))	# Distance category index
      site.dclass <- c(site.dclass, rep(j, yList[[t]][j,k]))
      year.dclass <- c(year.dclass, rep(t, yList[[t]][j,k]))
    }}
}

# Create one matrix for indexing year when calculating abundance per year in JAGS

allyears <- NULL 
for (i in 1:nyrs){
  allyears <- c(allyears,rep(yrs[i],nSites[i]))
}
m <- data.frame(allyears = allyears)
m$allyears <- as.factor(m$allyears)
indexYears <- model.matrix(~ allyears-1, data = m)


# ---- Compile data for JAGS model ----

data1 <- list(nyears = nyrs, nsites = max.sites, nG=nG, int.w=int.w, strip.width = strip.width, midpt = midpt, db = dist.breaks,
              year.dclass = year.dclass, site.dclass = site.dclass, y = y.sum, nind=nind, dclass=dclass,
              zoneB = zB, ob = ob.id, nobs = nobs, year1 = year_number)

# ---- JAGS model ----

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/Model")
cat("model{
    
    # PRIORS
    
    # Priors for lambda
    r ~ dunif(0,100) # Scale and shape from dgamma

    bYear.lam ~ dnorm(0, 0.001) # Prior for the trend

    for(j in 1:nsites){
    site[j] ~ dnorm(0, 0.001)   # Prior for site effects
    }

    for(t in 1:nyears){
    year[t] ~ dnorm(0, 0.001)   # Prior for year effects
    }
    

    # Priors for sigma
    bzB.sig ~ dnorm(0, 0.001)
    
    mu.sig ~ dunif(-10, 10) # Random effects for sigma per observer
    sig.sig ~ dunif(0, 10)
    tau.sig <- 1/(sig.sig*sig.sig)
    
    # Random observer effect for sigma
    for (o in 1:nobs){
    sig.obs[o] ~ dnorm(mu.sig, tau.sig)
    }
    
    for(i in 1:nind){
    dclass[i] ~ dcat(fct[site.dclass[i], year.dclass[i], 1:nG]) 
    }
    
    for(j in 1:nsites){ 

      for (t in 1:nyears){
    
      sigma[j,t] <- exp(sig.obs[ob[j,t]] + bzB.sig*zoneB[j])
    
      # Construct cell probabilities for nG multinomial cells (distance categories) PER SITE
    
        for(k in 1:nG){ 
    
          up[j,t,k]<-pnorm(db[k+1], 0, 1/sigma[j,t]^2) ##db are distance bin limits
          low[j,t,k]<-pnorm(db[k], 0, 1/sigma[j,t]^2) 
          p[j,t,k]<- 2 * (up[j,t,k] - low[j,t,k])
          pi[j,t,k] <- int.w[k] / strip.width 
          f[j,t,k]<- p[j,t,k]/f.0[j,t]/int.w[k]                   ## detection prob. in distance category k                      
          fc[j,t,k]<- f[j,t,k] * pi[j,t,k]                 ## pi=percent area of k; drops out if constant
          fct[j,t,k]<-fc[j,t,k]/pcap[j,t] 
          }
    
      pcap[j,t] <- sum(fc[j,t, 1:nG]) # Different per site and year (sum over all bins)
    
      f.0[j,t] <- 2 * dnorm(0,0, 1/sigma[j,t]^2) # Prob density at 0
    
    
      y[j,t] ~ dbin(pcap[j,t], N[j,t]) 
      N[j,t] ~ dpois(lambda.star[j,t]) 

      lambda[j,t] <- exp(site[j] + year[t] + bYear.lam*year1[t])
      rho[j,t] ~ dgamma(r,r) # Dispersion parameter
      lambda.star[j,t] <- rho[j,t]*lambda[j,t] # Lambda corrected by dispersion parameter
        }
      }
    
    # Derived parameters

    for(t in 1:nyears){
    popindex[t] <- sum(lambda.star[,t])
    }

    # Expected abundance per year inside model

    lam.star.tot[1] <- popindex[1] # Expected abundance in year 1
    for (i in 2:nyears){
      lam.star.tot[i] <- lam.star.tot[i-1] * # Here I add the starting population size as a baseline for the trend 
        exp(bYear.lam)}

    
    }",fill=TRUE, file = "s_sigma(integral)[obs(o,j,t)_covZone(j)]_lambda(PoisGam)[site(j)_year(t)_beta.year(j)].txt")


# Inits
Nst <- y.sum + 1
inits <- function(){list(mu.sig = runif(1, log(30), log(50)), sig.sig = runif(1), bzB.sig = runif(1),
                         site = rnorm(max.sites), year = rnorm(nyrs), bYear.lam = runif(1),
                         N = Nst)} 

# Params
params <- c( "mu.sig", "sig.sig", "bzB.sig",
              "site", "year", "bYear.lam",
             "popindex", 'r', "lam.star.tot"
)

# MCMC settings
nc <- 3 ; ni <- 30000 ; nb <- 2000 ; nt <- 2

# With jagsUI 
out2 <- jags(data1, inits, params, "s_sigma(integral)[obs(o,j,t)_covZone(j)]_lambda(PoisGam)[site(j)_year(t)_beta.year(j)].txt", n.chain = nc,
             n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)

print(out2)


setwd("C:/Users/ana.sanz/Documents/PhD/Second chapter/Data/Results/TRIM")
save(out2, file = "1.TRIM.RData")
