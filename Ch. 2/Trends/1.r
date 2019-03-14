
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

# ---- Compile data for JAGS model ----

data1 <- list(nyears = nyrs, max.sites = max.sites, nG=nG, siteYear.dclass = siteYear.dclass, int.w=int.w, strip.width = strip.width, 
              y = yLong, nind=nind, dclass=dclass, midpt = midpt, sitesYears = sitesYears, indexYears = indexYears,
              zoneB = zoneB, ob = ob, nobs = nobs, db = dist.breaks, year1 = year1, average_clus = average_clus)

# ---- JAGS model ----

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/Model")
cat("model{
    
    # PRIORS
    
    # Priors for lambda
    r ~ dunif(0,100) # Scale and shape from dgamma

    bYear.lam ~ dnorm(0, 0.001) # Prior for the trend

    for(j in 1:max.sites){
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
    dclass[i] ~ dcat(fct[siteYear.dclass[i], 1:nG])  
    }
    
    for(j in 1:length(y)){ 
    
    sigma[j] <- exp(sig.obs[ob[j]] + bzB.sig*zoneB[j])
    
    # Construct cell probabilities for nG multinomial cells (distance categories) PER SITE
    
    for(k in 1:nG){ 
    
    up[j,k]<-pnorm(db[k+1], 0, 1/sigma[j]^2) ##db are distance bin limits
    low[j,k]<-pnorm(db[k], 0, 1/sigma[j]^2) 
    p[j,k]<- 2 * (up[j,k] - low[j,k])
    pi[j,k] <- int.w[k] / strip.width 
    f[j,k]<- p[j,k]/f.0[j]/int.w[k]                   ## detection prob. in distance category k                      
    fc[j,k]<- f[j,k] * pi[j,k]                 ## pi=percent area of k; drops out if constant
    fct[j,k]<-fc[j,k]/pcap[j] 
    }
    
    pcap[j] <- sum(fc[j, 1:nG]) # Different per site and year (sum over all bins)
    
    f.0[j] <- 2 * dnorm(0,0, 1/sigma[j]^2) # Prob density at 0
    # To set that prob.of detection at distance 0 is one, you divide by f0 in the loop up
    
    y[j] ~ dbin(pcap[j], N[j]) 
    N[j] ~ dpois(lambda.star[j]) 
    lambda[j] <- exp(log.lambda[sitesYears[j]] + bzB.lam*zoneB[j] + bYear.lam*year1[j])
    rho[j] ~ dgamma(r,r) # Dispersion parameter
    lambda.star[j] <- rho[j]*lambda[j] # Lambda corrected by dispersion parameter(?)
    }
    
    # Derived parameters
    for (i in 1:nyears){
    Ntotal[i] <- sum(N*indexYears[,i]) 
    }
    
    for (i in 1:nyears){
    Ntotal_clus[i] <- average_clus*(sum(N*indexYears[,i]))
    }
    
    }",fill=TRUE, file = "s_sigma(integral)[obs(o,j,t)_covZone(j)]_lambda(PoisGam)[alpha(j)_covZone(j)_year(j)]_clustersize.txt")


