
rm(list=ls())

library(rjags)
library(jagsUI)

set.seed(2013)
# ---- Data simulation ----
#### Simulate abundance for one species; one sigma; site-specific lambda

#then generate distance sampling data from that   #####
#### and analyze with data-generating model; summarize results across iterations 

# Half-normal detection function
g <- function(x, sig) exp(-x^2/(2*sig^2))

nSites <- 50					# number of line transect surveys
strip.width <- 200 				# strip half-width, w (in this example only one side of the line transect is surveyed)
dist.breaks <- c(0,25,50,100,200)
int.w <- diff(dist.breaks) # width of distance categories (v)
midpt <- diff(dist.breaks)/2+dist.breaks[-5]
nG <- length(dist.breaks)-1	

# Year effect 
yrs <- 0:7 # eight years
nyrs <- length(yrs)


# ---- Detection component ----

sigma <- 55 # Same sigma in all transects
# Detection prob at farthest distance interval for sigma
g(4,3)


# ----  Abundance component with a site effect ----

# Mean abundance and sd across sites
mu.lam <- log(1.5)				
sig.lam <- 1				
##Site effect in lambda
log.lam <- rnorm(nSites, mu.lam, sig.lam) # Here I add it as a random effect, it could be as a predictor with covariates
lam <- exp(log.lam)                       # Also, I am assuming that lambda doesnt change by year

# Abundance per site and year
N <- matrix(nrow = nSites, ncol=nyrs)
for (t in 1:nyrs){
  N[ ,t] <- rpois(nSites,lam)
}

# Total number of individuals in all sampled transects per year
N.tot <- apply(N,2,sum)


# ---- Simulate continuous distance data ----

# Nc = count of individuals detected in each distance interval
y <- array(0, c(nSites, length(dist.breaks)-1))
yList <- list(y,y,y,y,y,y,y,y)

for (t in 1:nyrs){
  for(j in 1:nSites) {
    if(N[j,t] == 0)
    next
    # Distance from observer to the individual
    d <- runif(N[j,t], 0, strip.width) 		# Uniform distribution of animals
                                          # Simulates one distance for each individual in the site (N[j])
    p <- g(x=d, sig=sigma)   		# Detection probability
    seen <- rbinom(N[j,t], 1, p)
    if(all(seen == 0))
      next
    d1 <- d[seen==1] 				# The distance data for seen individuals
    counts <- table(cut(d1, dist.breaks, include.lowest=TRUE))
    yList[[t]][j,] <- counts 				# The number of detections in each distance interval
  }}

y <- do.call(cbind,lapply(yList, function(x) rowSums(x))) # Total count per site each year


# ---- Convert data to JAGS format ----

nind <- sum(y)

# Get one long vector with counts, distance category and site across all years
site <- dclass <- year <- NULL

for (t in 1:nyrs){
  for(j in 1:nSites){
  if (y[j,t] == 0) next
    
    site <- c(site, rep(j, y[j,t])) # site index: repeat the site as many times as counts in that site
                        # vector of sites through years (disregarding distance class)
    year <- c(year, rep(t, y[j,t]))
    for (k in 1:nG){
      if (yList[[t]][j,k] == 0) next # Refers for the ditance classes to the list with years and bins
      dclass <- c(dclass, rep(k, yList[[t]][j,k]))	# Distance category index
    }}
}