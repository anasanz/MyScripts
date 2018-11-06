
rm(list=ls())

library(jagsUI)

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


# ---- Detection component ----

sigma <- 55 # Same sigma in all transects
# Detection prob at farthest distance interval for sigma
g(4,3)


# ----  Abundance component with a site effect ----

# Mean abundance and sd across sites
mu.lam <- log(1.5)				
sig.lam <- 1				
##Site effect in lambda
log.lam <- rnorm(nSites, mu.lam, sig.lam)
lam <- exp(log.lam)
# Abundance per site
N <- rpois(nSites,lam)
# Total number of individuals in all sampled transects 
N.tot <- sum(N)


# ---- Simulate continuous distance data ----

# y = number of individuals detected in each distance interval
y <- array(0, c(nSites, length(dist.breaks)-1))

for(j in 1:nSites) {
  if(N[j] == 0)
    next
  # Distance from observer to the individual
  d <- runif(N[j], 0, strip.width) 		# Uniform distribution of animals
                                      # Simulates one distance for each individual in the site (N[j])
  p <- g(x=d, sig=sigma)   		# Detection probability
  seen <- rbinom(N[j], 1, p)
  if(all(seen == 0))
    next
  d1 <- d[seen==1] 				# The distance data for seen individuals
  counts <- table(cut(d1, dist.breaks, include.lowest=TRUE))
  y[j,] <- counts 				# The number of detections in each distance interval
}
y.sum<-apply(y, 1, sum) # Total count per site


# ---- Convert data to JAGS format ----

nind<-sum(y)

sst<-dclass<-NULL

for(j in 1:nSites){
  for (k in 1:nG){
    if (y[j,k] == 0) next
    sst <- c(sst, rep(j, y[j,k]))		# Site index
    dclass <- c(dclass, rep(k, y[j,k]))	# Distance category index
  }}


# ---- Compile data for JAGS model ----

data1 <- list(nsites=nSites, nG=nG, db=dist.breaks, int.w=int.w, strip.width = strip.width, 
            y = y.sum, nind=nind, dclass=dclass, site=sst)
       
# ---- JAGS model ----

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/Model")
cat("
    model{
    # Priors
    mu.lam ~ dunif(0, 10)
    sig.lam ~ dunif(0, 10)
    sigma ~ dunif(0, 1000)
    
    for(i in 1:nind){
    dclass[i] ~ dcat(fc[site[i],]) # Part 1 of HM
    }
    
    for(j in 1:nsites){
    
    # Construct cell probabilities for nD multinomial cells (distance categories)
    for(k in 1:nD){ 
    log(p[j,k]) <- -midpt[k] * midpt[k] / (2*sigma*sigma)
    pi[j,k] <- int.w[k] / strip.width # Probability per interval
    f[j,k] <- p[j,k] * pi[j,k] # f = p*pi;  pi is the probability of occurring in the interval (pi = delta/B)
                                          # p  is the integral under the detection function over the bin h 
    fc[j,k] <- f[j,k] / pcap # Prob of detection in that cell relative to the total pdetection in the site (sum of p of all bins)
    }
    
    pcap <- sum(f[]) # Pr(capture): sum of rectangular areas
    
    y[j] ~ dbin(pcap, N[j]) # Part 2 of HM
    N[j] ~ dpois(lambda[j]) # Part 3 of HM
    log(lambda[j]) ~ dnorm(mu.lam, sig.lam)
    
    }
    # Derived parameters
    Ntotal <- sum(N[])
    area <- nsites*1*2*strip.width # Unit length == 1, half-width = B
    D <- Ntotal/area
    }
    
    }",fill=TRUE, file = "s_sigma_lambda_j.txt")


# Inits
Nst <- y + 1
inits <- function(){list(mu.lam = runif(1), sig.lam = 0.2, sigma = runif(1, 20, 100), N=Nst)}

# Params
params <- c("Ntotal", "N", "D", "sigma", "lambda", "mu.lam", "sig.lam")

# MCMC settings
nc <- 1 ; ni <- 10000 ; nb <- 2000 ; nt <- 2

out <- jags(data1, inits, params, "s_sigma_lambda_j.txt", n.chains = nc,
            n.thin = nt, n.iter = ni, n.burnin = nb)