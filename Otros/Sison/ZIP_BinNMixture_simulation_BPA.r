

rm(list=ls())

library(rjags)
library(jagsUI)
library(plyr)

set.seed(2013)

# ---- Data simulation ----

# j = Number of sites
# i = Number of ocassions
# t = Number of years

# Abundance: Random year effect (intercept) + covariate
#   lam = alpha0[t] + alpha1 * X[]
# Detection: Intercept + covariate (I don't know but maybe: Observer random effect + land use)
#   p = beta0 + beta1*X[j]

nsites = 200
nrep = 3
nyrs = 6

  # R: number of sites at which counts were made (= number of spatial reps)
  # T: number of times that counts were made at each site
  # (= number of temporal reps)
  # xmin, xmax: define range of the covariate X
  # alpha0 and alpha1: intercept and slope of log-linear regression
  # relating abundance to the site covariate A
  # beta0 and beta1: intercept and slope of logistic-linear regression
  # of detection probability on A
  
  
  # Ecological process
  # Covariate values: sort for ease of presentation
  xmin <- -1
  xmax <- 1
  X <- sort(runif(n = nsites, min = xmin, max = xmax))
  # Parameters
  mu.alpha0 <- 1
  sig.alpha0 <- 0.5
  alpha0 <- rnorm(nyrs, mu.alpha0, sig.alpha0) # Random year effect

  alpha1 <- 3
  
  # Relationship expected abundance – covariate (Different per year)
  lam <- exp(matrix(alpha0, nrow = nsites, ncol = nyrs, byrow = TRUE) +
             matrix(alpha1*X, nrow = nsites, ncol = nyrs, byrow = FALSE))
  
  # Generate lambda effective (determines if a site is suitable or not == differenciates between true and false zeros)
  omega <- 0.6
  z <- rbinom(nsites*nyrs, 1, omega)
  lam.eff <- lam * z
  
  # Add Poisson noise: draw N from Poisson(lambda)
  N <- list()
  for (t in 1:nyrs){
    N[[t]] <- rpois(n = nsites, lambda = lam.eff[ ,t])
  }
  
  NLong <- ldply(N,cbind) # 1 long vector with all abundances per site and year
  N3 <- ldply(N,rbind)
  N.sitesYears <- t(N3)
  
  # Total number of individuals in all sampled transects per year
  N.tot <- lapply(N,sum)
  
  # Observation process
  # Parameters
  beta0 <- 0
  beta1 <- -5
  # Relationship detection prob – covariate
  p <- plogis(beta0 + beta1 * X) # p is site specific (same each year)
  
  # Make a 'census' (i.e., go out and count things)
  y <- array(dim = c(nsites, nrep, nyrs)) # Array for counts
  for(t in 1:nyrs){
    for (i in 1:nrep){
      y[,i,t] <- rbinom(n = nsites, size = N.sitesYears[,t], prob = p)
    }}


# Bundle data
data <- list(y = y, nsites = nsites, nrep = nrep, nyrs = nyrs, X = X)

# Specify model in BUGS language
sink("Nmix1.jags")
cat("
    model {
    
    # PRIORS
    omega ~ dunif(0, 1)                   # Prior for site suitability (ZIP)

    alpha1 ~ dunif(−10, 10)               # Prior co-variates lambda and p
    beta0 ~ dunif(−10, 10)
    beta1 ~ dunif(−10, 10)
    
    mu.alpha0 ~ dunif(-10, 10)            # Prior year random effect
    sig.alpha0 ~ dunif(0, 10)
    tau.alpha0 <- 1/(sig.alpha0*sig.alpha0)


    # RANDOM INTERCEPT PER YEAR
    for (t in 1:nyrs){                       # Year random effect in lambda
    alpha0[t] ~ dnorm(mu.alpha0, sig.alpha0)
    }
    
    # LIKELIHOOD

    # Ecological model for true abundance
    for (j in 1:nsites){                          
      for (t in 1:nyrs){                       
      z[j,t] ~ dbern(omega)                   # Latent suitability state for site (ZIP)

      N[j,t] ~ dpois(lam.eff[j,t])          # Latent abundance state
      lam.eff[j,t] <- z[j,t] * lambda[j,t]
      lambda[j,t] <- exp(alpha0[t] + alpha1 * X[j])
    
      # Observation model for replicated counts
    
      for (i in 1:nrep){                    
        y[j,i,t] ~ dbin(p[j,i,t], N[j,t])   # Detection
        p[j,i,t] <- exp(lp[j,i,t])/(1 + exp(lp[j,i,t])) # Transformation p to logit
        lp[j,i,t] <- beta0 + beta1 * X[j] # SOMETHING WRONG HERE
      }#i
      }#t
      }#j
    
    # Derived and other quantities
    
    for (t in 1:nyrs){
    totalN[t] <- sum(N[,t])	# Estimate total pop. size across all sites
    }
    }
    ",fill = TRUE)
sink()
#

# Initial values
# It is advisable to give initial values for the latent state z, the best option is to provide a vector of 1
Nst <- apply(y, c(1, 3), max) + 1
inits <- function(){list(N = Nst, mu.alpha0 = runif(1), sig.alpha0 = runif(1),
                         alpha1 = runif(1), beta0 = runif(1, -1, 1), beta1 = runif(1, -1, 1),
                         z = matrix(1, ncol = nyrs, nrow = nsites))}

# Parameters monitored
params <- c("omega", "mu.alpha0", "sig.alpha0",  "alpha1", "beta0", "beta1", "totalN")

# MCMC settings
ni <- 30000
nt <- 15
nb <- 15000
nc <- 3

# Call JAGS from R (BRT 3 min)
out1 <- jags(data, inits, params, "Nmix1.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)

