
rm(list=ls())

library(jagsUI)

# ---- Data simulation ----

set.seed(15)
sim.data <- function(N = 300, sigma = 60, strip.width = 200, break.points = c(0,25,50,100,200)){
  
  # 1. Generate distances for the true population N(300) and place them in bins
  
  xall <- runif(N, -strip.width, strip.width) # Generate distances
  xall <- abs(xall)
  xall.bin <- as.numeric(cut(xall, break.points)) # Distances in bins
  nbins <- length(unique(xall.bin)) # Number of bins
  midpt <- diff(break.points)/2+break.points[-5]
  bin.width <- diff(break.points) # To account for different bin sizes 
  
  # 2. Detection probability of each observation
  
  g <- function(x, sig) exp(-x^2/(2*sig^2))
  p <- g(xall, sig=sigma)
  
  # 3. Separate detected vS non detected (Simulate the n sample)
  
  y <- rbinom(N, 1, p) # only individuals with y=1 are detected
  
  # 5. Select the distances of n (y = 1 or observed individuals)
  
  x <- xall[y==1] 
  
  # 6. Place in bins the distances of each observed individual
  
  xbin <- as.numeric(cut(x, break.points))
  
  return(list(N = N, sigma = sigma, xbin = xbin, nbins=nbins, midpt = midpt,
              strip.width = strip.width, break.points = break.points, bin.width = bin.width))
}
  
dat <- sim.data()  

# Explore data

# Compute detection probability (p) in each distance interval (From detection function applied to each interval/bin)
dist.breaks <- dat$break.points # Distance breaks
p <- rep(NA, length(dist.breaks)-1) # NA in bins
sigma <- 60
g <- function(x, sig) exp(-x^2/(2*sig^2))
for(j in 1:length(p)){  # Integral detection function / size interval
  p[j] <- integrate(g, dist.breaks[j], dist.breaks[j+1],
                    sig=sigma)$value / (dist.breaks[j+1]-dist.breaks[j])}

# Compute the multinomial cell probabilities (pi)
# psi = probability of occurring in each interval regarding the distance
interval.width <- diff(dist.breaks) # Here we could account for different bin sizes
strip.width <- 200
psi <- interval.width/strip.width
pi <- p * psi # Probability of occurring * probability of detection
              #“the probability that an individual occurs and is detected in distance class h”


# ---- MODEL ----

# Data augmentation to provide NAs to the model to estimate
xbin <- dat$xbin
n <- length(xbin) # Number of observed
nau <- 400 # Augment observed data with 300 observations more (we dont know if are detections or not)
y <- c(rep(1, n), rep(0, nau)) # Vector describing observed (1) and augmented (0) individuals
xbin1 <- c(dat$xbin, rep(NA, nau)) # Vector with distances observed (x, known) and augmented (NA)

# Data

win.data <- list (n=n, nau=nau, xbin=xbin1, y=y, strip.width=dat$strip.width,
                  bin.width=dat$bin.width, nbins=dat$nbins, midpt=dat$midpt)

# Model

setwd("C:/OneDrive/PhD/Second chapter/Data/Model")
cat("
    model{
    
    # Priors # DA and g(x,sig) Parameters 
    psi ~ dunif(0, 1)
    sigma ~ dunif(0, 1000)
    #sigma <- 2 # For example if it doesnt work, you fix a parameter and try to estimate it.
    
    # Likelihood
    
    # Construct conditional detection probability and Pr(x) for each bin 
  
    
    for(g in 1:nbins){ # midpt = mid point of each cell
    log(p[g]) <- -midpt[g] * midpt[g] / (2 * sigma * sigma) # p.detection at midbin distances
    pi[g] <- bin.width[g] / strip.width # Probability of occuring at each interval (related to area)
    } 
    
    for(i in 1:(n+nau)){
    
    z[i] ~ dbern(psi) # model for individual covariates (DA)
    
    xbin[i] ~ dcat(pi[]) # Distribution of distances (distance classes because its binned)
    
    y[i] ~ dbern(mu[i])
    mu[i] <- z[i] * p[xbin[i]] # the probability that an individual occurs and is detected in distance class h
    } 
    
    # Derived quantities: Population size and density
    N <- sum(z[]) # The sum of all estimated z is what you ultimately want
    D <- N / 0.1
    }",fill=TRUE, file = "model_different_binsize.txt")

# Inits
zst <- y
inits <- function(){ list (psi=runif(1), z=zst, sigma=runif(1,40,200)) } # One inits per parameter and z

# Params to save
params <- c("N", "sigma", "D")

# MCMC settings
nc <- 3 ; ni <- 22000 ; nb <- 2000 ; nt <- 2

out <- jags(win.data, inits, params, "model_different_binsize.txt", n.chains = nc,
            n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
traceplot(out, param = c("N", "sigma", "D"))
print(out, 2)

par(mfrow = c(1,2))
plot(density(out$sims.list$N), xlab="Population size", ylab="Frequency", frame = F) 
abline(v = 300, col = "red", lwd = 3)

plot(density(out$sims.list$sigma), xlab="Sigma", ylab="Frequency", frame = F) 
abline(v = 60, col = "red", lwd = 3) 
