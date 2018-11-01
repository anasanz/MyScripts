
rm(list=ls())

library(jagsUI)

# ======================= WITH DATA AUGMENTATION ==========================
# ---- Analysis with continuous distances
  # A. ---- Simulate data similar to mine: continuous (not binned) ----
# Order of data simulation
# 1. Info: Strip width
# 2. Info: Sigma, detect function, plot it
# Function to simulate data:
# 3. Plot p = Detection function (g(x,sig)): From strip width and sigma
# 4. Generate real distances X from all individuals of population (N)
# 5. Calculate p for each individual  (p = g(x,sig))
# 6. Subset from N (real pop) the n (observed pop). 
#     For each individual of N, draw 1/0 (detec/nondetect) with Bern
# 7. Subset the observed distances x from the observed population n


# -- INFORMATION --

# Transect of 500 m length and 200 m half-width

strip_width <- 200
sigma <- 30 # Scale parameter of half-normal detection function

# Define half-normal detection function

g <- function(x, sig) exp(-x^2/(2*sig^2)) # Function definition
g(15, sig=sigma) # Example: Detection probability at a distance of 30m

# Plot the detection function

par(mfrow=c(1,2))
curve(g(x, sig=30), 0, 200, xlab="Distance (x)", ylab="Detection prob.", lwd = 2, frame = F)
curve(g(x, sig=60), 0, 200, add=TRUE, lty = 2, lwd = 2)

# Sigma of 60 looks more general to represent my data
sigma <- 60


# -- FUNCTION TO SIMULATE DATA --

# Function arguments:
# N: number of individuals along transect detected within -200,+200 m of the transect
# sigma: scale parameter of half-normal detection function

set.seed(15)

sim.data <- function(N = 300, sigma = 60){ 

# 1. Plots the detection function with the strip width and sigma
  
  par(mfrow = c(1,2))
  curve(exp(-x^2/(2*sigma^2)), 0, 200, xlab="Distance (x)", ylab="Detection prob.", lwd =
          2, main = "Detection function", ylim = c(0,1)) 
  text(80, 0.9, paste("sigma:", sigma))
  
# 2. Simulate true distances for all observations (300)
  
  xall <- runif(N, -200, 200) # Random values between -200 and +200 with uniform dist
                              # Uniform because it is true distances and we asume that 
                              # density is the same in all area
  hist(abs(xall), nclass=10, xlab = "Distance (x)", col = "grey", main = "True (grey) \nand
       observed distances (blue)") # Plot dist true N in abs values and 10 clases for clarity
  
# 3. Obtain detection probability (p) at each distance
  
  g <- function(x, sig) exp(-x^2/(2*sig^2)) # Define detection function (p = g(x,sig))
  p <- g(xall, sig=sigma) # p at each true distance given sig
  
# 4. Separate detected vS non detected (Simulate the n sample)
  
  y <- rbinom(N, 1, p) # For each observation draw a bernouilli trial 1/0 (detect/nondetect)
                      # From the specific p for each individual (calculated from g(x,sig))

# 5. Select the distances of n (y = 1 or observed individuals)
  
  x <- xall[y==1]  
  x <- abs(x) # now it doesn't have direction
  
  
  hist(x, col = "blue", add = TRUE) # Plot on top of true N, observed n (distances)
  return(list(N = N, sigma = sigma, xall = xall, x = x))
}

dat <- sim.data()


  # B. ---- Estimate population size ----

# 1. Known data:
  # Distances collected in the field:
  x <- dat$x
  # Number of detected individuals: 
  n <- length(x)
  # Strip width of the transect:
  sw <- 200
  # Length of the transect: 500m
  # Area sampled = 500*200 = 100.000 m2 = 0.1 km2
  500*200
  
# 2. Data augmentation to provide NAs to the model to estimate

  nau <- 300 # Augment observed data with 300 observations more 
            # (we dont know if are detections or not)
  y <- c(rep(1, n), rep(0, nau)) # Vector describing observed (1) and augmented (0) 
                                # individuals
  x <- c(x, rep(NA, nau)) # Vector with distances observed (x, known) and augmented (NA)
                        # because we dont know

# 3. Analysis in BUGS

# Bundle and summarize data set
# Give number of individuals observed, number of individuals augmented, observed distances
# whether an observation is obs or aug and strip width
win.data <- list(n = n, nau = nau, x=x, y=y, sw = sw)  

# BUGS model

setwd("C:/OneDrive/PhD/Second chapter/Data/Examples")

cat("
    model {
    # Priors: Both parameters (max-min limits and distribution that follow)
    sigma ~ dunif(0,1000) # Half-normal scale
    psi ~ dunif(0,1) # DA parameter
    
    # Likelihood
    for(i in 1:(n + nau)){ # For each observation of the detected and augmented data
    
    # Process model : You get z and x (one value per real and augmented observation)
    z[i] ~ dbern(psi) # DA variables 
    x[i] ~ dunif(0, sw) # Distribution of distances (Why does it belong to the process and not obs?)
    
    # Observation model: You get mu (p detection corrected by whether the individual is alive)
    logp[i] <- -((x[i]*x[i])/(2*sigma*sigma)) # Half-normal detection fct.
    p[i] <- exp(logp[i])
    mu[i] <- z[i] * p[i] # Combine p detection with whether that individual is alive or not?
                        # Probability that that individual is alive and detected?
                        # If the individual is not alive (Z = 0), cant be detected and mu[i] = 0
    y[i] ~ dbern(mu[i]) # Simple Bernoulli measurement error process
                        # Model y[i] (1/0) as bernouilli with probability mu[i]

    # y[i] ~ dbern(z[i] * p[i]) # Simple Bernoulli measurement error process
                                # If y is a 0 (non detected), the probability 
                                # that p is high (e.g. 1) is very low (because at the
                                # transect with a high p you would have detected it y=1)}
    
 # Derived quantities
    N <- sum(z[1:(n + nau)]) # Population size
    D <- N / 0.1 # Density, with A = 0.1 km^2 when sw = 200

    }",fill=TRUE,file="model_continuousDS.txt")

# Inits
zst <- y
inits <- function(){ list (psi=runif(1), z=zst, sigma=runif(1,40,200)) } # One inits per parameter and z

# Params to save
params <- c("N", "sigma", "D")

# MCMC settings
nc <- 3 ; ni <- 22000 ; nb <- 2000 ; nt <- 2

out <- jags(win.data, inits, params, "model_continuousDS.txt", n.chains = nc,
                        n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
traceplot(out, param = c("N", "sigma", "D"))
print(out, 2)

par(mfrow = c(1,2))
plot(table(out$sims.list$N), xlab="Population size", ylab="Frequency", frame = F) 
abline(v = 300, col = "red", lwd = 3)

plot(table(out$sims.list$sigma), xlim = c(40,70), xlab="Sigma", ylab="Frequency", frame = F) 
abline(v = 60, col = "red", lwd = 3) #?????

# ---- Analysis with binned distances ----
  # A. ---- Simulate data similar to mine: binned but equal bin sizes (p406)----

# Order of data simulation
# 1. Strip width and bin size
#Function to simulate the data:
# 2. Generate distances for the true population N and place them in bins
# 3. Calculate p for each individual  (p = g(x,sig))
# 4. Subset from N (real pop) the n (observed pop) with y = 1/0 and put it in bins
# 5.  Add 0 in the bins with no individuals detected (pad = rellenar). 
#   But that contain individuals in the real population
# 6.  Compute detection probability in each distance interval 
# 7. Compute the multinomial cell probabilities (pi): 
#  Combination between probability of occurring * probability of detection


# -- INFORMATION --
# Transect of 500 m length and 200 m half-width
strip.width <- 200
interval.width <- 50
nbins <- strip.width%/%interval.width

# -- FUNCTION TO SIMULATE DATA --
set.seed(15)
sim.data <- function(N = 300, sigma = 60){ 
  
  # 1. Plots the detection function with the strip width and sigma
  
  par(mfrow = c(1,2))
  curve(exp(-x^2/(2*sigma^2)), 0, 200, xlab="Distance (x)", ylab="Detection prob.", lwd =
          2, main = "Detection function", ylim = c(0,1)) 
  text(80, 0.9, paste("sigma:", sigma))
  
  # 2. Generate distances for the true population N(300) and plot
  
  xall <- runif(N, -strip.width, strip.width)
  xall <- abs(xall)
  xbinall <- xall %/% interval.width + 1 # note integer division function %/%
  xbin_plot1 <- xbinall #♥ Plot it
  xbin_plot1[xbin_plot1 == 1] <- 0.5
  xbin_plot1[xbin_plot1 == 2] <- 1.5
  xbin_plot1[xbin_plot1 == 3] <- 2.5
  xbin_plot1[xbin_plot1 == 4] <- 3.5
  hist(xbin_plot1, breaks = 3, xlab = "Distance bins (x)", col = "grey", main = "True (grey) \nand
       observed distances (blue)") # Plot dist true N in abs values and 10 clases for clarity
  
  # Number of bins
  nbins <- strip.width%/%interval.width

  # 3. Obtain detection probability (p) at each distance
  g <- function(x, sig) exp(-x^2/(2*sig^2))
  p <- g(xall, sig=sigma) 
  
  # 4. Separate detected vS non detected (Simulate the n sample)
  y <- rbinom(N, 1, p) # only individuals with y=1 are detected
  
  # 5. Select the distances of n (y = 1 or observed individuals)
  x <- xall[y==1] 
  
  # 6. Compute the distance category of each observed individual and plot it
  xbin <- x %/% interval.width + 1 
  xbin_plot2 <- xbin #♥ Plot it
  xbin_plot2[xbin_plot2 == 1] <- 0.5
  xbin_plot2[xbin_plot2 == 2] <- 1.5
  xbin_plot2[xbin_plot2 == 3] <- 2.5
  xbin_plot2[xbin_plot2 == 4] <- 3.5
  
  hist(xbin_plot2, breaks = 3, col = "blue", add = TRUE)
  
  # 7. Add the bins with non detected individuals as 0 
  
  y.obs <- table(xbin) # Multinomial frequencies, may have missing levels
  y.padded <- rep(0,nbins) # Pad the frequencies to include those with 0 detections
  names(y.padded) <- 1:nbins
  y.padded[names(y.obs)] <- y.obs
  y.obs <- y.padded # If all bins are detected y.obs = y.padded
  y.true <- c(y.obs, N-length(xbin)) # Last category is "Not detected"
  
  # 8. Relative frequencies of detection of each bin (similar to pi with N big)
  (y.rel <- y.true/N) # Last category is pi(0) or nondetected from above
  (pi0.v1 <- y.rel[nbins+1]) # Frequency of non detected  
  
  # 9.  Compute detection probability in each distance interval 
  # (From detection function applied to each interval/bin)
  dist.breaks <- seq(0, strip.width, by=interval.width) # Distance breaks
  p <- rep(NA, length(dist.breaks)-1) # NA in bins
  for(j in 1:length(p)){  # Integral detection function / size interval
    p[j] <- integrate(g, dist.breaks[j], dist.breaks[j+1],
                      sig=sigma)$value / (dist.breaks[j+1]-dist.breaks[j])
  }
  round(p, 2)
  
  # 10. Compute the multinomial cell probabilities (pi)
  # psi = probability of occurring in each interval regarding the distance
  interval.width <- diff(dist.breaks) # Here we could account for different bin sizes
  psi <- interval.width/strip.width
  pi <- p * psi # Probability of occurring * probability of detection
                #“the probability that an individual occurs and is detected in distance class h”
  sum(pi) # This is 1 - pi(0) from above
  (pi0.exact <- 1-sum(pi)) # Compare with 0.635 above
  
  return(list(N = N, sigma = sigma, xbinall = xbinall, xbin = xbin, nbins=nbins, psi = psi, pi = pi))
  }
dat <- sim.data()
  

  # B. ---- Estimate population size ----

#1. Known data
# Distance (in bands collected in the field)
xbin <- dat$xbin
# Strip width of the transect:
sw <- 200
# Number of bins
nbins <- dat$nbins
# Bin width
bw <- 50
# Interval mid-points
dist.breaks <- seq(0, sw, by=bw) 
midpt <- dist.breaks[-1] - bw/2  
# Number of detected individuals: 
n <- length(xbin)

# Length of the transect: 500m
# Area sampled = 500*200 = 100.000 m2 = 0.1 km2
500*200


# 2. Data augmentation to provide NAs to the model to estimate

nau <- 400 # Augment observed data with 300 observations more 
# (we dont know if are detections or not)
y <- c(rep(1, n), rep(0, nau)) # Vector describing observed (1) and augmented (0) 
# individuals
xbin1 <- c(xbin, rep(NA, nau)) # Vector with distances observed (x, known) and augmented (NA)
# because we dont know

# 3. Analysis in BUGS

# Bundle data

win.data <- list (n=n, nau=nau, xbin=xbin1, y=y, sw=sw,
                  bw=bw, nbins=nbins, midpt=midpt)

setwd("C:/OneDrive/PhD/Second chapter/Data/Examples")
cat("
model{
    
    # Priors # DA and g(x,sig) Parameters 
    psi ~ dunif(0, 1)
    sigma ~ dunif(0, 1000)
    #sigma <- 2 # For example if it doesnt work, you fix a parameter and try to estimate it.

    # Likelihood

      # Construct conditional detection probability and Pr(x) for each bin 
      # Objective is pi, then applied to each individual

    for(g in 1:nbins){ # midpt = mid point of each cell
      log(p[g]) <- -midpt[g] * midpt[g] / (2 * sigma * sigma) # p.detection at midbin distances
      pi[g] <- bw / sw # Probability of occuring at each interval (related to area)
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
    }",fill=TRUE, file = "model_equalbinDS.txt")



# Inits
zst <- y
inits <- function(){ list (psi=runif(1), z=zst, sigma=runif(1,40,200)) } # One inits per parameter and z

# Params to save
params <- c("N", "sigma", "D")

# MCMC settings
nc <- 3 ; ni <- 22000 ; nb <- 2000 ; nt <- 2

out <- jags(win.data, inits, params, "model_equalbinDS.txt", n.chains = nc,
            n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
traceplot(out, param = c("N", "sigma", "D"))
print(out, 2)

par(mfrow = c(1,2))
plot(density(out$sims.list$N), xlab="Population size", ylab="Frequency", frame = F) 
abline(v = 300, col = "red", lwd = 3)

plot(density(out$sims.list$sigma), xlab="Sigma", ylab="Frequency", frame = F) 
abline(v = 60, col = "red", lwd = 3) 

# ======================== NO DATA AUGMENTATION =========================================
# ---- Three part multinomial model ----
# 1. Multinomial (p in each bin)
# 2. Binomal (detection)
# 3. Poisson (process)

library(AHMbook)
library(jagsUI)

# Simulate line transect data set (continuous data, then we will cat in bins)
# Discard = FALSE, to not discard the data where individuals were not detected
# Important when you dont do data augmentation
set.seed(1234)
tmp <- simHDS(type="line", discard0=FALSE) 
attach(tmp)

# Get number of individuals detected per site
# ncap = 1 plus number of detected individuals per site
ncap <- table(data[,1]) # ncap = 1 if no individuals captured
sites0 <- data[is.na(data[,2]),][,1] # sites where nothing detected
ncap[as.character(sites0)] <- 0 # Fill in 0 for sites with no detections
ncap <- as.vector(ncap)

# Prepare other data (CATEGORIZE IN BINS)
site <- data[!is.na(data[,2]),1] # site ID of each observation
delta <- 0.1 # distance bin width for rect. approx.
midpt <- seq(delta/2, B, delta) # make mid-points and chop up data
dclass <- data[,5] %/% delta + 1 # convert distances to cat. distances
nD <- length(midpt) # Number of distance intervals
dclass <- dclass[!is.na(data[,2])] # Observed categorical observations
nind <- length(dclass) # Total number of individuals detected

# Bundle and summarize data set
# ncap is the number of individuals detected in each transect (0 if nothing detected,TRANSECT = 100)
# and dclass are the distances at which ind were detected (INDIVIDUAL = more than 100)
str( win.data <- list(nsites=nsites, nind=nind, B=B, nD=nD, midpt=midpt, delta=delta,
                      ncap=ncap, habitat=habitat, wind=wind, dclass=dclass, site=site) )


# BUGS model specification for line-transect HDS (NOT point transects!)

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/Examples")

cat("
    model{

    # Priors
    alpha0 ~ dunif(-10,10)
    alpha1 ~ dunif(-10,10)
    beta0 ~ dunif(-10,10)
    beta1 ~ dunif(-10,10)

    for(i in 1:nind){
    dclass[i] ~ dcat(fc[site[i],]) # Part 1 of HM
    }
            # fc is a parameter for the pdetection for an individual i
            # in transect s (knowing the bin)
            # SECOND COLUMN???
    for(s in 1:nsites){

    # Construct cell probabilities for nD multinomial cells
    for(g in 1:nD){ # midpt = mid-point of each cell

      log(p[s,g]) <- -midpt[g] * midpt[g] / (2*sigma[s]*sigma[s])
      pi[s,g] <- delta / B # Probability per interval

      f[s,g] <- p[s,g] * pi[s,g] 
        # Detection probability in a site in a distance bin * 
        # probability of being in that bin given the size of the bin

      fc[s,g] <- f[s,g] / pcap[s] # WHAT IS THIS
    }

    pcap[s] <- sum(f[s,]) # Pr(capture): sum of rectangular areas
    ncap[s] ~ dbin(pcap[s], N[s]) # Part 2 of HM
    N[s] ~ dpois(lambda[s]) # Part 3 of HM
    log(lambda[s]) <- beta0 + beta1 * habitat[s] # Linear model abundance
    log(sigma[s])<- alpha0 + alpha1*wind[s] # Linear model detection

    }
    # Derived parameters
    Ntotal <- sum(N[])
    area <- nsites*1*2*B # Unit length == 1, half-width = B
    D <- Ntotal/area
    }
    ",fill=TRUE, file = "three_part.txt")
# Inits
Nst <- ncap + 1
inits <- function(){list(alpha0=0, alpha1=0, beta0=0, beta1=0, N=Nst)}
# Params to save
params <- c("alpha0", "alpha1", "beta0", "beta1", "Ntotal","D")
# MCMC settings
ni <- 12000 ; nb <- 2000 ; nt <- 1 ; nc <- 3
# Run JAGS (ART 1 min) and summarize posteriors
library(jagsUI)
out3 <- jags(win.data, inits, params, "model3.txt", n.thin=nt,
             n.chains=nc, n.burnin=nb, n.iter=ni)
print(out3, 2)
