
rm(list=ls())

library(jagsUI)

# ---- Data simulation ----

# Transect of 500 m length and 200 m half-width

strip.width <- 200
interval.width <- 50
nbins <- strip.width%/%interval.width

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

# ---- MODEL ----

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

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/Examples")
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
                          # Because it is a latent variable (DA)
    
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