

# ---- Example binomial N-mixture model ----

###################
## DATA SIMULATION

# Choose sample sizes and prepare observed data array C
set.seed(24) # So we all get same data set
M <- 150 # Number of sites
J <- 2 # Number of abu. measurements per site (rep. counts)
C <- matrix(NA, nrow = M, ncol = J) # to contain the obs. data
# Parameter values
lambda <- 2.5 # Expected abundance
p <- 0.4 # Probability of detection (per individual)
# Generate local abundance data (the truth)
N <- rpois(n = M, lambda = lambda) # rpois = random generation of the poisson distribution
# Conduct repeated measurements (generate replicated counts)
for(j in 1:J){
  C[,j] <- rbinom(n = M, size = N, prob = p) # Generates  number of random values of given probability from a given sample.
}
# Look at data
# The truth ....
table(N) # True abundance distribution
N
sum(N) # True total population size at M sites
#[1] 358
sum(N>0) # True number of occupied sites
#[1] 139
mean(N) # True mean abundance (estimate of lambda)
#[1] 2.386667
# . and the observations
table(apply(C, 1, max)) # Observed abundance distribution (max count)
sum(apply(C, 1, max)) # Observed total population size at M sites
#[1] 204
sum(apply(C, 1, max)>0) # Observed number of occupied sites
#[1] 120
mean(apply(C, 1, max)) # Observed mean "relative abundance"
#[1] 1.36
head(cbind(N=N, count1=C[,1], count2=C[,2])) # First 6 sites

#####################################
## FIT N-MIXTURE MODEL WITH UNMARKED

library(unmarked) # Load package
umf <- unmarkedFramePCount(y = C) # Create um data frame
summary(umf) # Summarize
(fm1 <- pcount(~1 ~1, data = umf)) # Fit model: get estimates on link scale
backTransform(fm1, "state") # Get estimates on natural scale
backTransform(fm1, "det")

#####################################
## FIT N-MIXTURE MODEL WITH JAGS

# ---- Distance sampling examples ----

rm(list=ls())

# ---- 1. Minke: frequentist & Distance ----

library("Distance")

data("minke")
head(minke)
hist(minke$distance)

minke_hn <- ds(minke, truncation = 1.5) # Fit a detection function with default
plot(minke_hn)
minke_hrcos <- ds(minke, truncation = 1.5, key = "hr")
plot(minke_hrcos)
minke_unifcos <- ds(minke, truncation = 1.5, key = "unif", # to specify a uniform model with cosine
                    adjustment = "cos", order = c(1, 2)) # adjustments of order 1 and 2 we can write

# Estimate abundance: If Data introduced as flatfile, is already given by the summary of the model by Distance
summary(minke_hn)


# ---- 2. Amakihi: frequentist & Distance ----

data("amakihi")
head(amakihi)

# 2.1. Run ds models
amakihi_hr_obs <- ds(amakihi, truncation = 82.5, transect = "point",
                     key = "hr", formula = ~obs) # Including observer as covariate

amakihi_hr_obs_mas <- ds(amakihi, truncation = 82.5, transect = "point",
                         key = "hr", formula = ~obs + mas) # Include covariate minuts after sunrise

amakihi_hn <- ds(amakihi, truncation = 82.5, transect = "point",
                 key = "hn", adjustment = NULL)

summary(amakihi_hr_obs)


# 2.2. Test plausibility: GOF
gof_ds(amakihi_hn) # Check if the model fits the data: Points not in the line and p< 0.05
                    # Dont pass the GOF
gof_ds(amakihi_hr_obs_mas)
gof_ds(amakihi_hr_obs) # Both hr models are plausible

# 2.3. Test which is the best model
summarize_ds_models(amakihi_hn, amakihi_hr_obs, amakihi_hr_obs_mas) # Not big differences in AIXC between 1st and 2nd
 # Generally, models with similar AIC lead to similar p of detection
 # Note: Do not compare with AIC models with different truncation!

#2.4. Estimate abundance


# ---- 3. Muskox: bayesian (Ch. 8 Kery&Royle)

# ---- 3. DS analysis Kery$Royle ---- 
# ---- 3.1. DATA SIMULATION
# --------- DS data: CONTINUOUS ----

# Simulate data:
# - Transect of 10 km L
# - Subject individual muskox to detection by observer traversing the transect
# - Half normal detection probability function

strip.width <- 100 # one side of the transect, really half-width
sigma <- 30 # Scale parameter of half-normal detection function

# Define half-normal detection function
g <- function(x, sig) exp(-x^2/(2*sig^2)) # Function definition
g(30, sig=sigma) # Detection probability at a distance of 30m
# Plot the detection function
par(mfrow=c(1,2))
curve(g(x, sig=30), 0, 100, xlab="Distance (x)", ylab="Detection prob.", lwd = 2, frame = F)
curve(g(x, sig=60), 0, 100, add=TRUE, lty = 2, lwd = 2)

# Define function to simulate non-hierarchical line transect data
sim.ldata <- function(N = 200, sigma = 30){
  # Function to simulate line transect data under CDS.
  # Function arguments:
  # N: number of individuals along transect with distance u(-100, 100)
  # sigma: scale parameter of half-normal detection function
  # Function subjects N individuals to sampling, and then retains the value
  # of x[distance only for individuals that are captured]
  par(mfrow = c(1,2))
  # Plot the detection function
  curve(exp(-x^2/(2*sigma^2)), 0, 100, xlab="Distance (x)", ylab="Detection prob.", lwd =
          2, main = "Detection function", ylim = c(0,1)) # Plot detection function as function of sigma
  text(80, 0.9, paste("sigma:", sigma))
  xall <- runif(N, -100,100) # 1. - Simulate distances of all N individuals
  hist(abs(xall), nclass=10, xlab = "Distance (x)", col = "grey", main = "True (grey) \nand
       observed distances (blue)") # Histogram of distances (all N)
  g <- function(x, sig) exp(-x^2/(2*sig^2)) # 2. - Define detection function to obtain p from
                                            # sigma and all distances
  p <- g(xall, sig=sigma) # 3. - Obtain detection probability (p)
  y <- rbinom(N, 1, p) # 4. - To simulate the n sample (N is the true population size)
                        # some inds. are detected and their distance measured 
                        # Simulated taking into account that p.detection decreases with distance (p)
  x <- xall[y==1]  # 5. - Select the distances of detected individuals only (sample n)
                   # this has direction (right or left side of transect)
  x <- abs(x) # now it doesn't have direction
  hist(x, col = "blue", add = TRUE)
  return(list(N = N, sigma = sigma, xall = xall, x = x))
}

# Obtain a data set for analysis
set.seed(2015) # If you want to get same results
tmp <- sim.ldata(sigma = 30) # Execute function and assign results to 'tmp'
attach(tmp)

# This histogram (blue) doesnt resemble a half-normal curve
# So we calculate the maximum likelihood estimates of the half-normal parameter s (log-transformed to enforce
#   a positive value) from the simulated data

# CONDITIONAL LIKELIHOOD
# 1. - R function that evaluates the conditional likelihoods
# Conditional likelihood: estimation of N by first estimating the detection function parameter sigma
# from the likelihood for the observed distances constructed for the n observations, a procedure that is
# naturally conditional on the event that y = 1.
# Therefore we will obtain the maximum likelihood estimates of the half-normal parameter s (log-transformed to enforce
#a positive value) from the simulated data. To do this we define an R function that evaluates the conditional and full 
#likelihoods and use optim to minimize the negative log-likelihood in each case.
Lcond <- function(lsigma){ # Define conditional nll
  sigma <- exp(lsigma)
  -1*sum(log(g(x,sig=sigma)/integrate(g, 0, 100, sig=sigma)$value/100))
}
# 2. Call optim to maximize conditional likelihood
#    (minimize the negative log-likelihood in each case)
optim(log(30), Lcond, hessian=TRUE, method="Brent", lower=-5, upper=10)
# Obtain the MLE of log(sigma) -> Convert to MLE of p and compute N=n/p

# FULL LIKELIHOOD
# 1. - R function that evaluates the full likelihoods
# Full likelihood: recognizes that n is also a stochastic outcome of the study
# and should be modeled. The distribution of n is Binomial (N, p)
# To obtain the full likelihood we simply multiply the conditional likelihood by 
# the binomial component for n.
Lfull <- function(parm){ # Define full nll
  sigma <- exp(parm[1])
  n0 <- exp(parm[2])
  N <- length(x)+ n0
  pbar <- integrate(g, 0, 100, sig=sigma)$value/100
  -1*( lgamma(N+1) - lgamma(n0+1) + sum(log(g(x,sig=sigma)/100)) + n0*log(1-pbar) )
}
# 2. Call optim to maximize conditional likelihood
#    (minimize the negative log-likelihood in each case)
optim(c(log(30), log(4)), Lfull, hessian=TRUE)
# Obtain the MLE of N directly

#We estimate the logarithm of n0 = N-n and then have to backtransform
#and add back n to it for an estimate of N. To convert estimates of density we need simply
#divide the estimates of N by the area of the transect, which was 10 km long and 0.2 km wide 
#(100 m on either side) = 2 km2.
# With maximun likelihood estimates, estimate abundance:

pbar <- integrate(g, 0, 100, sig=exp(3.39))$value/100 # p.detection with max sigma
n <- length(tmp$x)
(Nhat.condl <- n/pbar) # Abundance (n corrected by p)
(Dhat.condl <- Nhat.condl/(10*.2)) #Density(corrected by area)
n0hat <- exp(4.78)
(Nhat.full <- n + n0hat) # Abundance
(Dhat.full <- Nhat.full/(10*.2)) 

# --------- DS data: BINNED ----

set.seed(2015)
# Design settings and truth (population size N and detection function g)
interval.width <- 10
strip.width <- 100 # half-width really (one side of transect)
nbins <- strip.width%/%interval.width
sigma <- 30 # Scale parameter of half-normal detection function
g <- function(x, sig) exp(-x^2/(2*sig^2)) # Half-normal detection function
N <- 200 # Population size
# Method 1: simulate continuous distances and put into intervals
x <- runif(N, -strip.width, strip.width) # Distance all animals
p <- g(x, sig=sigma) # Detection probability
y <- rbinom(N, 1, p) # only individuals with y=1 are detected
x <- x[y==1] # this has direction (right or left side of transect)
x <- abs(x) # now it doesn't have direction
# Compute the distance category of each observation
xbin <- x %/% interval.width + 1 # note integer division function %/%
# Multinomial frequencies, may have missing levels
y.obs <- table(xbin)
# Pad the frequencies to include those with 0 detections
y.padded <- rep(0,nbins)
names(y.padded) <- 1:nbins
y.padded[names(y.obs)] <- y.obs
y.obs <- y.padded
y.true <- c(y.obs, N-length(xbin)) # Last category is "Not detected"
# Relative frequencies by binning continuous data (pi). These should compare
# with the cell probabilities computed below when N is very large
(y.rel <- y.true/N) # Last category is pi(0) from above
(pi0.v1 <- y.rel[nbins+1])
0.635

# Compute detection probability in each distance interval
dist.breaks <- seq(0, strip.width, by=interval.width)
p <- rep(NA, length(dist.breaks)-1)
for(j in 1:length(p)){
  p[j] <- integrate(g, dist.breaks[j], dist.breaks[j+1],
                    sig=sigma)$value / (dist.breaks[j+1]-dist.breaks[j])
} # This would be equal to f = p * pi in jags model
round(p, 2)
[1] 0.98 0.88 0.71 0.51 0.33 0.19 0.10 0.05 0.02 0.01


# Compute the multinomial cell probabilities analytically. These are exact.
# psi = probability of occurring in each interval
interval.width <- diff(dist.breaks)
psi <- interval.width/strip.width
pi <- p * psi
sum(pi) # This is 1 - pi(0) from above
[1] 0.3756716
(pi0.exact <- 1-sum(pi))
[1] 0.6243284 # Compare with 0.635 above
# Method 2: Use rmultinom to simulate binned observations directly
# This includes 0 cells AND n0
pi[length(p)+1] <- 1 - sum(pi)
(y.obs2 <- as.vector(rmultinom(1, N, prob=pi)))
(y.obs2 <- y.obs2[1:nbins]) # Discard last cell for n0 (because not observed)
# This is a simple multinomial example (Like sampling one transect or pooling the data from several)

# ---- 3.2. BAYESIAN ANALYSIS  ----
# --------- ANALYSIS DS data: CONTINUOUS -----
# Impala example: Distance data along a 60 km transect
# Transect width = 1000 m (area = 60 km2 -> Used to estimate density D from N)
# In BUGS : 1. specify the uniform distribution for distance explicitly 
#           2. Conditional on the distances, the observation model is specified as a simple Bernoulli trial, 
#           like in a logistic regression

# Get data and do data-augmentation
# Observed distances (meters) in the impala data set
x <- c(71.93, 26.05, 58.47, 92.35, 163.83, 84.52, 163.83, 157.33,
       22.27, 72.11, 86.99, 50.8, 0, 73.14, 0, 128.56, 163.83, 71.85,
       30.47, 71.07, 150.96, 68.83, 90, 64.98, 165.69, 38.01, 378.21,
       78.15, 42.13, 0, 400, 175.39, 30.47, 35.07, 86.04, 31.69, 200,
       271.89, 26.05, 76.6, 41.04, 200, 86.04, 0, 93.97, 55.13, 10.46,
       84.52, 0, 77.65, 0, 96.42, 0, 64.28, 187.94, 0, 160.7, 150.45,
       63.6, 193.19, 106.07, 114.91, 143.39, 128.56, 245.75, 123.13,
       123.13, 153.21, 143.39, 34.2, 96.42, 259.81, 8.72)
B <- 500 # Strip half-width. Larger than max observed distance
nind <- length(x)
# Analysis of continuous data using data augmentation (DA)
nz <- 200 # Augment observed data with nz = 200 zeroes
y <- c(rep(1, nind), rep(0, nz)) # Augmented inds. have y=0 by definition
x <- c(x, rep(NA, nz)) # Value of distance are missing for the augmented
# Bundle and summarize data set
str( win.data <- list(nind=nind, nz=nz, x=x, y=y, B=B) )
# Save text file with BUGS model
cat("
    model {
    # Priors
    sigma ~ dunif(0,1000) # Half-normal scale
    psi ~ dunif(0,1) # DA parameter

    # Likelihood
    for(i in 1:(nind+nz)){

    # Process model
    z[i] ~ dbern(psi) # DA variables
    x[i] ~ dunif(0, B) # Distribution of distances

  # Observation model
  logp[i] <- -((x[i]*x[i])/(2*sigma*sigma)) # Half-normal detection fct.
  p[i] <- exp(logp[i])
  mu[i] <- z[i] * p[i]
  y[i] ~ dbern(mu[i]) # Simple Bernoulli measurement error process
    }

# Derived quantities
N <- sum(z[1:(nind + nz)]) # Population size
D <- N / 60 # Density, with A = 60 km^2 when B = 500
}",fill=TRUE,file="model1.txt")
# Inits
zst <- y
inits <- function(){ list (psi=runif(1), z=zst, sigma=runif(1,40,200)) }
# Params to save
params <- c("N", "sigma", "D")
# Experience the raw power of BUGS and summarize marginal posteriors
library(R2WinBUGS)
bd <- "c:/Program Files/WinBUGS14/" # May have to adapt for your computer
out1 <- bugs(win.data, inits, params, "model1.txt", n.thin=2,n.chains=3,
             n.burnin=1000, n.iter=11000, debug=TRUE, DIC=FALSE, bugs.dir=bd)
print(out1, 3)


# --------- ANALYSIS DS data: BINNED ----

# Analysis of the impala data but using binned data to demonstrate the BUGS
#implementation using data augmentation. We first need to convert the distance data into distance bins,
#which we define here to be 50 m bins.

#In BUGS we have to define detection probability for each interval, which we do by evaluating the half-normal 
#detection probability function at the midpoint of each interval (input as data), which will look like this:
log(p[g]) <- -midpt[g] * midpt[g] / (2 * sigma * sigma)

#We also have to compute the probability mass for each distance interval:
  pi[g] <- delta / B # probability of x in each interval
  
# Analysis of binned data using data augmentation
B <- 500
delta <- 50 # Width of distance bins
xg <- seq(0, B, delta) # Make the interval cut points
dclass <- x %/% delta + 1 # Convert distances to distance category
nD <- length(xg) -1 # N intervals = length(xg) if max(x) = B

# Bundle data
# Note data changed to include dclass, nD, bin-width delta and midpt
midpt <- xg[-1] - delta/2 # Interval mid-points
str( win.data <- list (nind=nind, nz=nz, dclass=dclass, y=y, B=B,
                       delta=delta, nD=nD, midpt=midpt) ) # Bundle and summarize
# BUGS model specification
cat("
model{

# Priors
psi ~ dunif(0, 1)
sigma ~ dunif(0, 1000)

# Likelihood

# Construct conditional detection probability and Pr(x) for each bin
for(g in 1:nD){ # midpt = mid point of each cell
log(p[g]) <- -midpt[g] * midpt[g] / (2 * sigma * sigma) # half-normal model
pi[g] <- delta / B # probability of x in each interval
} 

for(i in 1:(n+nau)){
z[i] ~ dbern(psi) # model for individual covariates

dclass[i] ~ dcat(pi[]) # population distribution of distance class
mu[i] <- z[i] * p[dclass[i]] # p depends on distance class

y[i] ~ dbern(mu[i]) } 

# Derived quantities: Population size and density
N <- sum(z[])
D <- N / 60
}",fill=TRUE, file = "model2.txt")
# Inits function
zst <- y # DA variables start at observed value of y
inits <- function(){ list (psi=runif(1), z=zst, sigma=runif(1,40,200)) }
# Parameters to save
params <- c("N", "sigma", "D")
# Unleash WinBUGS and summarize posteriors
out2 <- bugs(win.data, inits, params, "model2.txt", n.thin=2, n.chains=3,
             n.burnin=1000, n.iter=11000, debug=TRUE, DIC=FALSE, bugs.dir = bd)
print(out2, 2)
