

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

# 3.1. ---- DS data: CONTINUOUS ----

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

#AQUIIIIIIIIIIIIII

# CONDITIONAL LIKELIHOOD
# 1. - R function that evaluates the conditional likelihoods
# Conditional likelihood: estimation of N by first estimating the detection function parameter sigma
# from the likelihood for the observed distances constructed for the n observations, a procedure that is
# naturally conditional on the event that y = 1.
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

# 3.2. ---- DS data: BINNED ----

set.seed(2015)
# Design settings and truth (population size N and detection function g)
interval.width <- 10
strip.width <- 100 # half-width really (one side of transect)
nbins <- strip.width%/%interval.width
sigma <- 30 # Scale parameter of half-normal detection function
g <- function(x, sig) exp(-x^2/(2*sig^2)) # Half-normal detection function
N <- 200 # Population size

#There are two ways to go about simulating binned distance sampling data:
# Method 1: simulate continuous distances and aggregate into distance intervals
x <- runif(N, -strip.width, strip.width) # Distance all animals
xbin1 <- x %/% interval.width + 1
xbin1 <- abs(xbin1)
hist(xbin1)
p <- g(x, sig=sigma) # Detection probability
y <- rbinom(N, 1, p) # only individuals with y=1 are detected
x <- x[y==1] # this has direction (right or left side of transect)
x <- abs(x) # now it doesn't have direction
# Compute the distance category of each observation
xbin <- x %/% interval.width + 1 # note integer division function %/%
hist(xbin, col = "blue", add = TRUE)
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
}

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