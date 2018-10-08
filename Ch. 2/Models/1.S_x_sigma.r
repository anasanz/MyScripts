
# ---- DATA SIMULATION ----

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


# ---- MODEL ----

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