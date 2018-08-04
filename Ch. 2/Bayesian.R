

library(R2WinBUGS)

#BAYESIAN EXAMPLES

# ---- 1. Simulate data from coefficients ----

set.seed(13)
n <- 100 #Sample size
beta0 <- -2
beta1 <- 1.5
x <- rnorm(n,0.1) #Predictor variable

linpred <- beta0 + beta1*x #Generate linear predictor
y <- rpois(n,exp(linpred)) # Generate observations from the model



# ---- 2. Normal GLM ----

#A. Simulate data (from given x covariate and coefficients)
x <- rnorm(10)
mu <- -3.2 + 1.5*x
y <- rnorm(10,mu,sd=4)

#B. Model specification in BUGS: Priors and model specification (stored in a model file called normal.txt)
setwd("~/Second chapter/Data/Examples")
sink("normal.txt") 
cat ("
     model{
     for (i in 1:10){
     y[i] ~ dnorm(mu[i],tau) # The likelihood
     mu[i] <- beta0 + beta1*x[i] # The linear predictor
     }
     beta0 ~ dnorm(0,.01) # Prior distributions
     beta1 ~ dnorm(0,.01)
     sigma ~ dunif(0,100)
     tau <- 1/(sigma*sigma) # Tau is the precision
     } # and a derived parameter
     ")
sink()

#C. Describe data objects to WinBUGS (Before fittint the model)
#List of data objects identified in the model file
# List of starting values (inits)
# Identify parameter names
#debug = TRUE (for winbugs open after running)
#set working directory and specigy where you installed WinBUGS (bugs directory)
library( R2WinBUGS) # "load" the R2WinBUGS package
data <- list(y=y, x=x)
inits <- function() list(beta1=rnorm(1),beta0=rnorm(1),sigma=runif(1,0,2))
parameters <- c("beta0","beta1","sigma","tau")
#bugs.dir <- "C:/Program Files/WinBUGS14"
bugs.dir <- "C:/Users/ana.sanz/Documents/WinBUGS14"

#D. Fit the model
out <- bugs(data, inits, parameters, "normal.txt", n.thin=1, n.chains=2,
            n.burnin=2000, n.iter=6000, debug=FALSE, bugs.directory = bugs.dir, 
            working.dir=getwd())

#E. Summary output
print(out,digits=2)



# ---- 3. Poisson GLM ----

#Dataset
data.fn <- function(n = 40, alpha = 3.5576, beta1 = -0.0912,
                    beta2 = 0.0091, beta3 = -0.00014){
  # n: Number of years
  # alpha, beta1, beta2, beta3: coefficients of a
  # cubic polynomial of count on year
  # Generate values of time covariate
  year <- 1:n
  # Signal: Build up systematic part of the GLM
  log.expected.count <- alpha + beta1 * year + beta2 * year^2 + beta3 * year^3
  expected.count <- exp(log.expected.count)
  # Noise: generate random part of the GLM: Poisson noise around
  #expected counts
  C <- rpois(n = n, lambda = expected.count)
  # Plot simulated data
  plot(year, C, type = "b", lwd = 2, col = "black", main = "", las = 1,
       ylab = "Population size", xlab = "Year", cex.lab = 1.2,
       cex.axis = 1.2)
  lines(year, expected.count, type = "l", lwd = 3, col = "red")
  return(list(n = n, alpha = alpha, beta1 = beta1, beta2 = beta2,
              beta3 = beta3, year = year, expected.count = expected.count, C = C))
}

data <- data.fn()

#Fit the GLM in frequentist mode (Response is the simulated data from the parameters)
fm <- glm(C ~ year + I(year^2) + I(year^3), family = poisson, data = data)
summary(fm)


# Specify model in BUGS language. Write a text file with the model definition in the BUGS language

setwd("~/Second chapter/Data/Examples")

sink("GLM_Poisson.txt") 
cat("
    model {
    # Priors
    alpha ~ dunif(-20, 20)
    beta1 ~ dunif(-10, 10)
    beta2 ~ dunif(-10, 10)
    beta3 ~ dunif(-10, 10)
    # Likelihood: Note key components of a GLM on one line each
    for (i in 1:n){
    C[i] ~ dpois(lambda[i]) # 1. Distribution for random part
    log(lambda[i]) <- log.lambda[i] # 2. Link function
    log.lambda[i] <- alpha + beta1 * year[i] + beta2 * pow(year[i],2) +
    beta3 * pow(year[i],3) # 3. Linear predictor
    } # i
    }
    ",fill = TRUE)
sink()

#Object with address of WinBUGS
#bugs.dir <- "C:/Program Files/WinBUGS14"
bugs.dir <- "C:/Users/ana.sanz/Documents/WinBUGS14"


# Bundle data (R list with the data needed for the analysis)
win.data <- list(C = data$C, n = length(data$C), year = data$year)

#Function with INITIAL VALUES
inits <- function() list(alpha = runif(1, -2, 2), beta1 = runif(1, -3, 3)) 

#List with quantities we want to estimate/monitor
# Parameters monitored
params <- c("alpha", "beta1", "beta2", "beta3", "lambda")

# MCMC settings
ni <- 500
nt <- 1
nb <- 100
nc <- 3

# Bundle data. Standardize so that it works
mean.year <- mean(data$year) # Mean of year covariate
sd.year <- sd(data$year) # SD of year covariate
win.data <- list(C = data$C, n = length(data$C),
                 year = (data$year - mean.year) / sd.year)

# Call WinBUGS from R (BRT < 1 min)
library( R2WinBUGS) # "load" the R2WinBUGS package

out <- bugs(data = win.data, inits = inits, parameters.to.save = params,
            model.file = "GLM_Poisson.txt", n.chains = nc, n.thin = nt,
            n.iter = ni, n.burnin = nb, debug = TRUE, bugs.directory = bugs.dir,
            working.directory = getwd())

# Summarize posteriors
print(out, dig = 3)
plot(out)

#☻Example of non convergence
ni <- 100
nt <- 1
nb <- 1
# Call WinBUGS from R (BRT <1 min)
tmp <- bugs(data = win.data, inits = inits, parameters.to.save = params,
            model.file = "GLM_Poisson.txt", n.chains = nc, n.thin = nt,
            n.iter = ni, n.burnin = nb, debug = TRUE, bugs.directory = bugs.dir,
            working.directory = getwd())
print(tmp,dig = 3)

#Ploting the same figure as in the frequentist we see that the results are identical
plot(1:40, data$C, type = "b", lwd = 2, col = "black", main = "", las = 1,
     ylab = "Population size", xlab = "Year")
R.predictions <- predict(glm(C ~ year + I(year^2) + I(year^3),
                             family = poisson, data = data), type = "response")
lines(1:40, R.predictions, type = "l", lwd = 3, col = "green")
WinBUGS.predictions <- out$mean$lambda
lines(1:40, WinBUGS.predictions, type = "l", lwd = 3, col = "blue", lty = 2)


# ---- 4. Binomial GLM ----

#Simulate data
data.fn <- function(nyears = 40, alpha = 0, beta1 = -0.1, beta2 = -0.9){
  # nyears: Number of years
  # alpha, beta1, beta2: coefficients
  # Generate untransformed and transformed values of time covariate
  year <- 1:nyears
  YR <- (year-round(nyears/2)) / (nyears / 2)

  # Generate values of binomial totals (N)
  N <- round(runif(nyears, min = 20, max = 100))
  # Signal: build up systematic part of the GLM
  exp.p <- plogis(alpha + beta1 * YR + beta2 * (YR^2))
  # Noise: generate random part of the GLM: Binomial noise around
  #expected counts (which is N)
  C <- rbinom(n = nyears, size = N, prob = exp.p)
  # Plot simulated data
  plot(year, C/N, type = "b", lwd = 2, col = "black", main = "", las = 1,
       ylab = "Proportion successful pairs", xlab = "Year", ylim = c(0, 1))
  points(year, exp.p, type = "l", lwd = 3, col = "red")
  return(list(nyears = nyears, alpha = alpha, beta1 = beta1,
              beta2 = beta2, year = year, YR = YR, exp.p = exp.p, C = C, N = N))
}

data <- data.fn(nyears = 40, alpha = 1, beta1 = -0.03, beta2 = -0.9)


# Specify model in BUGS language

setwd("~/Second chapter/Data/Examples")

sink("GLM_Binomial.txt")
cat("
    model {
    # Priors
    alpha ~ dnorm(0, 0.001)
    beta1 ~ dnorm(0, 0.001)
    beta2 ~ dnorm(0, 0.001)
    # Likelihood
    for (i in 1:nyears){
    C[i] ~ dbin(p[i], N[i]) # 1. Distribution for random part
    logit(p[i]) <- alpha + beta1 * year[i] + beta2 * pow(year[i],2) # Link function and linear predictor
    }
    }
    ",fill = TRUE)
sink()
#We choose a different scaling of year this time and simply subtract 20 and divide the result by 20. This leads 
#to values of the covariate that range from about -1 to 1.

# Bundle data (R lust of data needed for the analysis)
win.data <- list(C = data$C, N = data$N, nyears = length(data$C), year = data$YR)
# Initial values
inits <- function() list(alpha = runif(1, -1, 1), beta1 = runif(1, -1, 1), beta2 = runif(1, -1, 1))
# Parameters monitored
params <- c("alpha", "beta1", "beta2", "p")
bugs.dir <- "C:/Users/ana.sanz/Documents/WinBUGS14"

# MCMC settings
ni <- 2500
nt <- 2
nb <- 500
nc <- 3
# Call WinBUGS from R (BRT < 1 min)
out <- bugs(data = win.data, inits = inits, parameters.to.save = params,
model.file = "GLM_Binomial.txt", n.chains = nc, n.thin = nt,
n.iter = ni, n.burnin = nb, debug = TRUE, bugs.directory = bugs.dir,
working.directory = getwd())

print(out)

# ---- 5. Path model ----

setwd("C:/Users/Ana/Documents/PHD/Path analysis")## set working directory

## Define parameters 
N <- 1000  # sample size 
beta0 <- 0 #intercept 
beta1 <- 2 # effect of climate on vegetation
beta2 <- -2 # effect of altitude on vegetation
sdError <- 5 # sd error
error <- rnorm(N, mean=0, sd=sdError)

## Simualte some variables 
climate <- rnorm(N, mean=0, sd=1)
altitude <- rnorm(N, mean=0, sd=1)

## simulate the direct effect of climate + altitude on vegetation quality
vegetation.quality <- beta0 + beta1*climate + beta2*altitude + error


## direct effect of climate + indirect effect of climate and altitude (through vegetation quality) on body mass 
beta0.1 <- 0 #intercept 
beta1.1 <- 5 # direct effect of climate on body mass
beta2.1 <- -5 # effect of vegetation quality on body mass.
error1 <- rnorm(N, mean=0, sd=10)


bodymass <- beta0.1 + beta1.1*climate + beta2.1 * vegetation.quality + error1 



### now the corresponding jags model 
sink("path.jags")
cat("model {
    ##------------------------------------------------------------------------------------------------------------------
    ##-----------------------------------------##
    ##----------    PRIORS           ----------##
    ##-----------------------------------------##
    tau.vegetation.quality <- 1/(sd.vegetation.quality * sd.vegetation.quality)
    sd.vegetation.quality ~ dunif(0,100)
    
    tau.bodymass <- 1/(sd.tau.bodymass * sd.tau.bodymass)
    sd.tau.bodymass ~ dunif(0,100)    
    
    beta0 ~ dunif(-10,10) 
    beta1 ~ dunif(-10,10) 
    beta2 ~ dunif(-10,10) 
    
    beta0.1 ~ dunif(-10,10) 
    beta1.1 ~ dunif(-10,10) 
    beta2.1 ~ dunif(-10,10) 
    
    ##------------------------------------------------------------------------------------------------------------------
    ##-----------------------------------------##
    ##----------    PATH MODEL       ----------##
    ##-----------------------------------------##
    for( i in 1:N){
    
    # Direct effect of climate and altitude on vegetation quality
    vegetation.quality[i] ~ dnorm(mu.vegetation.quality[i] ,tau.vegetation.quality)
    mu.vegetation.quality[i] <- beta0 + beta1 * climate[i] + beta2*altitude[i]
    
    # Direct effect of climate and indirect effect of  climate and altitude through vegetation quality on body mass
    bodymass[i]~ dnorm(mu.bodymass[i] ,tau.bodymass)
    mu.bodymass[i] <- beta0.1 + beta1.1 * climate[i] + beta2.1 * mu.vegetation.quality[i]
    
    }#i
    }",fill = TRUE)
sink()

## data for jags
my.jags.input <- list(   climate = climate
                         , altitude = altitude
                         , vegetation.quality = vegetation.quality
                         , bodymass = bodymass
                         , N = N )

# parameters for to monitor 
params <- c( "beta0", "beta1", "beta2"
             ,"beta0.1", "beta1.1", "beta2.1")

## run the model 
my.jags.output <- jags( data = my.jags.input
                        , inits = NULL
                        , parameters.to.save = params
                        , model.file = "path.jags"
                        , n.chains = 3
                        , n.adapt = 500
                        , n.iter = 1000
                        , n.burnin = 0
                        , n.thin = 2
                        , parallel = TRUE
                        , DIC = FALSE
                        , bugs.format = TRUE)
##plot chains
plot(my.jags.output$samples)

## compare to simulated values  values 
unlist(my.jags.output$mean)
c( beta0, beta1, beta2,
   beta0.1, beta1.1, beta2.1)

# ---- 6. Binomial N-mixture model ----

# A. Simulate data

# Choose sample sizes and prepare observed data array C
set.seed(24) # So we all get same data set
M <- 150 # Number of sites
J <- 2 # Number of abu. measurements per site (rep. counts)
C <- matrix(NA, nrow = M, ncol = J) # to contain the obs. data
# Parameter values
lambda <- 2.5 # Expected abundance
p <- 0.4 # Probability of detection (per individual)
# Generate local abundance data (the truth)
N <- rpois(n = M, lambda = lambda)
# Conduct repeated measurements (generate replicated counts)
for(j in 1:J){
  C[,j] <- rbinom(n = M, size = N, prob = p)
}
# Look at data
# The truth ....
table(N) # True abundance distribution
sum(N) # True total population size at M sites
sum(N>0) # True number of occupied sites
mean(N) # True mean abundance (estimate of lambda)
# . and the observations
table(apply(C, 1, max)) # Observed abundance distribution (max count)
sum(apply(C, 1, max)) # Observed total population size at M sites
sum(apply(C, 1, max)>0) # Observed number of occupied sites
mean(apply(C, 1, max)) # Observed mean "relative abundance"

head(cbind(N=N, count1=C[,1], count2=C[,2])) # First 6 sites

#B. Bayesian analysis

# Bundle and summarize data set
win.data <- list(C = C, M = nrow(C), J = ncol(C))
str(win.data) # Look at data
# Specify model in BUGS language
setwd("~/Second chapter/Data/Examples")
sink("model1.txt")
cat("
    model {
# Priors
    lambda ~ dgamma(0.001, 0.001)
    p ~ dunif(0, 1)
# Likelihood
    for (i in 1:M) {
    N[i] ~ dpois(lambda) # State model
    for (j in 1:J) {
    C[i,j] ~ dbin(p, N[i]) # Observation model
    }
    }
    }
    ",fill = TRUE)
sink()

# Initial values
Nst <- apply(C, 1, max) # Avoid data/model/inits conflict
inits <- function(){list(N = Nst)}

# Parameters monitored
params <- c("lambda", "p")

# MCMC settings
ni <- 25000 ; nt <- 20 ; nb <- 5000 ; nc <- 3

# Call JAGS (ART 1 min) and summarize posteriors

library(jagsUI)

fm2 <- jags(win.data, inits, params, "model1.txt", n.chains = nc, n.thin = nt, n.iter = ni,
            n.burnin = nb)
print(fm2, dig = 3)

plot(fm2)


# ---- 7. Binomial N-mixture model with covariates ----
# Choose sample sizes and prepare observed data array y
set.seed(1) # So we all get same data set
M <- 100 # Number of sites
J <- 3 # Number of repeated abundance measurements
C <- matrix(NA, nrow = M, ncol = J) # to contain the observed data
# Create a covariate called vegHt
vegHt <- sort(runif(M, -1, 1)) # sort for graphical convenience
# Choose parameter values for abundance model and compute lambda
beta0 <- 0 # Log-scale intercept
beta1 <- 2 # Log-scale slope for vegHt
lambda <- exp(beta0 + beta1 * vegHt) # Expected abundance
plot(vegHt, lambda, type = "l", lwd = 3) # Expected abundance
# Draw local abundance and look at data so far
N <- rpois(M, lambda)
points(vegHt, N) # Add realized abundance to plot
table(N)
N
0 1 2 3 4 5 6 8 9
35 24 12 7 9 5 4 3 1
# Plot the true system state (Figure 6.2, left)
par(mfrow = c(1, 3), mar = c(5,5,2,2), cex.axis = 1.5, cex.lab = 1.5)
plot(vegHt, N, xlab="Vegetation height", ylab="True abundance (N)", frame = F, cex = 1.5)
lines(seq(-1,1,,100), exp(beta0 + beta1* seq(-1,1,,100)), lwd=3, col = "red")