

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
#setwd("~/Second chapter/Data/Examples")
setwd("C:/OneDrive/PhD/Second chapter/Data/Examples")
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

#C. Describe data objects to WinBUGS (Before fitting the model)
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
bugs.dir <- "C:/OneDrive/PhD/WinBUGS14"

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

setwd("C:/OneDrive/PhD/Second chapter/Data/Examples")

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
bugs.dir <- "C:/OneDrive/PhD/WinBUGS14"


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

setwd("C:/OneDrive/PhD/Second chapter/Data/Examples")

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
bugs.dir <- "C:/OneDrive/PhD/WinBUGS14"

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
setwd("C:/OneDrive/PhD/Second chapter/Data/Examples")
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


# ---- 7. Binomial N-mixture model with covariates (6.4 Applied Hier. Mod.)----

# ----. 7.1. Simulate the data ----
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

# Plot the true system state (Figure 6.2, left)
par(mfrow = c(1, 3), mar = c(5,5,2,2), cex.axis = 1.5, cex.lab = 1.5)
plot(vegHt, N, xlab="Vegetation height", ylab="True abundance (N)", frame = F, cex = 1.5)
lines(seq(-1,1,,100), exp(beta0 + beta1* seq(-1,1,,100)), lwd=3, col = "red")

# Create a covariate called wind
wind <- array(runif(M * J, -1, 1), dim = c(M, J))
# Choose parameter values for measurement error model and compute detectability
alpha0 <- -2 # Logit-scale intercept
alpha1 <- -3 # Logit-scale slope for wind
p <- plogis(alpha0 + alpha1 * wind) # Detection probability
# plot(p ~ wind, ylim = c(0,1)) # Look at relationship
# Take J [ 3 abundance measurements at each site
for(j in 1:J) {
  C[,j] <- rbinom(M, N, p[,j])
}
# Plot observed data and effect of wind on det. probability (Figure 6.2, middle)
plot(wind, C/max(C), xlab="Wind", ylab="Scaled counts: C/max(C)", frame = F, cex = 1.5)
lines(seq(-1,1,,100), plogis(alpha0 + alpha1*seq(-1,1,,100)), lwd=3, col="red")

# Expected (lambda) and realized abundance (N) and measurements (C)
cbind(lambda=round(lambda,2), N=N, C1=C[,1], C2=C[,2], C3=C[,3])

# Create factors that are unrelated to the data (because the response was not generated with their effects “built in”)
time <- matrix(rep(as.character(1:J), M), ncol = J, byrow = TRUE)
hab <- c(rep("A", 33), rep("B", 33), rep("C", 34)) # assumes M = 100


# ----7.2. Analyze N-mixture model using UNMARKED ----

library(unmarked)

# Data into format
umf <- unmarkedFramePCount(
  y = C, # Counts matrix
  siteCovs = data.frame(vegHt = vegHt, hab = hab), # Site covariates
  obsCovs = list(time = time, wind = wind)) # Observation covs
summary(umf)

# Fit model and extract estimates
# linear model for p follows first tilde, then comes linear model for lambda
summary(fm.Nmix1 <- pcount(~wind ~vegHt, data=umf, control=list(trace=T, REPORT=1)))

#In unmarked, we may choose three alternative abundance models: the Poisson (default), negative
#binomial (NB), and zero-inflated Poisson (ZIP). We can compare them by AIC or a likelihood ratio test (LRT). 
#Not surprisingly, given our way of data simulation, the Poisson comes out best.
fm.Nmix2 <- pcount(~wind ~vegHt, data=umf, mixture="NB",
                   control=list(trace=TRUE, REPORT=5))
fm.Nmix3 <- pcount(~wind ~vegHt, data=umf, mixture="ZIP",
                   control=list(trace=TRUE, REPORT=5))
cbind(AIC.P=fm.Nmix1@AIC, AIC.NB=fm.Nmix2@AIC, AIC.ZIP=fm.Nmix3@AIC)

#The parameters of the linear model are defined on the log scale for abundance and on the logit scale
#for detection. Neither is a scale most of us like to think in, so to make better sense of what the
#parameter estimates mean, we can make predictions of lambda and p for specified values of the covariates.
#The function predict uses the delta rule to compute SEs and 95% CIs.

# Predictions of lambda for specified values of vegHt, say 0.2 and 2.1
newdat <- data.frame(vegHt=c(0.2, 1.1))
predict(fm.Nmix1, type="state", newdata=newdat, append = T)

# ... or of p for values of wind of -1 to 1
newdat <- data.frame(wind=seq(-1, 1, , 5))
predict(fm.Nmix1, type="det", newdata=newdat, append = T)

#We can compute the expected values of the expected abundance lambda and of detection probability p for
#the actual data set (i.e., for the specific, observed values of the vegHt and wind covariates). For the
#detection model, predictions are produced for each of (here) 300 observations. Predictions for the three
#surveys at site 1 are in rows 1–3 (i.e., p.hat[1:3,1] below, and not in rows 1, 101, and 201 as one
#might perhaps think.

# Predict lambda and detection for actual data set
(lambda.hat <- predict(fm.Nmix1, type="state")) # lambda at every site
(p.hat <- predict(fm.Nmix1, type="det")) # p during every survey

#To visualize the covariate relationships in general, it is best to predict for a new data frame with a
#suitable range of covariate values.
# Predict lambda and detection as function of covs
newdat <- data.frame(vegHt=seq(-1, 1, 0.01))
pred.lam <- predict(fm.Nmix1, type="state", newdata=newdat)
newdat <- data.frame(wind=seq(-1, 1, 0.1))
pred.det <- predict(fm.Nmix1, type="det", newdata=newdat)

# Fit detection-naive GLM to counts and plot comparison (Figure 6.2, right)
summary(fm.glm <- glm(c(C) ~ rep(vegHt, 3), family=poisson)) # p-naive model
matplot(vegHt, C, xlab="Vegetation height", ylab="Counts", frame = F, cex = 1.5, pch = 1,
        col = "black")
lines(seq(-1,1,,100), exp(beta0 + beta1* seq(-1,1,,100)), lwd=3, col = "red")
curve(exp(coef(fm.glm)[1]+coef(fm.glm)[2]*x), -1, 1, type ="l", lwd=3, add=TRUE)
lines(vegHt, predict(fm.Nmix1, type="state")[,1], col = "blue", lwd = 3)
legend(-1, 7, c("Truth", "'Poisson GLM' with p", "Poisson GLM without p"), col=c("red",
                                                                                 "blue", "black"), lty = 1, lwd=3, cex = 1.2)

#fitting of linear models involving factors inside an N-mixture model. 
#The linear models underlying these two models are called “main-effects analysis of covariance (ANCOVA)” and
#“interaction-effects ANCOVA” in other fields. 
#For a factor A and a continuous covariate x, these linear models could be denoted “A + x” and “A * x,” respectively.

# Main-effects ANCOVA: additive effects of factor and covariate
summary(fm.Nmix2 <- pcount(~ wind+time-1 ~ vegHt+hab-1, data=umf))

# Interaction-effects ANCOVA: multiplicative effects of factor and covariate
summary(fm.Nmix3 <- pcount(~ wind*time-1-wind ~ vegHt*hab-1-vegHt, data=umf))
# Get predictions for factor levels at average values of covariates
newdat <- data.frame(vegHt=0, hab = c("A", "B", "C"))
predict(fm.Nmix2, type="state", newdata=newdat, appendData = T) # for abundance

newdat <- data.frame(time = c("1", "2", "3"), wind = 0)
predict(fm.Nmix3, type="det", newdata=newdat, appendData = T) # for detection

newdat <- data.frame(vegHt=seq(0, 2 ,by = 0.1), hab = factor("A", levels = c("A", "B", "C")))
predict(fm.Nmix2, type="state", newdata=newdat, appendData = T)

#Model selection can be done using the AIC (see above) or by a likelihood ratio test.
LRT(fm.Nmix3, fm.Nmix1)


# ----7.3. Analyze N-mixture model using BUGS (interaction effects - Ancova) ----

# Adding a condition: a famous theoretical
#ecologist has just come up with a sophisticated theory about the extinction risk of metapopulations.
#His theory predicts that our metapopulation would go extinct if 75% or more of all patches had only
#two or fewer individuals. Is it possible to estimate the extinction probability of our metapopulation?

# IN BUGS IT IS EASY: we simply define a statistic that codes for the desired condition, 
#add it up over the M sites and then obtain posterior samples for this latter quantity. 
#So as one solution to this difficult problem, we could add the following lines in the model:

critical[i] <- step(2-N[i]) # yields 1 whenever N is less or equal to 2
N.critical <- sum(critical[]) # Number of sites with critical size

#Thus, whenever Ni is less than or equal to 2, the indicator critical evaluates to 1, and N.critical
#tallies the number of populations for which this is the case.

# Bundle data
win.data <- list(C = C, M = nrow(C), J = ncol(C), wind = wind, vegHt = vegHt,
                 hab = as.numeric(factor(hab)), XvegHt = seq(-1, 1,, 100), Xwind = seq(-1, 1,,100) )
str(win.data)
# Specify model in BUGS language

setwd("C:/OneDrive/PhD/Second chapter/Data/Examples")

cat(file = "model2.txt", "
model {
# Priors
for(k in 1:3){ # Loop over 3 levels of hab or time factors
alpha0[k] ~ dunif(-10, 10) # Detection intercepts
alpha1[k] ~ dunif(-10, 10) # Detection slopes
beta0[k] ~ dunif(-10, 10) # Abundance intercepts
beta1[k] ~ dunif(-10, 10) # Abundance slopes}
# Likelihood
# Ecological model for true abundance
for (i in 1:M){
N[i] ~ dpois(lambda[i])
log(lambda[i]) <- beta0[hab[i]] + beta1[hab[i]] * vegHt[i]
# Some intermediate derived quantities
critical[i] <- step(2-N[i]) # yields 1 whenever N is 2 or less
z[i] <- step(N[i]-0.5) # Indicator for occupied site
# Observation model for replicated counts
for (j in 1:J){
C[i,j] ~ dbin(p[i,j], N[i])
logit(p[i,j]) <- alpha0[j] + alpha1[j] * wind[i,j]}}
# Derived quantities: functions of latent variables and predictions
Nocc <- sum(z[]) # Number of occupied sites among sample of M
Ntotal <- sum(N[]) # Total population size at M sites combined
Nhab[1] <- sum(N[1:33]) # Total abundance for sites in hab A
Nhab[2] <- sum(N[34:66]) # Total abundance for sites in hab B
Nhab[3] <- sum(N[67:100]) # Total abundance for sites in hab C
for(k in 1:100){ # Predictions of lambda and p ...
for(level in 1:3){ # . for each level of hab and time factors
lam.pred[k, level] <- exp(beta0[level] + beta1[level] * XvegHt[k])
logit(p.pred[k, level]) <- alpha0[level] + alpha1[level] * Xwind[k]}}
N.critical <- sum(critical[]) # Number of populations with critical size
}")

# Initial values
Nst <- apply(C, 1, max)+1 # Important to give good inits for latent N
inits <- function() list(N = Nst, alpha0 = rnorm(3), alpha1 = rnorm(3), beta0 = rnorm(3),
                         beta1 = rnorm(3))
# Parameters monitored
params <- c("alpha0", "alpha1", "beta0", "beta1", "Nocc", "Ntotal", "Nhab",
            "N.critical", "lam.pred", "p.pred") 

# MCMC settings
nc <- 3 ; ni <- 22000 ; nb <- 2000 ; nt <- 10
# Call JAGS, time run (ART 1 min) and summarize posteriors
library(jagsUI)

system.time(out <- jags(win.data, inits, params, "model2.txt", n.chains = nc,
                        n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE))

traceplot(out, param = c('alpha0', 'alpha1', 'beta0', 'beta1', 'Nocc', 'Ntotal',
                         'Nhab', 'N.critical'))
print(out, 2)

plot(table(out$sims.list$N.critical), xlab="Number of populations with critical size",
     ylab="Frequency", frame = F) # Produces plot 6.4
abline(v = 74.5, col = "red", lwd = 3)

#let us produce some plots to illustrate the
#covariate relationships for one of the levels of the two factors (Figure 6.5).
par(mfrow = c(1,2), mar = c(5,5,3,2), cex.axis = 1.5, cex.lab = 1.5)
X <- seq(-1, 1,, 100)
plot(X, out$summary[219:318,1], xlab = "Vegetation Height", ylab = "Expected abundance
(lambda)", ylim = c(0, 11), frame = F, type = "l")
polygon(c(X, rev(X)), c(out$summary[219:318,3], rev(out$summary[219:318,7])),
        col = "gray", border = F)
lines(X, out$summary[219:318,1], lty = 1, lwd = 3, col = "blue")
plot(X, out$summary[519:618,1], xlab = "Wind speed", ylab = "Detection probability (p)",
     ylim = c(0, 1), frame = F, type = "l")
polygon(c(X, rev(X)), c(out$summary[519:618,3], rev(out$summary[519:618,7])),
        col = "gray", border = F)
lines(X, out$summary[519:618,1], lty = 1, lwd = 3, col = "blue")
# ----- 8. Bernouilli ----
mean(rbinom(n = 1000,size = 1,prob = 0.5))# dbern(prob)
                                          # When size = 1 is a bernouilli trial (Success (1) / Failure (0)). Eg: flip a coin. fly in glass
rbinom(n = 1000, size = 20, prob = 0.25)  # n = times you flip the coin
                                          # Within each draw, you have only one trial

mean(rbinom(n = 1000, size = 20, prob = 0.25))# Within each draw, you have 20 trials

#Data simulation
x <- rbinom(n = 100,size = 1,prob = 0.5)

# BUGS model to get p
cat("model {
    # Priors
    prob ~ dunif(0,1) # DA parameter
    
    # Likelihood
    for(i in 1:N){
    x[i] ~ dbern(prob) # Simple Bernoulli measurement error process
    }
  
    }",fill=TRUE,file="model1.txt")

# With the data that it has (x), tries to find p with many iterations

inits <- function(){ list (prob=runif(1)) }
# Params to save
params <- c("prob")
# Experience the raw power of BUGS and summarize marginal posteriors
library(R2WinBUGS)
bd <- "c:/Program Files/WinBUGS14/" 
win.data <- list(N=100, x=x) # This is all the data that it uses to find p

out1 <- bugs(win.data, inits, params, "model1.txt", n.thin=2,n.chains=3,
             n.burnin=1000, n.iter=11000, debug=TRUE, DIC=FALSE, bugs.dir=bd)
print(out1, 3)
