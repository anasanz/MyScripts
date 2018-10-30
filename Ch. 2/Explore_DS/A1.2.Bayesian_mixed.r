
# =========================================================================
#                       LMM: VIPER EXAMPLE
#===========================================================================

# ===================== DATA SIMULATION ===============================

n.groups <- 56 # Number of populations
n.sample <- 10 # Number of vipers in each pop
n <- n.groups * n.sample # Total number of data points
pop <- gl(n = n.groups, k = n.sample) # Indicator for population

#We directly normalize covariate length to avoid trouble with WinBUGS.
# Body length (cm)
original.length <- runif(n, 45, 70)
mn <- mean(original.length)
sd <- sd(original.length)
cat("Mean and sd used to normalise.original length:", mn, sd, "\n\n")
length <- (original.length-mn) / sd 
hist(length, col = "grey")

# We build a design matrix without intercept.
Xmat <- model.matrix(~ pop * length-1-length)
print(Xmat[1:21,], dig = 2) # Print top 21 rows

intercept.mean <- 230 # mu alpha
intercept.sd <- 20 # sigma alpha
slope.mean <- 60 # mu beta
slope.sd <- 30 # sigma beta
intercept.effects <- rnorm(n = n.groups, mean = intercept.mean, sd = intercept.sd)
slope.effects <- rnorm(n = n.groups, mean = slope.mean, sd = slope.sd)
all.effects <- c(intercept.effects, slope.effects) # Put them all together

# We assemble the measurements yi as before.
lin.pred <- Xmat[,] %*% all.effects # Value of lin.predictor
eps <- rnorm(n = n, mean = 0, sd = 30) # residuals
mass <- lin.pred + eps # response lin.pred + residual
hist(mass, col = "grey") # Inspect what we've created

library("lattice")
xyplot(mass ~ length | pop)

# ===================== MODEL IN FREQUENTIST =============================

library('lme4')
lme.fit1 <- lmer(mass ~ length + (1 | pop), REML = TRUE)
lme.fit1

# ===================== MODEL IN JAGS ===============================

library(jagsUI)

# Specify data that will be used in the model

win.data <- list(mass = as.numeric(mass), pop = as.numeric(pop), length = length,
                ngroups = max(as.numeric(pop)), n = n)

# Write model
setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/Examples")

cat("
    model {

    # Priors:

    #RANDOM INTERCEPT PER POPULATION
    for (i in 1:ngroups){
    alpha[i] ~ dnorm(mu.int, tau.int) # Random intercepts
    }
    mu.int ~ dnorm(0, 0.001) # Mean hyperparameter for random intercepts
    tau.int <- 1 / (sigma.int * sigma.int)
    sigma.int ~ dunif(0, 100) # SD hyperparameter for random intercepts
   
    #COMMON SLOPE FOR ALL POPULATIONS
    beta ~ dnorm(0, 0.001) # Common slope

    tau <- 1 / ( sigma * sigma) # Residual precision
    sigma ~ dunif(0, 100) # Residual standard deviation

    # Likelihood

    for (i in 1:n) {
    mass[i] ~ dnorm(mu[i], tau) # The random variable
    mu[i] <- alpha[pop[i]] + beta* length[i] # Expectation
    }
    }
    ",fill = TRUE, file = "lmm.txt")

# Inits function
inits <- function(){list(alpha = rnorm(n.groups, 0, 2), beta = rnorm(1, 1, 1),
                        mu.int = rnorm(1, 0, 1), sigma.int = rlnorm(1), sigma = rlnorm(1))}

# Parameters to estimate
parameters <- c("alpha", "beta", "mu.int", "sigma.int", "sigma")

# MCMC settings
ni <- 2000
nb <- 500
nt <- 2
nc <- 3

# Run the model in JAGS

out <- jags(win.data, inits, parameters, "lmm.txt", n.chains = nc,
            n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)

# Inspect results
print(out, dig = 3)

traceplot(out)
traceplot(out, param = c("mu.int", "sigma.int", "beta"))

print(out, 2)

# Compare with input values
intercept.mean ; slope.mean ; intercept.sd ; slope.sd ; sd(eps)

# Plot
par(mfrow = c(1,2))
plot(density(out$sims.list$mu.int), xlab="alpha.mean", ylab="Frequency", frame = F) 
abline(v = 230, col = "red", lwd = 3)
