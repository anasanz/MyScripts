
#### Example Butterfly Binomial N-mixture model Zero Inflated ####

setwd("D:/PhD/Stats/BPA course")

# Get the data and put them into 3D array
bdat <- read.table("fritillary.txt", header = TRUE)
y <- array(NA, dim = c(95, 2, 7)) # 95 sites, 2 reps, 7 days

for(k in 1:7){
  sel.rows <- bdat$day == k
  y[,,k] <- as.matrix(bdat)[sel.rows, 3:4]
}

y # Look at data set in 3D layout
str(y)
# Have a look at raw data
day.max <- apply(y, c(1, 3), max, na.rm = TRUE) # Max count each site and day
day.max
site.max <- apply(day.max, 1, max, na.rm = TRUE) # Max count each site
site.max
table(site.max) # Frequency distribution of max counts
plot(table(site.max))
table(site.max>0) # Observed occupancy is only 56%
# Sum of observed max as conventional estimator of total abundance
max1 <- apply(y, c(1, 3), max)
obs.max.sum <- apply(max1, 2, sum, na.rm = TRUE)
obs.max.sum

# Bundle data
R = nrow(y)
T = ncol(y)
win.data <- list(y = y, R = R, T = T)

# Specify model in BUGS language
sink("Nmix1.jags")
cat("
    model {
    
    # Priors
    omega ~ dunif(0, 1)
    
    for (k in 1:6){                       # k = years (year ranfom effect)
    alpha.lam[k] ~ dnorm(0, 0.01)
    p[k] ~ dunif(0, 1)
    }
    
    # Likelihood
    # Ecological model for true abundance

    for (i in 1:R){                          # Loop over R sites (95): i = site
      z[i] ~ dbern(omega)                   # Latent suitability state
      
      for (k in 1:7){                       # Loop over survey periods (seasons)
      N[i,k] ~ dpois(lam.eff[i,k])          # Latent abundance state
      lam.eff[i,k] <- z[i] * lambda[i,k]
      log(lambda[i,k]) <- alpha.lam[k]
      
        # Observation model for replicated counts
        
        for (j in 1:T){                    # Loop over temporal reps (2): j = temporal replicates
          y[i,j,k] ~ dbin(p[k], N[i,k])   # Detection
        
        # Assess model fit using Chi-squared discrepancy
        # Compute fit statistic for observed data
        eval[i,j,k] <- p[k] * N[i,k]
        E[i,j,k] <- pow((y[i,j,k] - eval[i,j,k]),2) / (eval[i,j,k] + 0.5)
        # Generate replicate data and compute fit stats for them
        y.new[i,j,k] ~ dbin(p[k], N[i,k])
        E.new[i,j,k] <- pow((y.new[i,j,k] - eval[i,j,k]),2) / (eval[i,j,k]+0.5)
        } #j
        } #k
        } #i
    
    # Derived and other quantities
    
    for (k in 1:7){
    totalN[k] <- sum(N[,k])	# Estimate total pop. size across all sites
    mean.abundance[k] <- exp(alpha.lam[k])
    }

    fit <- sum(E[,,])
    fit.new <- sum(E.new[,,])
    }
    ",fill = TRUE)
sink()


# Initial values
# It is advisable to give initial values for the latent state z, the best option is to provide a vector of 1
Nst <- apply(y, c(1, 3), max) + 1
Nst[is.na(Nst)] <- 1
inits <- function(){list(N = Nst, alpha.lam = runif(7, -1, 1), z = rep(1, 95))}

# Parameters monitored
params <- c("omega", "totalN", "alpha.lam", "p", "mean.abundance", "fit", "fit.new")

# MCMC settings
ni <- 30000
nt <- 15
nb <- 15000
nc <- 3

# Call JAGS from R (BRT 3 min)
out1 <- jags(win.data, inits, params, "Nmix1.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, working.directory = getwd())

# Summarize posteriors
print(out1, dig = 3)

# Evaluation of fit
plot(out1$BUGSoutput$sims.list$fit, out1$BUGSoutput$sims.list$fit.new, main = "", xlab = "Discrepancy actual data", ylab = "Discrepancy replicate data", frame.plot = FALSE)
abline(0, 1, lwd = 2, col = "black")
mean(out1$BUGSoutput$sims.list$fit.new > out1$BUGSoutput$sims.list$fit)
mean(out1$BUGSoutput$mean$fit) / mean(out1$BUGSoutput$mean$fit.new)


