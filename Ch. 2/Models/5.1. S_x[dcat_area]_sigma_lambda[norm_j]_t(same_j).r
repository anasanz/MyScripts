
rm(list=ls())

library(rjags)
library(jagsUI)

set.seed(2013)
# ---- Data simulation ----
#### Simulate abundance for one species; 8 years; one sigma; lambda site specific

#then generate distance sampling data from that   #####
#### and analyze with data-generating model; summarize results across iterations 

# Half-normal detection function
g <- function(x, sig) exp(-x^2/(2*sig^2))

nSites <- 50					# number of line transect surveys
strip.width <- 200 				# strip half-width, w (in this example only one side of the line transect is surveyed)
dist.breaks <- c(0,25,50,100,200)
int.w <- diff(dist.breaks) # width of distance categories (v)
midpt <- diff(dist.breaks)/2+dist.breaks[-5]
nG <- length(dist.breaks)-1	

# Year effect 
yrs <- 1:8 # eight years
nyrs <- length(yrs)


# ---- Detection component ----

sigma <- 55 # Same sigma in all transects
# Detection prob at farthest distance interval for sigma
g(4,3)


# ----  Abundance component with a site effect ----

# Mean abundance and sd across sites
mu.lam <- log(1.5)				
sig.lam <- 1				
##Site effect in lambda
log.lam <- rnorm(nSites, mu.lam, sig.lam) # Here I add it as a random effect, it could be as a predictor with covariates
lam <- exp(log.lam)                       # Also, I am assuming that lambda doesnt change by year
# Abundance per site and year
N <- matrix(nrow = nSites, ncol=nyrs)
for (t in 1:nyrs){
  N[ ,t] <- rpois(nSites,lam)
}

# Total number of individuals in all sampled transects per year
N.tot <- apply(N,2,sum)


# ---- Simulate continuous distance data ----

# Nc = count of individuals detected in each distance interval
y <- array(0, c(nSites, length(dist.breaks)-1))
yList <- list(y,y,y,y,y,y,y,y)

for (t in 1:nyrs){
  for(j in 1:nSites) {
    if(N[j,t] == 0)
    next
    # Distance from observer to the individual
    d <- runif(N[j,t], 0, strip.width) 		# Uniform distribution of animals
                                          # Simulates one distance for each individual in the site (N[j])
    p <- g(x=d, sig=sigma)   		# Detection probability
    seen <- rbinom(N[j,t], 1, p)
    if(all(seen == 0))
      next
    d1 <- d[seen==1] 				# The distance data for seen individuals
    counts <- table(cut(d1, dist.breaks, include.lowest=TRUE))
    yList[[t]][j,] <- counts 				# The number of detections in each distance interval
  }}

y.sum <- do.call(cbind,lapply(yList, function(x) rowSums(x))) # Total count per site each year


# ---- Convert data to JAGS format ----

nind <- sum(y.sum)

# Get one long vector with counts and sites
yLong <- unlist(as.data.frame(y.sum), use.names = F)
sites <- 1:50
sitesYears <- rep(sites,8)


# Get one long vector with years, distance category and site for counts
site <- dclass <- year <- NULL

for (t in 1:nyrs){
  for(j in 1:nSites){
  if (y.sum[j,t] == 0) next
    
    site <- c(site, rep(j, y.sum[j,t])) # site index: repeat the site as many times as counts in that site
                                    # vector of sites through years (disregarding distance class)
    year <- c(year, rep(t, y.sum[j,t]))
    for (k in 1:nG){
      if (yList[[t]][j,k] == 0) next # Refers for the ditance classes to the list with years and bins
      dclass <- c(dclass, rep(k, yList[[t]][j,k]))	# Distance category index
    }}
}
        # The indexes year and site dont need to be used for this model

# Create one matrix for indexing year when calculating abundance per year in JAGS

allyears <- NULL 
for (i in 1:nyrs){
  allyears <- c(allyears, rep(i, nSites))
}
m <- data.frame(allyears = allyears)
m$allyears <- as.factor(m$allyears)
indexYears <- model.matrix(~ allyears-1, data = m)

# ---- Compile data for JAGS model ----

data1 <- list(nsites=nSites, nG=nG, int.w=int.w, strip.width = strip.width, indexYears = indexYears,
              y = yLong, nind=nind, dclass=dclass, midpt = midpt, nyears = nyrs, sitesYears = sitesYears)

# ---- JAGS model ----

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/Model")
cat("model{
    # Priors

    mu.lam ~ dunif(-10, 10) # I allow it to have negative values because the log of lambda can have
    sig.lam ~ dunif(0, 10)
    sigma ~ dunif(0, 1000)
    tau.lam <- 1/(sig.lam*sig.lam)
    
    for(i in 1:nind){
    dclass[i] ~ dcat(fc[]) 
    }
    
    # Construct cell probabilities for nG multinomial cells (distance categories)
    for(k in 1:nG){ 
    log(p[k]) <- -midpt[k] * midpt[k] / (2*sigma*sigma)
    pi[k] <- int.w[k] / strip.width 
    f[k] <- p[k] * pi[k] 
    fc[k] <- f[k] / pcap 
    }
    pcap <- sum(f[]) # Same for all sites all years
    
    for(j in 1:(nsites*nyears)){ # I build site and year together because in my data there is different number of sites per year
    y[j] ~ dbin(pcap, N[j]) 
    N[j] ~ dpois(lambda[j]) 
    lambda[j] <- exp(log.lambda[sitesYears[j]]) 
    }
    
    #RANDOM TRANSECT LEVEL EFFECT (doesn't change over time) # takes care of the dependence in data when you repeatedly visit the same transect
    for (s in 1:nsites){
    log.lambda[s] ~ dnorm(mu.lam, tau.lam)
    }

    # Derived parameters
    for (i in 1:nyears){
    Ntotal[i] <- sum(N*indexYears[,i]) 
    }
    }",fill=TRUE, file = "s_sigma_lambda_norm_j_year.txt")

# Inits
Nst <- yLong + 1
inits <- function(){list(mu.lam = runif(1), sig.lam = 0.2, sigma = runif(1, 20, 100), N=Nst)}

# Params
params <- c("Ntotal", "N", "D", "sigma", "lambda", "mu.lam", "sig.lam")

# MCMC settings
nc <- 3 ; ni <- 100000 ; nb <- 2000 ; nt <- 2

# With jagsUI 
out <- jags(data1, inits, params, "s_sigma_lambda_norm_j_year.txt", n.chain = nc,
            n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
print(out)
traceplot(out)

for (i in 1:nyrs){
plot(density(out$sims.list$Ntotal[,i]), xlab="Population size", ylab="Frequency", 
     frame = F, main = paste("year",i)) 
abline(v = N.tot[i], col = "blue", lwd = 3)
abline(v = mean(out$sims.list$Ntotal[,i]), col = "red", lwd = 3)
}

plot(density(out$sims.list$sigma), xlab="Sigma", ylab="Frequency", frame = F) 
abline(v = sigma, col = "blue", lwd = 3) 
abline(v = mean(out$sims.list$sigma), col = "red", lwd = 3)



# With rjags
modelFile = "s_sigma_lambda_j.txt"
mod <- jags.model(modelFile, data1, inits, n.chain = nc, n.adapt = 500)
out <- coda.samples(mod, params, n.iter = 8000, thin=8)
summary(out)
samps.jags <- jags.samples(mod, params, ni, nt, n.burnin=nb)