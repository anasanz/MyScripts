rm(list=ls())

library(rjags)
library(jagsUI)
library(plyr)

# ---- Data simulation ----
#### Simulate abundance for one species:
#8 years (unbalanced number of transects per year); lambda site specific; one sigma;
# Zone variable and 2 areas variable
# Half-normal detection function
g <- function(x, sig) exp(-x^2/(2*sig^2))

# Number of transects per year (unbalanced)
nSites <- seq(50,106, by = 8)				# number of line transect surveys (DIFFERENT BY YEAR)
max.sites <- max(nSites)            # Maximun number of sites is the last year

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


# ----  Abundance component: random effect accross sites, zone covariate and 2 area covariates

# RANDOM EFFECT IN SITE (INDEPENDENT OF THE YEAR)
# Mean abundance and sd across sites
mu.lam.alpha.site <- log(1.5)				
sig.lam.alpha.site <- 1				
##Site effect in lambda
lam.alpha.site <- rnorm(max.sites, mu.lam.alpha.site, sig.lam.alpha.site) 
                       

#ZONE COVARIATE (SITE)
#b.zoneA <- rnorm(1,0,0.05) #I wont include it (its in the intercept)
b.zoneB <- rnorm(1,0,0.05)
# Site specific binary co-variate
z <- data.frame(var = sample(c("A", "B"), max.sites, replace = TRUE))
z$var <- as.factor(z$var)
zone <- model.matrix(~ var-1, z)



#AREA COVARIATE (SITE AND YEAR)
#Coefficients
b.a1 <- rnorm(1,0,0.05)
b.a2 <- rnorm(1,0,0.05)
#Covariates
a1 <- abs(rnorm(max.sites*nyrs, 10, 5)) # Although it makes sense to make them positive, it wouldnt matter (you put them on the exp)
a2 <- abs(rnorm(max.sites*nyrs, 5, 2.5))



matrix(a1,nrow = max.sites, ncol = nyrs)


lam <- exp(matrix(lam.alpha.site, nrow = max.sites, ncol = nyrs) + 
             matrix(b.zoneB*zone[,2], nrow = max.sites, ncol = nyrs, byrow=T) + 
             matrix(b.a1*a1, nrow = max.sites, ncol = nyrs, byrow=T) +
             matrix(b.a2*a2, nrow = max.sites, ncol = nyrs, byrow=T) )



# Abundance per site and year
N <- list()

for (t in 1:nyrs){
  N[[t]] <- rpois(nSites[t],lam[1:length(nSites[t]), t])
} # Here we can have all the sites because its the real abundance (even if we haven't sampled them??)

NLong <- ldply(N,cbind) # 1 long vector with all abundances per site and year
N3 <- ldply(N,rbind)
N.sitesYears <- t(N3) # N per site and year stored in a matrix with columns

# Total number of individuals in all sampled transects per year
N.tot <- lapply(N,sum)


# ---- Simulate continuous distance data ----

# Nc = count of individuals detected in each distance interval
yList <- list()
for (i in 1:nyrs){
  yList[[i]] <- array(0, c(nSites[i], length(dist.breaks)-1))
}


for (t in 1:nyrs){
  for(j in 1:max.sites) {
    if(N.sitesYears[j,t] == 0 | is.na(N.sitesYears[j,t]))
      next
    # Distance from observer to the individual
    d <- runif(N.sitesYears[j,t], 0, strip.width) 		# Uniform distribution of animals
    # Simulates one distance for each individual in the site (N[j])
    p <- g(x=d, sig=sigma)   		# Detection probability
    seen <- rbinom(N.sitesYears[j,t], 1, p)
    if(all(seen == 0))
      next
    d1 <- d[seen==1] 				# The distance data for seen individuals
    counts <- table(cut(d1, dist.breaks, include.lowest=TRUE))
    yList[[t]][j,] <- counts 				# The number of detections in each distance interval
  }}

y.sum.sites <- lapply(yList, function(x) rowSums(x)) # Total count per site each year
y.sum.sites2 <- ldply(y.sum.sites,rbind)
y.sum <- t(y.sum.sites2) # y per site and year stored in a matrix with columns

# ---- Convert data to JAGS format ----

nind.year <- lapply(yList,sum)
nind <- sum(unlist(nind.year, use.names = F))

# Get one long vector with counts and sites
yLong.na <- unlist(as.data.frame(y.sum), use.names = F)
yLong <- yLong.na[complete.cases(yLong.na)]

sitesYears <- NULL
for (i in 1:nyrs){
  sitesYears <- c(sitesYears,c(1:nSites[i]))
}

# Create one long vector with covariate values
area1 <- NULL
for (i in 1:nyrs){
  area1 <- c(area1,a1[1:nSites[i]])
}

area2 <- NULL
for (i in 1:nyrs){
  area2 <- c(area2,a2[1:nSites[i]])
}

#zA <- as.vector(zone[,1])
#zoneA <- NULL
#for (i in 1:nyrs){
#  zoneA <- c(zoneA,zA[1:nSites[i]])
#}

zB <- as.vector(zone[,2])
zoneB <- NULL
for (i in 1:nyrs){
  zoneB <- c(zoneB,zB[1:nSites[i]])
}


# Get one long vector with years, distance category and site
site <- dclass <- year <- NULL

for (t in 1:nyrs){
  for(j in 1:max.sites){
    if (y.sum[j,t] == 0 | is.na(y.sum[j,t])) 
      next
    site <- c(site, rep(j, y.sum[j,t])) # site index: repeat the site as many times as counts in that site
    # vector of sites through years (disregarding distance class)
    year <- c(year, rep(t, y.sum[j,t]))
    for (k in 1:nG){
      if (yList[[t]][j,k] == 0) # Refers for the ditance classes to the list with years and bins
        next 
      dclass <- c(dclass, rep(k, yList[[t]][j,k]))	# Distance category index
    }}
}

# Create one matrix for indexing year when calculating abundance per year in JAGS

allyears <- NULL 
for (i in 1:nyrs){
  allyears <- c(allyears,rep(yrs[i],nSites[i]))
}
m <- data.frame(allyears = allyears)
m$allyears <- as.factor(m$allyears)
indexYears <- model.matrix(~ allyears-1, data = m)


# ---- Compile data for JAGS model ----

data1 <- list(nyears = nyrs, max.sites = max.sites, nG=nG, int.w=int.w, strip.width = strip.width, 
              y = yLong, nind=nind, dclass=dclass, midpt = midpt, sitesYears = sitesYears, indexYears = indexYears,
              area1 = area1, area2 = area2, zoneB = zoneB)

# ---- JAGS model ----

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/Model")
cat("model{
    # Priors
    
    # Coefficients for lambda
    bzB.lam ~ dnorm(0, 0.001)
    ba1.lam ~ dnorm(0, 0.001)
    ba2.lam ~  dnorm(0, 0.001)

    
    mu.lam ~ dunif(-10, 10) # Random effects for lambda per site
                            # I allow it to have negative values because the log of lambda can have
    sig.lam ~ dunif(0, 10)
    tau.lam <- 1/(sig.lam*sig.lam)
    sigma ~ dunif(0, 1000)

    #RANDOM TRANSECT LEVEL EFFECT FOR LAMBDA (doesn't change over time) # takes care of the dependence in data when you repeatedly visit the same transect
    for (s in 1:max.sites){
    log.lambda[s] ~ dnorm(mu.lam, tau.lam)
    }
    
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
    
    for(j in 1:length(y)){ # sites*years. Because in my data there is different number of sites per year
    y[j] ~ dbin(pcap, N[j]) 
    N[j] ~ dpois(lambda[j]) 
    lambda[j] <- exp(log.lambda[sitesYears[j]]  
                      + bzB.lam*zoneB[j]
                      + ba1.lam*area1[j] + ba2.lam*area2[j]) 
    }
    
    # Derived parameters
    for (i in 1:nyears){
    Ntotal[i] <- sum(N*indexYears[,i]) 
    }
    }",fill=TRUE, file = "s_sigma_lambda[alpha(j)_covZones(j)_covAreas(jt)].txt")

# Inits
Nst <- yLong + 1
inits <- function(){list(mu.lam = runif(1), sig.lam = 0.2, sigma = runif(1, 20, 100), N=Nst,
                         bzB.lam = runif(1), ba1.lam = runif(1), ba2.lam = runif(1) )}

# Params
params <- c("Ntotal", "N", "sigma", "lambda", "mu.lam", "sig.lam", 
            "bzB.lam", "ba1.lam", "ba2.lam")

# MCMC settings
nc <- 3 ; ni <- 15000 ; nb <- 2000 ; nt <- 2

# With jagsUI 
out <- jags(data1, inits, params, "s_sigma_lambda[alpha(j)_covZones(j)_covAreas(jt)].txt", n.chain = nc,
            n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
print(out)
out$mean # Doesnt retreive estimates and bad pop.estimates
summary <- as.data.frame(as.matrix(out$summary)) # Doesnt converge in zones and mu.lambda
par(mfrow = c(1,1))
traceplot(out, parameters = c("bzB.lam", "mu.lam")) # Chains didnt converge

for (i in 1:nyrs){
  plot(density(out$sims.list$Ntotal[,i]), xlab="Population size", ylab="Frequency", 
       frame = F, main = paste("year",i)) 
  abline(v = N.tot[i], col = "blue", lwd = 3)
  abline(v = mean(out$sims.list$Ntotal[,i]), col = "red", lwd = 3)
}

plot(density(out$sims.list$sigma), xlab="Sigma", ylab="Frequency", frame = F) 
abline(v = sigma, col = "blue", lwd = 3) 
abline(v = mean(out$sims.list$sigma), col = "red", lwd = 3)

density(out$sims.list$sigma)


# With rjags
modelFile = "s_sigma_lambda_j.txt"
mod <- jags.model(modelFile, data1, inits, n.chain = nc, n.adapt = 500)
out <- coda.samples(mod, params, n.iter = 8000, thin=8)
summary(out)
samps.jags <- jags.samples(mod, params, ni, nt, n.burnin=nb)