rm(list=ls())

library(rjags)
library(jagsUI)
library(plyr)


set.seed(2013)
# ---- Data simulation ----
#### Simulate abundance for one species:
# THIS MODEL IS TO CALCULATE TRENDS AND THEN COMPARE IT WITH THE TRIM
# 8 years (unbalanced number of transects per year); lambda site specific(Zone variable)
# Sigma site-year specific (effect of zone cov(?) and random effect in observer)

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


#################################
# ---- Detection component ----

# RANDOM EFFECT IN OBSERVER
obs <- 1:9
nobs <- length(obs)
mu.sig.obs <- log(50)
sig.sig.obs <- 0.25
# Observer effect in sigma
sig.obs <- rnorm(length(obs), mu.sig.obs, sig.sig.obs) 
# Observer covariate

ob.id <- matrix(sample(1:9, max.sites*nyrs, replace = TRUE), nrow = max.sites, ncol = nyrs) # Matix with IDs

ob <- matrix(sig.obs[ob.id],  nrow = max.sites, ncol = nyrs) # Matrix with intercept for simulating data


#ZONE COVARIATE (SITE)
b.sig.zoneB <- 0.7
# Site specific binary co-variate
z <- data.frame(var = sample(c("A", "B"), max.sites, replace = TRUE))
z$var <- as.factor(z$var)
zone <- model.matrix(~ var-1, z)

#SIGMA
sigma <- exp(ob + matrix(b.sig.zoneB*zone[,2], nrow = max.sites, ncol = nyrs, byrow = FALSE) )# HERE IT SHOULD BE FALSE :o!


# ----  Abundance component: random effect accross sites, zone covariate and 2 area covariates

# RANDOM EFFECT IN SITE (INDEPENDENT OF THE YEAR)
# Mean abundance and sd across sites
mu.lam.alpha.site <- log(1.5)				
sig.lam.alpha.site <- 0.5				
##Site effect in lambda
lam.alpha.site <- rnorm(max.sites, mu.lam.alpha.site, sig.lam.alpha.site) 


#ZONE COVARIATE (SITE)
# Coefficient (I had created the co-variate already!So dont generate it twice!)
b.lam.zoneB <- -0.5

#TIME CO-VARIATE (YEAR)
b.lam.year <- 0.3
year <- matrix(NA,nrow = max.sites, ncol = nyrs)
colnames(year) <- yrs
for (i in 1:nyrs){
year[ ,yrs[i]] <- rep(yrs[i], max.sites)
}

lam <- exp(matrix(lam.alpha.site, nrow = max.sites, ncol = nyrs) + 
             matrix(b.lam.zoneB*zone[,2], nrow = max.sites, ncol = nyrs, byrow = F) + # By row has to be false for site covariates that dont change with year!
            b.lam.year*year) 
              

# Abundance per site and year
N <- list()

for (t in 1:nyrs){
  N[[t]] <- rpois(nSites[t],lam[1:nSites[t], t])
} 

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
    p <- g(x=d, sig=sigma[j,t])   		# Detection probability. Sigma is site-time specific
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

#############################################
# ---- Convert data to JAGS format ----

nind.year <- lapply(yList,sum)
nind <- sum(unlist(nind.year, use.names = F))

# Get one long vector with counts and sites
yLong.na <- unlist(as.data.frame(y.sum), use.names = F) # With NA included (useful if I ever make a model estimating abundance in sites with no information)
yLong <- yLong.na[complete.cases(yLong.na)]

sitesYears <- NULL
for (i in 1:nyrs){
  sitesYears <- c(sitesYears,c(1:nSites[i]))
}

# Create one long vector with covariate values

zB <- as.vector(zone[,2])
zoneB <- NULL
for (i in 1:nyrs){
  zoneB <- c(zoneB,zB[1:nSites[i]])
}

ob <- NULL
for (i in 1:nyrs){
  ob <- c(ob,ob.id[1:nSites[i], i])
}

year1 <- NULL
for (i in 1:nyrs){
  year1 <- c(year1,year[1:nSites[i], i])
}

# Get one long vector with years, distance category and site
site <- dclass <- year <- NULL

for (t in 1:nyrs){
  for(j in 1:max.sites){
    if (y.sum[j,t] == 0 | is.na(y.sum[j,t])) 
      next
    site <- c(site, rep(j, y.sum[j,t])) # site index: repeat the site as many times as counts in that site (for multi model??)
    
    # vector of sites through years (disregarding distance class)
    year <- c(year, rep(t, y.sum[j,t]))
    
    for (k in 1:nG){
      if (yList[[t]][j,k] == 0) # Refers for the ditance classes to the list with years and bins
        next 
      dclass <- c(dclass, rep(k, yList[[t]][j,k]))	# Distance category index
    }}
}

# Get one long vector for each site-year combination of each dclass observation
# (so, at which j, or siteyear is every observation or dclass corresponding?) 
# This new siteyear index is to refer to the prob. of detection at each site (now it is different, because sigma varies per site)

n.allSiteYear <- sum(nSites)
#seq.allSiteYear <- 1:length(dclass)
siteYear.dclass <- NULL

###RS: Fixed index to map dclass onto site-year combinations
for (i in 1:n.allSiteYear){
  siteYear.dclass <- c(siteYear.dclass,rep(i, yLong[i]))}


# Create one matrix for indexing year when calculating abundance per year in JAGS

allyears <- NULL 
for (i in 1:nyrs){
  allyears <- c(allyears,rep(yrs[i],nSites[i]))
}
m <- data.frame(allyears = allyears)
m$allyears <- as.factor(m$allyears)
indexYears <- model.matrix(~ allyears-1, data = m)


# ---- Compile data for JAGS model ----

data1 <- list(nyears = nyrs, max.sites = max.sites, nG=nG, siteYear.dclass = siteYear.dclass, int.w=int.w, strip.width = strip.width, 
              y = yLong, nind=nind, dclass=dclass, midpt = midpt, sitesYears = sitesYears, indexYears = indexYears,
              zoneB = zoneB, ob = ob, nobs = nobs, db = dist.breaks, year1 = year1)

# ---- JAGS model ----

setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/Model")
cat("model{
    
    # PRIORS
    
    # Priors for lambda
    bzB.lam ~ dnorm(0, 0.001)
    bYear.lam ~ dnorm(0, 0.001)
    
    mu.lam ~ dunif(-10, 10) # Random effects for lambda per site
    sig.lam ~ dunif(0, 10)
    tau.lam <- 1/(sig.lam*sig.lam)
    
    # Priors for sigma
    bzB.sig ~ dnorm(0, 0.001)
    
    mu.sig ~ dunif(-10, 10) # Random effects for sigma per observer
    sig.sig ~ dunif(0, 10)
    tau.sig <- 1/(sig.sig*sig.sig)
    
    #RANDOM TRANSECT LEVEL EFFECT FOR LAMBDA (doesn't change over time) # takes care of the dependence in data when you repeatedly visit the same transect
    for (s in 1:max.sites){
    log.lambda[s] ~ dnorm(mu.lam, tau.lam)
    }
    
    #RANDOM OBSERVER EFFECT FOR SIGMA 
    for (o in 1:nobs){
    sig.obs[o] ~ dnorm(mu.sig, tau.sig)
    }
    
    for(i in 1:nind){
    dclass[i] ~ dcat(fct[siteYear.dclass[i], 1:nG])  
    }
    
    for(j in 1:length(y)){ 
    
    sigma[j] <- exp(sig.obs[ob[j]] + bzB.sig*zoneB[j])
    
    # Construct cell probabilities for nG multinomial cells (distance categories) PER SITE
    
    for(k in 1:nG){ 
    
    up[j,k]<-pnorm(db[k+1], 0, 1/sigma[j]^2) ##db are distance bin limits
    low[j,k]<-pnorm(db[k], 0, 1/sigma[j]^2) 
    p[j,k]<- 2 * (up[j,k] - low[j,k])
    pi[j,k] <- int.w[k] / strip.width 
    f[j,k]<- p[j,k]/f.0[j]/int.w[k]                   ## detection prob. in distance category k                      
    fc[j,k]<- f[j,k] * pi[j,k]                 ## pi=percent area of k; drops out if constant
    fct[j,k]<-fc[j,k]/pcap[j] 
    }
    
    pcap[j] <- sum(fc[j, 1:nG]) # Different per site and year (sum over all bins)
    
    f.0[j] <- 2 * dnorm(0,0, 1/sigma[j]^2) # Prob density at 0
    # To set that prob.of detection at distance 0 is one, you divide by f0 in the loop up
    
    y[j] ~ dbin(pcap[j], N[j]) 
    N[j] ~ dpois(lambda[j]) 
    lambda[j] <- exp(log.lambda[sitesYears[j]] + bzB.lam*zoneB[j] + bYear.lam*year1[j]) 
    }
    
    # Derived parameters
    for (i in 1:nyears){
    Ntotal[i] <- sum(N*indexYears[,i]) 
    }
    }",fill=TRUE, file = "s_sigma(integral)[obs(o,j,t)_covZone(j)]_lambda[alpha(j)_covZone(j)_year(j)].txt")

# Inits
Nst <- yLong + 1
inits <- function(){list(mu.lam = runif(1), sig.lam = 0.2, #sigma = runif(624, 0, 50), I dont need sigma because I have already priors for his hyperparameters!!!!!
                         N=Nst,
                         bzB.lam = runif(1),
                         bYear.lam = runif(1),
                         mu.sig = runif(1, log(30), log(50)), sig.sig = runif(1), bzB.sig = runif(1)
                         ###changed inits for mu.sig - don't start too small, better start too large
)}

# Params
params <- c("Ntotal", #"N", "sigma", "lambda", I remove it so that it doesnt save the lambdas and takes shorter. It still calculates them
            "mu.lam", "sig.lam", 
            "bzB.lam", "bYear.lam",
            "mu.sig", "sig.sig", "bzB.sig"
)

# MCMC settings
nc <- 3 ; ni <- 15000 ; nb <- 2000 ; nt <- 2

# With jagsUI 
out <- jags(data1, inits, params, "s_sigma(integral)[obs(o,j,t)_covZone(j)]_lambda[alpha(j)_covZone(j)_year(j)].txt", n.chain = nc,
            n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
print(out)

summary <- as.data.frame(as.matrix(out$summary))

# To compare:
data_comp <- list(N.tot = N.tot, b.lam.zoneB = b.lam.zoneB,
                  mu.lam.alpha.site = mu.lam.alpha.site,
                  sig.lam.alpha.site = sig.lam.alpha.site,
                  b.sig.zoneB = b.sig.zoneB, mu.sig.obs = mu.sig.obs,
                  b.lam.year = b.lam.year,
                  sig.sig.obs = sig.sig.obs)



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

###########################################################################################


