rm(list=ls())

library(rjags)
library(jagsUI)
library(plyr)

#set.seed(2013)

# MODEL 12
# Adding species random effects in coefficients from variables of model 11 (de momento solo probar 1, el de area 1)
# IT DOESN'T WORK (The problem is in b from the HR detection function, corrected from RE4_10.1.7 or 13)

# ---- Data simulation ----

# Same number of species: 48
# 5 years (unbalanced number of transects per year) 

# Observation model calculated with Hazard Rate detection function (to make b differ per species as well)
# Sigma site-year specific
### Random sp intercept 
### Year random effect
### Random effect in observer (site-year)
### Temperature co-variate
# sigma = exp(alpha(s) + observer(j,t) + year(t) + b(s)*Temp(j,t))

# Lambda site-year specific
### Random sp-year intercept (include different baseline abundance per species and also per year)
### sp-Site effect independent of year
### 3 areas variables
### Crop diversity variable
### Field size variable
# lambda = exp(alpha(s,t) + sp.site(s,j) + b1(s)*fallowSG(j,t) + b2(s)*fallowAES(j,t) + b3(s)*fallowGREEN(j,t) + b4(s)*Cropdiv(j,t) + b5(s)*Fieldsize(j,t))

# Detection function
g <- function(x, sig, b) 1 - exp(-(x/sig)^-b)

# Number of transects per year (unbalanced)
nSites <- seq(74,106, by = 8)				# number of line transect surveys (DIFFERENT BY YEAR)
max.sites <- max(nSites)            # Maximun number of sites is the last year
total.sites <- sum(nSites)          # Total number of site-year combinations

strip.width <- 500 				# strip half-width, w (in this example only one side of the line transect is surveyed)
dist.breaks <- c(0,25,50,100,200,500)
int.w <- diff(dist.breaks) # width of distance categories (v)
midpt <- (int.w/2) + dist.breaks[-6]

nG <- length(dist.breaks)-1	

# Year effect 
yrs <- 1:5 # five years
nyrs <- length(yrs)

# Number of species
nSpecies <- 48


#################################
# ---- Detection component ----

# RANDOM INTERCEPT PER SPECIES

mu.sig.sp <- log(50)					# Mean of species-level random effect on intercept of sigma
sig.sig.sp <- 0.25				# SD of species-level random effect on intercept of sigma
s.alpha <- rnorm(nSpecies, mu.sig.sp, sig.sig.sp)

# Look at distribution of sigma intercepts (to see if I chose reasonable)
hist(exp(rnorm(1000, mu.sig.sp, sig.sig.sp)))



# RANDOM EFFECT IN OBSERVER
obs <- 1:9
nobs <- length(obs)
sig.sig.obs <- 0.25

# Observer effect in sigma
sig.obs <- rnorm(length(obs), 0, sig.sig.obs) # Mean is 0 because is adding noise around the mean

# Observer covariate
ob.id <- matrix(sample(1:9, max.sites*nyrs, replace = TRUE), nrow = max.sites, ncol = nyrs) # Matix with IDs
ob <- matrix(sig.obs[ob.id],  nrow = max.sites, ncol = nyrs) # Matrix with intercept for simulating data

# YEAR EFFECT IN SIGMA (RANDOM)
sig.sig.year <- 1		
sig.year <- rnorm(nyrs, 0, sig.sig.year)
ye <- matrix(sig.year, nrow = max.sites, ncol = nyrs, byrow = TRUE)

# TEMPERATURE COVARIATE
bTemp.sig <- 0.5
temp <- matrix(rnorm(max.sites*nyrs, 30, 7), nrow = max.sites, ncol = nyrs)
#SCALED
temp_mean <- mean(temp)
temp_sd <- sd(temp)
temp_sc <- (temp - temp_mean) / temp_sd

#SIGMA: sigma[j,t,s]
sigma <- exp(array(rep(s.alpha, each = max.sites*nyrs), c(max.sites, nyrs, nSpecies)) 
             + replicate(nSpecies,ob) 
             + replicate(nSpecies,ye) 
             + array(bTemp.sig*temp_sc, c(max.sites,nyrs,nSpecies)) )

#BETA
mu.b <- 1.5
sig.b <- 0.5
beta <- rnorm(nSpecies, mu.b, sig.b)


# ----  Abundance component: random effect accross sites, zone covariate and 2 area covariates

# RANDOM INTERCEPT PER SPECIES-YEAR
# Mean abundance and sd per species and year
mu.lam.alpha.spyear <- log(1.5)
sig.lam.alpha.spyear <- 0.5
# Intercept
lam.alpha.spyear <- rnorm(nyrs*nSpecies, mu.lam.alpha.spyear, sig.lam.alpha.spyear) 

lam.alpha.spyear_data <- array(rep(lam.alpha.spyear, each = max.sites), c(max.sites, nyrs, nSpecies))

# RANDOM EFFECT IN SPECIES-SITE (Independent of year)
sig.lam.spsite <- 0.3
lam.spsite <- rnorm(nSpecies*max.sites, 0, sig.lam.spsite) 
ar <- array(rep(lam.spsite, each = nyrs), c(nyrs, max.sites, nSpecies)) # I need to make it in wide format and transpose it to long
ar1 <- list() 
for (i in 1:nSpecies){
  df <- as.data.frame(ar[,,i])
  df <- t(df)
  df <- as.matrix(df)
  ar1[[i]] <- df
}

lam.spsite_data <- array(as.numeric(unlist(ar1)), c(max.sites, nyrs, nSpecies))


#AREA COVARIATE (SITE AND YEAR)
#Coefficients
# Area 1
mu.a1 <- 0
sig.a1 <- 0.5
b.a1 <- rnorm(nSpecies, mu.a1, sig.a1)

b.a2 <- 1.2
b.a3 <- -0.5

#Covariates
a1 <- abs(rnorm(max.sites*nyrs, 10, 5)) # Although it makes sense to make them positive, it wouldnt matter (you put them on the exp)
a2 <- abs(rnorm(max.sites*nyrs, 5, 2.5))
a3 <- abs(rnorm(max.sites*nyrs, 2, 1))

#SCALED
area1_mean <- mean(a1)
area1_sd <- sd(a1)
area1_sc <- (a1 - area1_mean) / area1_sd

area2_mean <- mean(a2)
area2_sd <- sd(a2)
area2_sc <- (a2 - area2_mean) / area2_sd

area3_mean <- mean(a3)
area3_sd <- sd(a3)
area3_sc <- (a3 - area3_mean) / area3_sd


# COVARIATES CROP DIVERSITY AND FIELD SIZE
# Coefficients
bCropdiv <- 1.5
bFieldsize <- -0.9

#   Covariates
crop_diversity <- round(abs(rnorm(max.sites*nyrs, 7, 3)),0)
field_size <- abs(rnorm(max.sites*nyrs, 2, 0.5))

#SCALED
cropdiv_mean <- mean(crop_diversity)
cropdiv_sd <- sd(crop_diversity)
cropdiv_sc <- (crop_diversity - cropdiv_mean) / cropdiv_sd

fieldsize_mean <- mean(field_size)
fieldsize_sd <- sd(field_size)
fieldsize_sc <- (field_size - fieldsize_mean) / fieldsize_sd

#ARRANGED INTO AN ARRAY


lam <- exp(lam.alpha.spyear_data + 
             lam.spsite_data + 
             array(rep(b.a1,each=max.sites*nyrs)*area1_sc, c(max.sites, nyrs, nSpecies)) +
             array(b.a2*area2_sc, c(max.sites, nyrs, nSpecies)) +
             array(b.a3*area3_sc, c(max.sites, nyrs, nSpecies)) +
             array(bCropdiv*cropdiv_sc, c(max.sites, nyrs, nSpecies)) +
             array(bFieldsize*fieldsize_sc, c(max.sites, nyrs, nSpecies)) ) # I had to multiply the coefficients INSIDE lambda (otherwise it doesn't retrieve the a1 and a2 estimates)


# Abundance per site year (different abundance per species): N.sysp[j,t,s] 
N <- list()
N.sysp <- list() 

for (s in 1:nSpecies){
  for (t in 1:nyrs){
    N[[t]] <- rpois(nSites[t],lam[1:nSites[t], t, s]) 
  } 
  NLong <- ldply(N,cbind) # 1 long vector with all abundances per site and year
  N3 <- ldply(N,rbind)
  N.sitesYears <- t(N3) # N per site and year stored in a matrix with columns
  N.sysp[[s]] <- N.sitesYears
}

N.sysp <- array(as.numeric(unlist(N.sysp)), c(max.sites,nyrs,nSpecies))

# Total number of individuals in all sampled transects per year
N.tot <- lapply(N,sum) # I only need this list for now, is the same abundance


# ---- Simulate continuous distance data ----


# Create list to store the counts:
yList <- list()

for (i in 1:nyrs){
  yList[[i]] <- array(0, c(nSites[i], nG, nSpecies)) }


# EXPLANATION DIMENSIONS: yList[[t]][[j,k,s]]
yList[[1]][,,1] # This is the counts for the year 1 for species 1
yList[[2]][,3,1] # This is the counts for the year 1 for species 1 in bin 3
N.sysp[,,1] # This is the real number of individuals per stite and year of species 1
sigma[,,1] # And this is sigma per site and year for species 1

for (s in 1:nSpecies)
  for (t in 1:nyrs){
    for(j in 1:max.sites) {
      if(N.sysp[j,t,s] == 0 | is.na(N.sysp[j,t,s]))
        next
      # Distance from observer to the individual
      d <- runif(N.sysp[j,t,s], 0, strip.width) 		# Uniform distribution of animals
      # Simulates one distance for each individual in the site (N[j])
      p <- g(x=d, sig=sigma[j,t,s], b = beta[s])   		# Detection probability. Sigma is site-time specific
      seen <- rbinom(N.sysp[j,t,s], 1, p)
      if(all(seen == 0))
        next
      d1 <- d[seen==1] 				# The distance data for seen individuals
      counts <- table(cut(d1, dist.breaks, include.lowest=TRUE))
      yList[[t]][j,,s] <- counts 				# The number of detections in each distance interval per year and species
    }}

y.sum.sysp <- list()
y.sum.sites <- list()

for (t in 1:nyrs){
  
  for(s in 1:nSpecies){
    y.sum.sysp[[s]] <- rowSums(yList[[t]][,,s]) # List with counts per site in a given year t. Each element is one species
  }
  y.sum.sites[[t]] <- y.sum.sysp # Stored by years (y.sum.sites[[t]][[s]]): 8 elements with 15 subelements each
}

# Check things in loop because I dont want to do a 3diiimensional mistake
rowSums(yList[[2]][,,2])
h <- yList[[2]]
class(h)
rowSums(h[,,1])

# Arrange it by species (so that each element of the list is a species)
y.sum.sites.sp <- list()
for (s in 1:nSpecies){
  y.sum.sites.sp[[s]] <- sapply(y.sum.sites, function(x) x[s]) }

# From here I do the same than in script 8.1 but applied to the list of lists
y.sum <- list()

for (s in 1:nSpecies){
  store <- unlist(y.sum.sites.sp[s], recursive = F) # Convert it into single list to use ldply later
  y.sum.sites2 <- ldply(store,rbind) # Put all together (in rows) 
  y.sum[[s]] <- t(y.sum.sites2)} 

# y.sum is a list of species counts.
# Contains y per site and year stored in a matrix with columns.


#############################################

# ---- Convert data to JAGS format ----

nind.sp <- list()
for (s in 1:nSpecies){
  nind.year.sp <- lapply(y.sum.sites.sp[[s]],sum)
  nind.sp[[s]] <- sum(unlist(nind.year.sp, use.names = F)) # Just to know, but jags only wants the sum
}
nind <- do.call(sum, nind.sp)


# Get one long matrix with counts and sites per species (columns)
yLong.sp <- matrix(NA, nrow = total.sites, ncol = nSpecies)
for (s in 1:nSpecies){
  yLong.na <- unlist(as.data.frame(y.sum[[s]]), use.names = F) # With NA included (useful if I ever make a model estimating abundance in sites with no information)
  yLong.sp[,s] <- yLong.na[complete.cases(yLong.na)]
}

#sitesYears.sp <- matrix(NA, nrow = total.sites, ncol = nSpecies)
#sitesYears <- NULL
#for (s in 1:nSpecies){
#  for (i in 1:nyrs){
#    sitesYears <- c(sitesYears,c(1:nSites[i]))} 
#sitesYears.sp[,s] <- sitesYears
#sitesYears <- NULL # Set the vector empty to use it again
#  }

# All this index and variables are site-speficic (not species specific) so they stay like this
sitesYears <- NULL # I did that loop but the normal version actually works, since is an index per site-year
for (i in 1:nyrs){
  sitesYears <- c(sitesYears,c(1:nSites[i]))
}

# Create one long vector with covariate values
a1.m <- matrix(area1_sc, nrow = max.sites, ncol = nyrs, byrow = F) # I need to make it from the same matrix
a2.m <- matrix(area2_sc, nrow = max.sites, ncol = nyrs, byrow = F)# from which I created lambda, to make it fit!
a3.m <- matrix(area3_sc, nrow = max.sites, ncol = nyrs, byrow = F)# from which I created lambda, to make it fit!
cropdiv.m <- matrix(cropdiv_sc, nrow = max.sites, ncol = nyrs, byrow = F)# from which I created lambda, to make it fit!
fieldsize.m <- matrix(fieldsize_sc, nrow = max.sites, ncol = nyrs, byrow = F)# from which I created lambda, to make it fit!


area1 <- NULL
for (i in 1:nyrs){
  area1 <- c(area1,a1.m[1:nSites[i],i])
}

area2 <- NULL
for (i in 1:nyrs){
  area2 <- c(area2,a2.m[1:nSites[i],i])
}

area3 <- NULL
for (i in 1:nyrs){
  area3 <- c(area3,a3.m[1:nSites[i],i])
}

cdiv <- NULL
for (i in 1:nyrs){
  cdiv <- c(cdiv, cropdiv.m[1:nSites[i],i])
}

fsiz <- NULL
for (i in 1:nyrs){
  fsiz <- c(fsiz, fieldsize.m[1:nSites[i],i])
}

temperature_sc <- NULL
for (i in 1:nyrs){
  temperature_sc <- c(temperature_sc,temp_sc[1:nSites[i], i])
}

year_df <- matrix(yrs, nrow = max.sites, ncol = nyrs, byrow = TRUE) # Matix with IDs
year_index <- NULL
for (i in 1:nyrs){
  year_index <- c(year_index,year_df[1:nSites[i], i])
}

ob <- NULL
for (i in 1:nyrs){
  ob <- c(ob,ob.id[1:nSites[i], i])
}

# Get one long vector with years, distance category and site
site <- dclass <- year <- NULL

for (s in 1:nSpecies){
  for (t in 1:nyrs){
    for(j in 1:max.sites){
      if (y.sum[[s]][j,t] == 0 | is.na(y.sum[[s]][j,t])) 
        next
      site <- c(site, rep(j, y.sum[[s]][j,t])) # site index: repeat the site as many times as counts in that site (for multi model??)
      
      # vector of sites through years (disregarding distance class)
      year <- c(year, rep(t, y.sum[[s]][j,t]))
      
      for (k in 1:nG){
        if (yList[[t]][j,k,s] == 0) # Refers for the ditance classes to the list with years and bins
          next 
        dclass <- c(dclass, rep(k, yList[[t]][j,k,s]))}	# Distance category index
    }}}

# Get one long vector for each site-year combination of each dclass observation
# (so, at which j, or siteyear is every observation or dclass corresponding?)

n.allSiteYear <- sum(nSites)
siteYear.dclass <- NULL

###RS: Fixed index to map dclass onto site-year combinations (all species together)

for (s in 1:nSpecies){
  for (i in 1:n.allSiteYear){
    siteYear.dclass <- c(siteYear.dclass,rep(i, yLong.sp[i,s]))}
}

# Fixed index to map dclass in species (so that it matches with the dimensions (s,j,K))

sp.dclass <- NULL
for (s in 1:nSpecies){
  for (i in 1:n.allSiteYear){
    sp.dclass <- c(sp.dclass,rep(s, yLong.sp[i,s]))}
}

# Create one matrix for indexing year when calculating abundance per year in JAGS (works for all species)

allyears <- NULL 
for (i in 1:nyrs){
  allyears <- c(allyears,rep(yrs[i],nSites[i]))
}
m <- data.frame(allyears = allyears)
m$allyears <- as.factor(m$allyears)
indexYears <- model.matrix(~ allyears-1, data = m)


# ---- Compile data for JAGS model ----

data1 <- list(nyears = nyrs, max.sites = max.sites, nG=nG, siteYear.dclass = siteYear.dclass, int.w=int.w, strip.width = strip.width, midpt = midpt, db = dist.breaks, year_index = year_index,
              y = yLong.sp, n.allSiteYear = n.allSiteYear, nind=nind, dclass=dclass, sitesYears = sitesYears, indexYears = indexYears, allyears = allyears,
              area1 = area1, area2 = area2, area3 = area3, cdiv = cdiv, fsiz = fsiz, ob = ob, nobs = nobs, temperature_sc = temperature_sc,
              nSpecies = nSpecies, sp.dclass = sp.dclass, nyrs = nyrs)

# ---- JAGS model ----

setwd("C:/Users/ana.sanz/Documents/PhD/Second chapter/Data/Model")
cat("model{
    
    # PRIORS
    
    # SPECIES SPECIFIC PARAMETERS (random effects)
    
    for (s in 1:nSpecies){              # Random intercept for sigma (dif detection per species)
    asig[s] ~ dnorm(mu_s, tau_s)
    b.a1[s] ~ dnorm(mu_a1, tau_a1)}
    
    for (s in 1:nSpecies){              # Random beta per species (dif shape of detection curve per species)
    beta[s] ~ dnorm(mu_b, tau_b)}
    
    for(s in 1:nSpecies){              # Random intercept for lambda (dif abundance per species and year)
    for(t in 1:nyrs){
    alam[s,t] ~ dnorm(mu_l,tau_l)}}
    
    for (s in 1:nSpecies){             # Random effect for lambda (dif abundance per species and site)
    for (i in 1:max.sites){
    spsite[s,i] ~ dnorm(0, tau_spsite) }}    
    
    # Hyperparameters of species level random effects
    
    mu_s ~ dnorm(0,0.01) # Hyperparameters for sigma intercept
    tau_s <- 1/(sig_s*sig_s)
    sig_s ~ dunif(0,500)
    
    mu_b ~ dnorm(0,0.01) # Hyperparameters for beta
    tau_b <- 1/(sig_b*sig_b)
    sig_b ~ dunif(0,500)
    
    mu_l ~ dnorm(0,0.01) # Hyperparameters for lambda intercept
    tau_l <- 1/(sig_l*sig_l)
    sig_l ~ dunif(0,500)
    
    tau_spsite <- 1/(sig_spsite*sig_spsite) # Hyperparameter for site random effect in lambda
    sig_spsite ~ dunif(0,500)

    mu_a1 ~ dnorm(0,0.01) # Hyperparameters for beta coefficient area1
    tau_a1 <- 1/(sig_a1*sig_a1)
    sig_a1 ~ dunif(0,500)
    
    
    # PRIORS FOR LAMBDA
    ba2.lam ~  dnorm(0, 0.001)
    ba3.lam ~  dnorm(0, 0.001)
    b.cdiv.lam ~  dnorm(0, 0.001)
    b.fsiz.lam ~  dnorm(0, 0.001)
    
    
    # PRIORS FOR SIGMA
    
    bTemp ~ dnorm(0, 0.001)
    
    sig.sig.ob ~ dunif(0, 10) # Random effects for sigma per observer
    tau.sig.ob <- 1/(sig.sig.ob*sig.sig.ob)
    
    sig.sig.year ~ dunif(0, 10) # Random effects for sigma per observer
    tau.sig.year <- 1/(sig.sig.year*sig.sig.year)
    
    #Random observer effect for sigma
    
    for (o in 1:nobs){
    sig.obs[o] ~ dnorm(0, tau.sig.ob)
    }
    
    #Random year effect for sigma
    
    for (t in 1:nyrs){
    sig.year[t] ~ dnorm(0, tau.sig.year)
    }
    
    for(i in 1:nind){
    dclass[i] ~ dcat(fct[sp.dclass[i],siteYear.dclass[i], 1:nG])  
    }
    
    for (s in 1:nSpecies){
    
    for(j in 1:n.allSiteYear){ 
    
    sigma[s,j] <- exp(asig[s] + sig.obs[ob[j]] + sig.year[year_index[j]] + bTemp*temperature_sc[j])
    
    # Construct cell probabilities for nG multinomial cells (distance categories) PER SITE
    
    for(k in 1:nG){ 
    
    p[s,j,k]<-1-exp(-(midpt[k]/sigma[s,j])^-beta[s])
    pi[s,j,k] <- int.w[k] / strip.width 
    fc[s,j,k]<- p[s,j,k] * pi[s,j,k]                 ## pi=percent area of k; drops out if constant
    fct[s,j,k]<-fc[s,j,k]/pcap[s,j] 
    }
    
    pcap[s,j] <- sum(fc[s,j,1:nG]) # Different per site and year (sum over all bins)
    
    
    y[j,s] ~ dbin(pcap[s,j], N[j,s]) 
    N[j,s] ~ dpois(lambda[j,s]) 
    lambda[j,s] <- exp(alam[s,allyears[j]] + spsite[s,sitesYears[j]] 
                    + b.a1[s]*area1[j] + ba2.lam*area2[j] + ba3.lam*area3[j]
                    + b.cdiv.lam*cdiv[j] + b.fsiz.lam*fsiz[j]) 
    } }
    # Derived parameters
    
    #for (i in 1:nyears){
    #Ntotal[i] <- sum(N[s]*indexYears[,i]) 
    #}
    
    for (s in 1:nSpecies){
    for (i in 1:nyears){
    Ntotal[i,s] <- sum(N[,s]*indexYears[,i]) }}
    
    }", fill=TRUE, 
    file = "s_HRdetect_beta(s)_sigma[alpha(s)_obs(j,t)_year(t)_covTemp(j)]_lambda[alpha(s,t)_spsite(s,j)_covArea(s,j,t)_covLands(j,t)].txt")

# Inits
Nst <- yLong.sp + 1
inits <- function(){list(mu_l = runif(1), sig_l = 0.2, sig_spsite = runif(1),
                         N=Nst,
                         mu_a1 = runif(1), sig_a1 = runif(1), ba2.lam = runif(1), ba3.lam = runif(1),
                         b.cdiv.lam = runif(1), b.fsiz.lam = runif(1),
                         sig.sig.ob = runif(1), bTemp = runif(1), sig.sig.year = runif(1),
                         mu_s = runif(1, log(30), log(50)) , sig_s = runif(1),
                         mu_b = runif(1) , sig_b = runif(1))}


# Params
params <- c("Ntotal", #"N", "sigma", "lambda", I remove it so that it doesnt save the lambdas and takes shorter. It still calculates them
            "mu_l", "sig_l", "sig_spsite",
            "mu_a1", "sig_a1", "ba2.lam", "ba3.lam", "b.cdiv.lam", "b.fsiz.lam",
            "sig.sig.ob", "bTemp", "sig.sig.year",
            "mu_s", "sig_s", "mu_b", "sig_b"
)

# MCMC settings
nc <- 3 ; ni <- 15000 ; nb <- 2000 ; nt <- 2

# With jagsUI 
out <- jags(data1, inits, params, "s_HRdetect_beta(s)_sigma[alpha(s)_obs(j,t)_year(t)_covTemp(j)]_lambda[alpha(s,t)_spsite(s,j)_covArea(s,j,t)_covLands(j,t)].txt", n.chain = nc,
            n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
print(out)

summary <- as.data.frame(as.matrix(out$summary))

# To compare:
data_comp <- list(N.tot = N.tot, mu.a1 = mu.a1, sig.a1 = sig.a1, b.a2 = b.a2, b.a3 = b.a3,
                  mu.lam.alpha.spyear = mu.lam.alpha.spyear,
                  bCropdiv = bCropdiv, bFieldsize = bFieldsize,
                  sig.lam.alpha.spyear = sig.lam.alpha.spyear,
                  sig.lam.spsite = sig.lam.spsite,
                  bTemp.sig = bTemp.sig, 
                  sig.sig.obs = sig.sig.obs,
                  sig.sig.year = sig.sig.year,
                  mu.sig.sp = mu.sig.sp,
                  sig.sig.sp = sig.sig.sp,
                  mu.b = mu.b,
                  sig.b = sig.b)

