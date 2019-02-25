
rm(list=ls())

library(rjags)
library(jagsUI)
library(plyr)


set.seed(2013)

# ---- Data simulation ----

#### Simulate abundance for one species with a Markovian structure: Abundance in one year depends on the previous year
# I stopped because of RS comment: If abundance in one year in a site is 0, it will remain 0 for the rest of the study.
# This means that there is no inmigration in the population and so it is NOT ECOLOGICALLY PLAUSIBLE FOR MY DATA.

# THIS MODEL IS TO CALCULATE TRENDS AND THEN COMPARE IT WITH THE TRIM
  # 8 years (unbalanced number of transects per year); 
    # Lambda site specific (Zone variable)
    # Sigma site-year specific (effect of zone cov and random effect in observer)

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
yrs <- 0:7 # eight years
nyrs <- length(yrs)

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


# ----  Abundance component: random effect accross sites and zone covariate ----

# RANDOM EFFECT IN SITE (INDEPENDENT OF THE YEAR)
# Mean abundance and sd across sites
mu.lam.alpha.site <- log(1.5)				
sig.lam.alpha.site <- 0.5				
##Site effect in lambda
lam.alpha.site <- rnorm(max.sites, mu.lam.alpha.site, sig.lam.alpha.site) 

#ZONE COVARIATE (SITE)
# Coefficient (I had created the co-variate already!So dont generate it twice!)
b.lam.zoneB <- -0.5

lamnew <- exp(lam.alpha.site + b.lam.zoneB*zone[,2])  # One lambda per site, that will be the lambda of year 1
lamnew <- data.frame(lamnew)

# The lambdas of subsequent years will depend on year 1*survival + recruitment
#change recruitment to .3 for pop. rate of change of .9
surv=0.6
rec=0.35  #set to 0.3 for pop. rate of change of 0.95
Beta<- log(surv+rec)

# ---- SIMULATE ABUNDANCE ----

trans <- sort(sample(1:max.sites, max.sites, replace=FALSE))
N <- matrix(nrow = max.sites, ncol = nyrs)

# Abundance in year 1  (negative binomial for overdispersion)
N[,1]<-rnbinom (n=max.sites, size=exp(-1.02), mu=lamnew[trans,1]) #generate individual counts per transect

# Abundance in subsequent years
for(y in 2:nyrs){
  N[,y]<-rbinom(max.sites,N[,y-1], surv) + rpois(max.sites,N[,y-1]*rec)
}# Im gonna do it without NA, because I think the model should be writen the same and its easier to simulate

# Total number of individuals in all sampled transects per year
N.tot <- colSums(N)

# Cluster size
average_clus <- 2.3 # Invent an average cluster size: 2.3
Nclus <- N * average_clus # Correct N with average cluster size
Nclus.tot <- colSums(Nclus,na.rm = TRUE) # Total pop.abundance corrected by cluster size

#########################################################################################
# ---- SIMULATE CONTINUOUS DISTANCE DATA ---- aqui

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
             