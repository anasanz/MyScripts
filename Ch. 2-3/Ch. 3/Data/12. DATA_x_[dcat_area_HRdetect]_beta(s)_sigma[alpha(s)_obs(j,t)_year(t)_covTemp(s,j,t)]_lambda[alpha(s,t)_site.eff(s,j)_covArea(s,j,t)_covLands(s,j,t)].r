rm(list=ls())

library(rjags)
library(jagsUI)
library(dplyr)


# MODEL 12. in Community data

# sigma = exp(alpha(s) + observer(j,t) + year(t) + b*Temp(j,t))
# lambda = exp(alpha(s,t) + sp.site(s,j) + b1(s)*fallowSG(j,t) + b2(s)*fallowAES(j,t) + b3(s)*fallowGREEN(j,t) + b4(s)*Cropdiv(j,t) + b5(s)*Fieldsize(j,t))

# ---- Data ----

#setwd("C:/Users/ana.sanz/Documents/PhD/Third chapter/Data")
setwd("D:/ANA/Data")
d <- read.csv("DataDS_ch3_20_18_final.csv")
d <- d[ ,colnames(d) %in% c("Species2", "Zone", "distance", "transectID", "Temp", "Observer", "Banda", "Year", "Sample.Label", "Count")]
d <- d[-which(d$Species2 == "TERAX_ind"), ] # To consider F and M as different species
d <- d[which(d$Year %in% c(2015,2016,2017)), ] # For now only 3 years, because I don't have the data for 2019 and the AES from 2018 and 2019

# Information: bins, years, sites, species

strip.width <- 500 				# strip half-width, w (in this example only one side of the line transect is surveyed)
dist.breaks <- c(0,25,50,100,200,500)
int.w <- diff(dist.breaks) # width of distance categories (v)
midpt <- (int.w/2) + dist.breaks[-6]
nG <- length(dist.breaks)-1	

yrs <- c(2015, 2016, 2017)
yrs2 <- c(1, 2, 3)
nyrs <- length(yrs)

all.sites <- unique(d$transectID)
all.sites <- sort(all.sites, descreasing = TRUE)
max.sites <- length(all.sites)
total.sites <- max.sites*nyrs # Total number of site-year combinations

sp <- unique(d$Species2)
sp <- sort(sp, descreasing = TRUE)
nSpecies <- length(sp)


# ----  All detections all species (to detect sites not sampled)   ---- 

m <- matrix(NA, nrow = max.sites, ncol = nyrs)
rownames(m) <- all.sites
colnames(m) <- yrs

# Add counts > 0
count <- aggregate(Species2 ~ Year + transectID, FUN = length, data = d)

for (i in 1:nrow(count)){
  m[which(rownames(m) %in% count$transectID[i]), which(colnames(m) %in% count$Year[i])] <- count$Species2[i]
}

not_sampled <- is.na(m) # These are the sites not sampled in a given year. There are errors (NA por fichas no pasadas)

# ---- Counts per transect y (from distance observations) ----

data_sp <- array(0, c(max.sites, nyrs, nSpecies)) # Array to store all species all counts


for (s in 1:nSpecies){
  d_sp <- d[which(d$Species2 %in% sp[s]), ] # Select SP
  count_sp <- aggregate(Species2 ~ Year + transectID, FUN = length, data = d_sp) # Group counts per year and site
  
  m_sp <- matrix(0, nrow = max.sites, ncol = nyrs) # df to store data of individual species in loop
  rownames(m_sp) <- all.sites
  colnames(m_sp) <- yrs
  
  for (i in 1:nrow(count_sp)){ # Fill counts per transect and year in df
    m_sp[which(rownames(m_sp) %in% count_sp$transectID[i]), which(colnames(m_sp) %in% count_sp$Year[i])] <- count_sp$Species2[i] 
  }
  m_sp[is.na(m)] <- NA # NA in sites not sampled that year
  print(sum(m_sp, na.rm = TRUE)) 
  data_sp[,,s] <- m_sp # Store in array with all species
}

# ---- Co-variates ----

## FALLOW ##
#setwd("C:/Users/ana.sanz/Documents/PhD/Third chapter/Data")
setwd("D:/ANA/Data")
manag <- read.csv("management_area_15_17.csv")

manag <- manag[which(manag$Codi %in% all.sites), ] # Select transects with census

# Be sure the fields are in the same order
order <- as.data.frame(m)
order_codi <- as.vector(rownames(order))
order$Codi <- order_codi
manag <- left_join(order,manag)

# ABUNDANCE MODEL #
# Area AES
area_aes <- as.matrix(manag[ ,c(6:8)])

aes_mean <- mean(area_aes)
aes_sd <- sd(area_aes)
aes_sc <- (area_aes - aes_mean) / aes_sd

# Area SG
area_sg <- as.matrix(manag[ ,c(9:11)])

sg_mean <- mean(area_sg)
sg_sd <- sd(area_sg)
sg_sc <- (area_sg - sg_mean) / sg_sd

# Area GREEN
area_green <- as.matrix(manag[ ,c(12:14)])

green_mean <- mean(area_green)
green_sd <- sd(area_green)
green_sc <- (area_green - green_mean) / green_sd

## LANDSCAPE ##

# Crop diversity

#setwd("C:/Users/ana.sanz/Documents/PhD/Third chapter/Data")
setwd("D:/ANA/Data")
cropdiv <- read.csv("crop_richness_500.csv")

cropdiv <- cropdiv[which(cropdiv$Codi %in% all.sites), ] # Select transects with census

# Be sure the fields are in the same order
order <- as.data.frame(m)
order_codi <- as.vector(rownames(order))
order$Codi <- order_codi
cropdiv <- left_join(order,cropdiv)

crop_diversity <- as.matrix(cropdiv[ ,c(6:8)])

crop_diversity_mean <- mean(crop_diversity)
crop_diversity_sd <- sd(crop_diversity)
crop_diversity_sc <- (crop_diversity - crop_diversity_mean) / crop_diversity_sd

# Field size

fieldsiz <- read.csv("av_fieldsize_500.csv")

fieldsiz <- fieldsiz[which(fieldsiz$Codi %in% all.sites), ] # Select transects with census

# Be sure the fields are in the same order
order <- as.data.frame(m)
order_codi <- as.vector(rownames(order))
order$Codi <- order_codi
fieldsiz <- left_join(order,fieldsiz)

field_size <- as.matrix(fieldsiz[ ,c(6:8)])

field_size_mean <- mean(field_size)
field_size_sd <- sd(field_size)
field_size_sc <- (field_size - field_size_mean) / field_size_sd

# OBSERVATION MODEL #

# Observer 
obs <- matrix(NA, nrow = max.sites, ncol = nyrs)
rownames(obs) <- all.sites
colnames(obs) <- yrs

d$Observer <- as.character(d$Observer)

for (i in 1:nrow(d)){
  obs[which(rownames(obs) %in% d$transectID[i]), which(colnames(obs) %in% d$Year[i])] <- d$Observer[i]}

# Temperature
temp <- matrix(NA, nrow = max.sites, ncol = nyrs)
rownames(temp) <- all.sites
colnames(temp) <- yrs

for (i in 1:nrow(d)){
  temp[which(rownames(temp) %in% d$transectID[i]), which(colnames(temp) %in% d$Year[i])] <- d$Temp[i]}

temp_mean <- mean(temp, na.rm = TRUE)
temp_sd <- sd(temp, na.rm = TRUE)
temp_sc <- (temp - temp_mean) / temp_sd

# Year
year <- matrix(NA, nrow = max.sites, ncol = nyrs)
rownames(year) <- all.sites
colnames(year) <- yrs

for (i in 1:nrow(d)){
  year[which(rownames(year) %in% d$transectID[i]), which(colnames(year) %in% d$Year[i])] <- d$Year[i]}

# ---- Specify data in JAGS format ----

##### For multinomial model (model the observations in bins) ######
# Distance class and ind (should have the same length)

nind.sp <- list()
for (s in 1:nSpecies){
  nind.sp[[s]] <- sum(unlist(data_sp[,,s], use.names = F), na.rm = TRUE)} # Just to know, but jags only wants the sum
nind <- do.call(sum, nind.sp)

dclass <- d$Banda # Right order?or it doesnt matter because it is to build the bin probabilities?

##### For abundance model (y) ######
# Get one long matrix with counts and sites per species (columns) 
yLong.sp <- matrix(NA, nrow = total.sites, ncol = nSpecies)
for (s in 1:nSpecies){
  yLong.sp[ ,s] <- unlist(as.data.frame(data_sp[,,s]), use.names = F) # With NA included (model estimating abundance in sites with no information)
}

##### For multinomial model (model the observations in bins) ######

# Get one long vector for each site-year combination of each dclass observation
# (so, at which j, or siteyear is every observation or dclass corresponding?)
###RS: Fixed index to map dclass onto site-year combinations (all species together)
# For the index, create a vector of ylong where NA are 0 (because I need the same length)

yLong_index <- yLong.sp
yLong_index[which(is.na(yLong_index))] <- 0

n.allSiteYear <- max.sites*nyrs
siteYear.dclass <- NULL

for (s in 1:nSpecies){
  for (i in 1:n.allSiteYear){
    siteYear.dclass <- c(siteYear.dclass,rep(i, yLong_index[i,s]))}
}

# Fixed index to map dclass in species (so that it matches with the dimensions (s,j,K))

sp.dclass <- NULL
for (s in 1:nSpecies){
  for (i in 1:n.allSiteYear){
    sp.dclass <- c(sp.dclass,rep(s, yLong_index[i,s]))}
}

##### For abundance model (y) ###### 

# All these index and variables are site-speficic (not species specific) so they stay like in single species model

sitesYears <- NULL
for (i in 1:nyrs){
  sitesYears <- c(sitesYears,c(1:length(all.sites)))}

# Get one vector per co-variate
# Abundance covariates
area_SG <- NULL
for (i in 1:nyrs){
  area_SG <- c(area_SG,sg_sc[1:length(all.sites),i])}

area_AES <- NULL
for (i in 1:nyrs){
  area_AES <- c(area_AES,aes_sc[1:length(all.sites),i])}

area_GREEN <- NULL
for (i in 1:nyrs){
  area_GREEN <- c(area_GREEN,green_sc[1:length(all.sites),i])}

CDIV <- NULL
for (i in 1:nyrs){
  CDIV <- c(CDIV,crop_diversity_sc[1:length(all.sites),i])}

FSIZ <- NULL
for (i in 1:nyrs){
  FSIZ <- c(FSIZ,field_size_sc[1:length(all.sites),i])}


# Detection covariates
ob <- NULL
for (i in 1:nyrs){
  ob <- c(ob,obs[1:length(all.sites),i])}
ob <- as.numeric(factor(ob)) # JAGS doesn't accept categorical variables

## RS: = observer is a covariate so NAs are a problem because nothing
# in the model specifies how the NAs can be estimated ( never shows up 
# on the left hand side of a line of code) So there are two solutions:

### 1.Estimate observer for missing observations
####### log(sigma[j,k])<-alpha[observer[j,k]] + beta*X  
####### observer[j,k]~dcat(probs)

### 2. Because there is no data points where observer is NA, and because
# I am not trying to estimate sigma in every point (only abundance, and in the
# missing points of data this is estimated using the noNA and the co-variates.
# i.e., you have covariate information for the abundance component of the missing 
# year-transect combinations, so you can use that to predict abundance for these missing points)
# Then, you can fill observer NAs with random IDs and it wont affect the model estimates.
# (ONLY BECAUSE THERE IS NO DATA ASSOCIATED WITH THE OBSERVER NAs)

obs_id <- unique(ob)[-3]
ob[which(is.na(ob))] <- sample(obs_id, length(which(is.na(ob))), replace = TRUE)

nobs <- length(unique(ob))

temperature_sc <- NULL
for (i in 1:nyrs){
  temperature_sc <- c(temperature_sc,temp_sc[1:length(all.sites), i])
}
# Same than with observer (cannot have NA)
tmp_id <- unique(temperature_sc)[-17]
temperature_sc[which(is.na(temperature_sc))] <- sample(tmp_id, length(which(is.na(temperature_sc))), replace = TRUE)


year_df <- matrix(yrs2, nrow = max.sites, ncol = nyrs, byrow = TRUE) # Matix with IDs
year_index <- NULL
for (i in 1:nyrs){
  year_index <- c(year_index,year_df[1:length(all.sites), i])
}

# Create one matrix for indexing year when calculating abundance per year in JAGS (works for all species)

allyears <- NULL 
for (i in 1:nyrs){
  allyears <- c(allyears,rep(yrs2[i],length(all.sites)))
}
m <- data.frame(allyears = allyears)
m$allyears <- as.factor(m$allyears)
indexYears <- model.matrix(~ allyears-1, data = m)

# ---- Compile data for JAGS model ----

data1 <- list(nyears = nyrs, max.sites = max.sites, nG=nG, siteYear.dclass = siteYear.dclass, int.w=int.w, strip.width = strip.width, midpt = midpt, db = dist.breaks, year_index = year_index,
              y = yLong.sp, n.allSiteYear = n.allSiteYear, nind=nind, dclass=dclass, sitesYears = sitesYears, indexYears = indexYears, allyears = allyears,
              area1 = area_SG, area2 = area_AES, area3 = area_GREEN, cdiv = CDIV, fsiz = FSIZ, ob = ob, nobs = nobs, temperature_sc = temperature_sc,
              nSpecies = nSpecies, sp.dclass = sp.dclass, nyrs = nyrs)

# ---- JAGS model ----

#setwd("C:/Users/ana.sanz/Documents/PhD/Second chapter/Data/Model")
setwd("D:/ANA/Model")

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
nc <- 3 ; ni <- 50000 ; nb <- 10000 ; nt <- 2

# With jagsUI 
out <- jags(data1, inits, params, "s_HRdetect_beta(s)_sigma[alpha(s)_obs(j,t)_year(t)_covTemp(j)]_lambda[alpha(s,t)_spsite(s,j)_covArea(s,j,t)_covLands(j,t)].txt", n.chain = nc,
            n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
print(out)

summary <- as.data.frame(as.matrix(out$summary))

#setwd("C:/Users/ana.sanz/Documents/PhD/Third chapter/Data/Model")
setwd("D:/ANA/Results")

save(out, file = "12_D.RData")