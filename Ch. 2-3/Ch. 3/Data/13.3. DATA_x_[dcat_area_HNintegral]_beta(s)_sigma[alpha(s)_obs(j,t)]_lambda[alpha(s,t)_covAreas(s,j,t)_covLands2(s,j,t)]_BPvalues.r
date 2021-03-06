rm(list=ls())

library(rjags)
library(jagsUI)
library(dplyr)


# MODEL 13.3 in Community data (15 species)

# sigma = exp(alpha(s) + observer(j,t) 
# lambda = exp(alpha(s,t) + b1*fallowSG(j,t) + b2(s)*fallowAES(j,t) + b3(s)*cropdiver(j,t) + b4(s)*fieldsize(j,t) 

# ---- Data ----

setwd("D:/PhD/Third chapter/Data")
d <- read.csv("DataDS_ch3_20_19_READY.csv")
d <- d[ ,colnames(d) %in% c("Species", "Zone", "distance", "transectID", "Temp", "Observer", "Banda", "Year", "Sample.Label", "Count")]
d <- d[which(d$Year %in% c(2015,2016,2017,2018,2019)), ] 

# Information: bins, years, sites, species

strip.width <- 500 				# strip half-width, w (in this example only one side of the line transect is surveyed)
dist.breaks <- c(0,25,50,100,200,500)
int.w <- diff(dist.breaks) # width of distance categories (v)
midpt <- (int.w/2) + dist.breaks[-6]
nG <- length(dist.breaks)-1	

yrs <- c(2015, 2016, 2017, 2018, 2019)
yrs2 <- c(1, 2, 3, 4, 5)
nyrs <- length(yrs)

all.sites <- unique(d$transectID)
all.sites <- sort(all.sites, descreasing = TRUE)
max.sites <- length(all.sites)
total.sites <- max.sites*nyrs # Total number of site-year combinations


# ----  All detections all species (to detect sites not sampled)   ---- 

m <- matrix(NA, nrow = max.sites, ncol = nyrs)
rownames(m) <- all.sites
colnames(m) <- yrs

# Add counts > 0
count <- aggregate(Species ~ Year + transectID, FUN = length, data = d)

for (i in 1:nrow(count)){
  m[which(rownames(m) %in% count$transectID[i]), which(colnames(m) %in% count$Year[i])] <- count$Species[i]
}

not_sampled <- is.na(m) # These are the sites not sampled in a given year. There are errors (NA por fichas no pasadas)

# --- Select the species that I want to analyze ----

# Select the species with best detection curve and sample size
select_best <- read.csv("infospecies_ch3_FINAL_1019.csv", sep = ";")
best <- as.character(select_best$Species[which(select_best$BEST == 1)])

d <- d[which(d$Species %in% best), ]

sp <- as.character(unique(d$Species))
sp <- sort(sp)
nSpecies <- length(sp)


# ---- Counts per transect y (from distance observations) ----

data_sp <- array(0, c(max.sites, nyrs, nSpecies)) # Array to store all species all counts


for (s in 1:nSpecies){
  d_sp <- d[which(d$Species %in% sp[s]), ] # Select SP
  count_sp <- aggregate(Species ~ Year + transectID, FUN = length, data = d_sp) # Group counts per year and site
  
  m_sp <- matrix(0, nrow = max.sites, ncol = nyrs) # df to store data of individual species in loop
  rownames(m_sp) <- all.sites
  colnames(m_sp) <- yrs
  
  for (i in 1:nrow(count_sp)){ # Fill counts per transect and year in df
    m_sp[which(rownames(m_sp) %in% count_sp$transectID[i]), which(colnames(m_sp) %in% count_sp$Year[i])] <- count_sp$Species[i] 
  }
  m_sp[is.na(m)] <- NA # NA in sites not sampled that year
  print(sum(m_sp, na.rm = TRUE)) 
  data_sp[,,s] <- m_sp # Store in array with all species
}

# ---- Co-variates ----

setwd("D:/PhD/Third chapter/Data")
aes <- read.csv("AES_15_19.csv")
sg <- read.csv("SG_15_19.csv")
crop_diversity <- read.csv("crop_richness_500.csv")
field_size <- read.csv("av_fieldsize_500.csv")


aes <- aes[which(aes$Codi %in% all.sites), ] # Select transects with census
sg <- sg[which(sg$Codi %in% all.sites), ] 
crop_diversity <- crop_diversity[which(crop_diversity$Codi %in% all.sites), ] 
field_size <- field_size[which(field_size$Codi %in% all.sites), ] 


# Be sure the fields are in the same order
order <- as.data.frame(m)
order_codi <- as.vector(rownames(order))
order$Codi <- order_codi
aes <- left_join(order,aes, by = "Codi")
sg <- left_join(order,sg, by = "Codi")
crop_diversity <- left_join(order,crop_diversity, by = "Codi")
field_size <- left_join(order,field_size, by = "Codi")

# ABUNDANCE MODEL #

# Need to log-transform the FALLOW variables (AES and SG) because of extreme values (see script Explore_corr.r)

# Area AES
area_aes_real <- as.matrix(aes[ ,c(8:12)])
area_aes_real <- area_aes_real + 0.001 # Add a constant for log-transform (log(0) = Inf)
area_aes <- log(area_aes_real)

aes_mean <- mean(area_aes) # Also scale to unify with different variables like crop diversity
aes_sd <- sd(area_aes)
aes_sc <- (area_aes - aes_mean) / aes_sd

# Area SG
area_sg_real <- as.matrix(sg[ ,c(8:12)])
area_sg_real <- area_sg_real + 0.001 # Add a constant for log-transform (log(0) = Inf)
area_sg <- log(area_sg_real)


sg_mean <- mean(area_sg)
sg_sd <- sd(area_sg)
sg_sc <- (area_sg - sg_mean) / sg_sd

# Crop diversity
crop_diversity <- as.matrix(crop_diversity[ ,c(8:12)])

cd_mean <- mean(crop_diversity)
cd_sd <- sd(crop_diversity)
cd_sc <- (crop_diversity - cd_mean) / cd_sd

# Field size
field_size <- as.matrix(field_size[ ,c(8:12)])

fs_mean <- mean(field_size)
fs_sd <- sd(field_size)
fs_sc <- (field_size - fs_mean) / fs_sd


# OBSERVATION MODEL #

# Observer 
obs <- matrix(NA, nrow = max.sites, ncol = nyrs)
rownames(obs) <- all.sites
colnames(obs) <- yrs

d$Observer <- as.character(d$Observer)

for (i in 1:nrow(d)){
  obs[which(rownames(obs) %in% d$transectID[i]), which(colnames(obs) %in% d$Year[i])] <- d$Observer[i]}


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


# Get one vector per co-variate
# Abundance covariates
area_SG <- NULL
for (i in 1:nyrs){
  area_SG <- c(area_SG,sg_sc[1:length(all.sites),i])}

area_AES <- NULL
for (i in 1:nyrs){
  area_AES <- c(area_AES,aes_sc[1:length(all.sites),i])}

crop_div <- NULL
for (i in 1:nyrs){
  crop_div <- c(crop_div,cd_sc[1:length(all.sites),i])}

f_size <- NULL
for (i in 1:nyrs){
  f_size <- c(f_size,fs_sc[1:length(all.sites),i])}

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


# Create one matrix for indexing year when calculating abundance per year in JAGS (works for all species)

allyears <- NULL 
for (i in 1:nyrs){
  allyears <- c(allyears,rep(yrs2[i],length(all.sites)))
}
m <- data.frame(allyears = allyears)
m$allyears <- as.factor(m$allyears)
indexYears <- model.matrix(~ allyears-1, data = m)



# ---- JAGS model ----

data1 <- list(nyears = nyrs, max.sites = max.sites, nG=nG, siteYear.dclass = siteYear.dclass, int.w=int.w, strip.width = strip.width, 
              y = yLong.sp, n.allSiteYear = n.allSiteYear, nind=nind, dclass=dclass, indexYears = indexYears, allyears = allyears,
              area1 = area_SG, area2 = area_AES, cdiv = crop_div, fsiz = f_size,
              ob = ob, nobs = nobs, db = dist.breaks,
              nSpecies = nSpecies, sp.dclass = sp.dclass, nyrs = nyrs)


# ---- JAGS model ----

setwd("D:/PhD/Third chapter/Data/Model")
cat("model{
    
    # PRIORS
    
    # SPECIES SPECIFIC PARAMETERS (random effects)
    
    for (s in 1:nSpecies){              # Random intercept for sigma (dif detection per species)
    asig[s] ~ dnorm(mu_s, tau_s)
    b.a1[s] ~ dnorm(mu_a1, tau_a1)
    b.a2[s] ~ dnorm(mu_a2, tau_a2)
    bCropdiv[s] ~ dnorm(mu_cd, tau_cd)
    bFieldsize[s] ~ dnorm(mu_fs, tau_fs)
    }
    
    
    for(s in 1:nSpecies){              # Random intercept for lambda (dif abundance per species and year)
    for(t in 1:nyrs){
    alam[s,t] ~ dnorm(mu_l,tau_l)}}
    
    
    # Hyperparameters of species level random effects
    
    mu_s ~ dnorm(0,0.01) # Hyperparameters for sigma intercept
    tau_s <- 1/(sig_s*sig_s)
    sig_s ~ dunif(0,500)
    
    mu_l ~ dnorm(0,0.01) # Hyperparameters for lambda intercept
    tau_l <- 1/(sig_l*sig_l)
    sig_l ~ dunif(0,500)
    
    mu_a1 ~ dnorm(0,0.01) # Hyperparameters for beta coefficient area1
    tau_a1 <- 1/(sig_a1*sig_a1)
    sig_a1 ~ dunif(0,500)
    
    mu_a2 ~ dnorm(0,0.01) # Hyperparameters for beta coefficient area1
    tau_a2 <- 1/(sig_a2*sig_a2)
    sig_a2 ~ dunif(0,500)
    
    mu_cd ~ dnorm(0,0.01) # Hyperparameters for beta coefficient crop diver
    tau_cd <- 1/(sig_cd*sig_cd)
    sig_cd ~ dunif(0,500)
    
    mu_fs ~ dnorm(0,0.01) # Hyperparameters for beta coefficient field size
    tau_fs <- 1/(sig_fs*sig_fs)
    sig_fs ~ dunif(0,500)
    
    
    # PRIORS FOR SIGMA
    
    sig.sig.ob ~ dunif(0, 10) # Random effects for sigma per observer
    tau.sig.ob <- 1/(sig.sig.ob*sig.sig.ob)
    
    #Random observer effect for sigma
    
    for (o in 1:nobs){
    sig.obs[o] ~ dnorm(0, tau.sig.ob)
    }
    
    for(i in 1:nind){
    dclass[i] ~ dcat(fct[sp.dclass[i],siteYear.dclass[i], 1:nG])

    # FOR BP.OBS
    # Generate new observations, calculate residuals for Bayesian p-value on detection component
    
    dclassnew[i] ~ dcat(fct[sp.dclass[i],siteYear.dclass[i],1:nG]) 
    Tobsp[i] <- pow(1- sqrt(fct[sp.dclass[i],siteYear.dclass[i],dclass[i]]),2)
    Tobspnew[i] <- pow(1- sqrt(fct[sp.dclass[i],siteYear.dclass[i],dclassnew[i]]),2)
    }
    
    for (s in 1:nSpecies){
    
    for(j in 1:n.allSiteYear){ 
    
    sigma[s,j] <- exp(asig[s] + sig.obs[ob[j]])
    
    f.0[s,j] <- 2 * dnorm(0,0, 1/sigma[s,j]^2)
    
    # Construct cell probabilities for nG multinomial cells (distance categories) PER SITE
    
    for(k in 1:nG){ 
    
    up[s,j,k]<-pnorm(db[k+1], 0, 1/sigma[s,j]^2) ##db are distance bin limits
    low[s,j,k]<-pnorm(db[k], 0, 1/sigma[s,j]^2) 
    p[s,j,k]<- 2 * (up[s,j,k] - low[s,j,k])
    pi[s,j,k] <- int.w[k] / strip.width 
    f[s,j,k]<- p[s,j,k]/f.0[s,j]/int.w[k]                   ## detection prob. in distance category k                      
    fc[s,j,k]<- f[s,j,k] * pi[s,j,k]                 ## pi=percent area of k; drops out if constant
    fct[s,j,k]<-fc[s,j,k]/pcap[s,j] 
    }
    
    pcap[s,j] <- sum(fc[s,j,1:nG]) # Different per site and year (sum over all bins)
    
    y[j,s] ~ dbin(pcap[s,j], N[j,s]) 
    N[j,s] ~ dpois(lambda[j,s]) 
    
    lambda[j,s] <- exp(alam[s,allyears[j]] + b.a1[s]*area1[j] + b.a2[s]*area2[j] + bCropdiv[s]*cdiv[j] + bFieldsize[s]*fsiz[j] ) 
    
    # FOR BP.N
    # Create replicate abundances (new observations) for Bayesian p-value on abundance component
    Nnew[j,s]~dpois(lambda[j,s])
      
    # Residuals for 'observed' and new abundances: species and site specific residuals
    FT1[j,s] <- pow(sqrt(N[j,s]) - sqrt(lambda[j,s]),2)
    FT1new[j,s] <- pow(sqrt(Nnew[j,s]) - sqrt(lambda[j,s]),2)
    } 
    # Sum residuals over sites and years
    T1p[s]<-sum(FT1[1:n.allSiteYear,s])
    T1newp[s]<-sum(FT1new[1:n.allSiteYear,s])
    }

    # Bayesian p-value
    Bp.N <- sum(T1newp[1:nSpecies]) > sum(T1p[1:nSpecies])
    Bp.Obs <- sum(Tobspnew[1:nind]) > sum(Tobsp[1:nind])

    # Derived parameters
    
    #for (i in 1:nyears){
    #Ntotal[i] <- sum(N[s]*indexYears[,i]) 
    #}
    
    for (s in 1:nSpecies){
    for (i in 1:nyears){
    Ntotal[i,s] <- sum(N[,s]*indexYears[,i]) }}
    
    }", fill=TRUE, 
    file = "s_HNintegral_sigma[alpha(s)_obs(j,t)]_lambda[alpha(s,t)_covAreas(s,j,t)_covLands2(s,j,t)]_BPvalues.txt")

# Inits
Nst <- yLong.sp + 1
inits <- function(){list(mu_l = runif(1), sig_l = 0.2, 
                         N=Nst,
                         mu_a1 = runif(1), sig_a1 = runif(1), mu_a2 = runif(1), sig_a2 = runif(1),
                         mu_cd = runif(1), sig_cd = runif(1), mu_fs = runif(1), sig_fs = runif(1),
                         sig.sig.ob = runif(1),
                         mu_s = runif(1, log(30), log(50)) , sig_s = runif(1)
)}


# Params: Monitor also the species specific coefficients (random effects levels)
params <- c( "mu_l", "sig_l", 
             "b.a1","mu_a1", "sig_a1", "b.a2", "mu_a2", "sig_a2",
             "bCropdiv", "mu_cd", "sig_cd", "bFieldsize", "mu_fs", "sig_fs",
             "sig.sig.ob",
             "mu_s", "sig_s", "Bp.N", "Bp.Obs"
)

# MCMC settings
nc <- 3 ; ni <- 200000 ; nb <- 30000 ; nt <- 10

# With jagsUI 
out <- jags(data1, inits, params, "s_HNintegral_sigma[alpha(s)_obs(j,t)]_lambda[alpha(s,t)_covAreas(s,j,t)_covLands2(s,j,t)]_BPvalues.txt", n.chain = nc,
            n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)

setwd("D:/ANA/Results/chapter3")
save(out, file = "13.3_DATA.RData")

print(out)

summary <- as.data.frame(as.matrix(out$summary))

traceplot(out, parameters = c("mu_l", "sig_l", 
                              "mu_a1", "sig_a1", "mu_a2", "sig_a2",
                              "mu_cd", "sig_cd", "mu_fs", "sig_fs",
                              "sig.sig.ob",
                              "mu_s", "sig_s"))

