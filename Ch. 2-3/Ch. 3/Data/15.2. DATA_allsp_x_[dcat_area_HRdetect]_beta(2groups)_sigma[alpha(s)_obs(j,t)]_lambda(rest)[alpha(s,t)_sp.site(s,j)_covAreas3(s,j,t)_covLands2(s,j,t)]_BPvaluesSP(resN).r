rm(list=ls())

library(rjags)
library(jagsUI)
library(dplyr)


# MODEL 15.2 in Community data (all species): HR df and 2 betas for the community
# Calculate residuals for bp.obs with the total N instead of the FT test (resN)
# Restricted distribution of PTALC and CABRA

# sigma = exp(alpha(s) + observer(j,t) 
# lambda = exp(alpha(s,t) + sp.site (s,j) + b1*fallowSG(j,t) + b2(s)*fallowAES(j,t) + b3(s)*fallowGREEN(j,t) + b3(s)*cropdiver(j,t) + b4(s)*fsize(j,t)

# ---- Data ----

setwd("D:/PhD/Third chapter/Data")
d <- read.csv("DataDS_ch3_15_19_READY_FIXED.csv")

# To restrict distribution of PTALC and CABRA: remove observations out of the distr.range (probably a mistake)
unique(d$Region.Label)
d <- d[-which(d$Species == "CABRA" & d$Region.Label %in% c("BA", "SI", "BM", "AL")), ]
d[which(d$Species == "PTALC" & d$Region.Label %in% c("BA", "SI", "BM", "AL", "BE")), ]


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

# --- Select the species that I want to analyze (all) ----

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
green <- read.csv("GREEN_15_19.csv")
crop_diversity <- read.csv("crop_richness_500.csv")
field_size <- read.csv("av_fieldsize_500.csv")


aes <- aes[which(aes$Codi %in% all.sites), ] # Select transects with census
sg <- sg[which(sg$Codi %in% all.sites), ] 
green <- green[which(green$Codi %in% all.sites), ] 
crop_diversity <- crop_diversity[which(crop_diversity$Codi %in% all.sites), ] 
field_size <- field_size[which(field_size$Codi %in% all.sites), ] 


# Be sure the fields are in the same order
order <- as.data.frame(m)
order_codi <- as.vector(rownames(order))
order$Codi <- order_codi
aes <- left_join(order,aes, by = "Codi")
sg <- left_join(order,sg, by = "Codi")
green <- left_join(order,green, by = "Codi")
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

# Area GREEN
area_green_real <- as.matrix(green[ ,c(8:12)])
area_green_real <- area_green_real + 0.001 # Add a constant for log-transform (log(0) = Inf)
area_green <- log(area_green_real)


green_mean <- mean(area_green)
green_sd <- sd(area_green)
green_sc <- (area_green - green_mean) / green_sd

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

# Restrict the presence of PTALC (26) and CABRA(5)
# Vector of 1 or 0 indicating the transects
restrict.sp <- as.data.frame(matrix(1, nrow = total.sites, ncol = nSpecies))
restrict.sp$sites <- rep(all.sites, nyrs)
colnames(restrict.sp) <- sp

sites_ptalc <- all.sites[c(grep("AF", all.sites), grep("GR", all.sites))]
sites_cabra <- all.sites[c(grep("AF", all.sites), grep("GR", all.sites), grep("BE", all.sites))]
restrict.sp[which(!restrict.sp$sites %in% sites_ptalc), which(colnames(restrict.sp) %in% c("PTALC"))] <- 0
restrict.sp[which(!restrict.sp$sites %in% sites_cabra), which(colnames(restrict.sp) %in% c("CABRA"))] <- 0
restrict.sp <- as.matrix(restrict.sp[,-c(nSpecies+1)])

# Be sure that where Im gonna restrict it to 0 is actually 0 (error model because there is one observation in SIO)
yLong.sp[!is.na(yLong.sp) & restrict.sp == 0] <- 0


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

# All this index and variables are site-speficic (not species specific) so they stay like this
sitesYears <- NULL # I did that loop but the normal version actually works, since is an index per site-year
for (i in 1:nyrs){
  sitesYears <- c(sitesYears,c(1:length(all.sites)))
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

area_GREEN <- NULL
for (i in 1:nyrs){
  area_GREEN <- c(area_GREEN,green_sc[1:length(all.sites),i])}

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

# Create matrix for indexing species when calculating residuals for Bp.Obs for each species

m2 <- data.frame(sp.dclass = sp.dclass)
m2$sp.dclass <- as.factor(m2$sp.dclass)
indexSP <- model.matrix(~ sp.dclass -1, data = m2)
dim(indexSP)


# ---- Compile data for JAGS model ----

data1 <- list(nyears = nyrs, max.sites = max.sites, nG=nG, siteYear.dclass = siteYear.dclass, int.w = int.w, strip.width = strip.width, midpt = midpt,
              y = yLong.sp, n.allSiteYear = n.allSiteYear, nind=nind, dclass=dclass, sitesYears = sitesYears, indexYears = indexYears, allyears = allyears,
              area1 = area_SG, area2 = area_AES, area3 = area_GREEN, fsiz = f_size, cdiv = crop_div,
              ob = ob, nobs = nobs, db = dist.breaks,
              nSpecies = nSpecies, sp.dclass = sp.dclass, nyrs = nyrs, indexSP = indexSP, restrict.sp = restrict.sp)

# ---- JAGS model ----

setwd("D:/PhD/Third chapter/Data/Model")
cat("model{
    
    # PRIORS
    
    # SPECIES SPECIFIC PARAMETERS (random effects)
    
    for (s in 1:nSpecies){             
    asig[s] ~ dnorm(mu_s, tau_s)    # Random intercept for sigma (dif detection per species)
    b.a1[s] ~ dnorm(mu_a1, tau_a1)
    b.a2[s] ~ dnorm(mu_a2, tau_a2)
    b.a3[s] ~ dnorm(mu_a3, tau_a3)
    bCropdiv[s] ~ dnorm(mu_cd, tau_cd)
    bFieldsize[s] ~ dnorm(mu_fs, tau_fs)
    }
    
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
    
    mu_l ~ dnorm(0,0.01) # Hyperparameters for lambda intercept
    tau_l <- 1/(sig_l*sig_l)
    sig_l ~ dunif(0,500)
    
    mu_a1 ~ dnorm(0,0.01) # Hyperparameters for beta coefficient area1
    tau_a1 <- 1/(sig_a1*sig_a1)
    sig_a1 ~ dunif(0,500)
    
    mu_a2 ~ dnorm(0,0.01) # Hyperparameters for beta coefficient area2
    tau_a2 <- 1/(sig_a2*sig_a2)
    sig_a2 ~ dunif(0,500)
    
    mu_a3 ~ dnorm(0,0.01) # Hyperparameters for beta coefficient area3
    tau_a3 <- 1/(sig_a3*sig_a3)
    sig_a3 ~ dunif(0,500)
    
    mu_cd ~ dnorm(0,0.01) # Hyperparameters for beta coefficient crop diversity
    tau_cd <- 1/(sig_cd*sig_cd)
    sig_cd ~ dunif(0,500)
    
    mu_fs ~ dnorm(0,0.01) # Hyperparameters for beta coefficient field size
    tau_fs <- 1/(sig_fs*sig_fs)
    sig_fs ~ dunif(0,500)
    
    tau_spsite <- 1/(sig_spsite*sig_spsite) # Hyperparameter for site random effect in lambda
    sig_spsite ~ dunif(0,500)
    
    
    # PRIORS FOR SIGMA
    
    sig.sig.ob ~ dunif(0, 10) # Random effects for sigma per observer
    tau.sig.ob <- 1/(sig.sig.ob*sig.sig.ob)
    
    #Random observer effect for sigma
    
    for (o in 1:nobs){
    sig.obs[o] ~ dnorm(0, tau.sig.ob)
    }
    
    # PRIOR FOR BETA
    
    p.g ~ dbeta(1,1) #♠ Prior for probability of group membership
    
    for (s in 1:nSpecies){ # To model group membership (index for beta)
    g[s] ~ dbern(p.g)
    g.eff[s] <- g[s] + 1 #just turn it into a 1/2 index 
    }
    
    for (g in 1:2){              # Random beta per species (2 different ones)
    beta[g] ~ dunif(0, 100)}
    
    for(i in 1:nind){
    dclass[i] ~ dcat(fct[sp.dclass[i],siteYear.dclass[i], 1:nG])
    }
    
    for (s in 1:nSpecies){
    
    for(j in 1:n.allSiteYear){ 
    
    sigma[s,j] <- exp(asig[s] + sig.obs[ob[j]])
    
    # Construct cell probabilities for nG multinomial cells (distance categories) PER SITE
    
    for(k in 1:nG){ 
    
    p[s,j,k]<-1-exp(-(midpt[k]/sigma[s,j])^-beta[g.eff[s]])
    pi[s,j,k] <- int.w[k] / strip.width 
    fc[s,j,k]<- p[s,j,k] * pi[s,j,k]                 ## pi=percent area of k; drops out if constant
    fct[s,j,k]<-fc[s,j,k]/pcap[s,j] 
    }
    
    pcap[s,j] <- sum(fc[s,j,1:nG]) # Different per site and year (sum over all bins)
    
    y[j,s] ~ dbin(pcap[s,j], N[j,s]) 
    N[j,s] ~ dpois(lambda.eff[j,s]) 
    
    lambda[j,s] <- exp(alam[s,allyears[j]] + spsite[s,sitesYears[j]] 
    + b.a1[s]*area1[j] + b.a2[s]*area2[j] + b.a3[s]*area3[j] + bCropdiv[s]*cdiv[j] + bFieldsize[s]*fsiz[j] ) 
    
    lambda.eff[j,s] <- lambda[j,s] * restrict.sp[j,s]

    # FOR BP.OBS
    # Create a new Y (detections)
    y.new[j,s]~ dbin(pcap[s,j], N[j,s])
    
    # Calculate residuals residuals: look at the total number of individuals detected instead 
    Tobsp[j,s] <- pow(  (y[j,s] - (pcap[s,j] * N[j,s])) ,2)
    Tobsnewp[j,s] <- pow(  (y.new[j,s] - (pcap[s,j] * N[j,s])) ,2)
    
    
    # FOR BP.N
    # Create replicate abundances (new observations) for Bayesian p-value on abundance component
    Nnew[j,s]~dpois(lambda.eff[j,s])
    
    # Residuals for 'observed' and new abundances: species and site specific residuals
    FT1[j,s] <- pow(sqrt(N[j,s]) - sqrt(lambda.eff[j,s]),2)
    FT1new[j,s] <- pow(sqrt(Nnew[j,s]) - sqrt(lambda.eff[j,s]),2)
    } 
    
    # FOR BP.OBS:
    # Sum residuals over sites and years to get sp-specific bp.obs.values
    T1obsp[s]<-sum(Tobsp[1:n.allSiteYear,s])
    T1obsnewp[s]<-sum(Tobsnewp[1:n.allSiteYear,s])
    
    # SP-SPECIFIC BP.OBS
    Bp.Obs.sp[s] <-  T1obsp[s] > T1obsnewp[s] 
    
    
    # FOR BP.N
    # Sum residuals over sites and years to get sp-specific bp.N.values
    T1p[s]<-sum(FT1[1:n.allSiteYear,s])
    T1newp[s]<-sum(FT1new[1:n.allSiteYear,s])
    
    # SP-SPECIFIC BP.N
    Bp.N.sp[s] <- T1p[s] > T1newp[s]
    }
    
    # COMMUNITY BP.OBS
    Bp.Obs <- sum(T1obsnewp[1:nSpecies]) > sum(T1obsp[1:nSpecies])
    
    
    # COMMUNITY BP.N
    Bp.N <- sum(T1newp[1:nSpecies]) > sum(T1p[1:nSpecies])
    
    # Derived parameters
    
    for (s in 1:nSpecies){
    for (i in 1:nyears){
    Ntotal[i,s] <- sum(N[,s]*indexYears[,i]) }}
    
    }", fill=TRUE, 
    file = "s_HR_beta(2groups)_sigma[alpha(s)_obs(j,t)]_lambda(rest.sp)[alpha(s,t)_sp.site(s,j)_covAreas3(s,j,t)_covLands2(s,j,t)]_BPvaluesSP_resN.txt")

# Inits
Nst <- yLong.sp + 1
inits <- function(){list(mu_l = runif(1), sig_l = 0.2, sig_spsite = runif(1),
                         N=Nst, beta = runif(2), 
                         mu_a1 = runif(1), sig_a1 = runif(1), mu_a2 = runif(1), sig_a2 = runif(1),
                         mu_cd = runif(1), sig_cd = runif(1), mu_fs = runif(1), sig_fs = runif(1),
                         sig.sig.ob = runif(1),
                         mu_s = runif(1, log(30), log(50)) , sig_s = runif(1)
)}


# Params
params <- c( "mu_l", "sig_l", "sig_spsite", "beta", "p.g",
             "b.a1","mu_a1", "sig_a1", "b.a2", "mu_a2", "sig_a2", "b.a3", "mu_a3", "sig_a3",
             "bCropdiv", "mu_cd", "sig_cd", "bFieldsize", "mu_fs", "sig_fs",
             "sig.sig.ob", "Bp.N", "Bp.N.sp", "Bp.Obs", "Bp.Obs.sp",
             "mu_s", "sig_s")

# MCMC settings
nc <- 3 ; ni <- 200000 ; nb <- 30000 ; nt <- 10

# With jagsUI 
out <- jags(data1, inits, params, "s_HR_beta(2groups)_sigma[alpha(s)_obs(j,t)]_lambda(rest.sp)[alpha(s,t)_sp.site(s,j)_covAreas3(s,j,t)_covLands2(s,j,t)]_BPvaluesSP_resN.txt", n.chain = nc,
            n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)

setwd("D:/ANA/Results/chapter3")
save(out, file = "15.2_DATA_allsp_bp(resiN).RData")

print(out)

summary <- as.data.frame(as.matrix(out$summary))


traceplot(out, parameters = c("mu_l", "sig_l", "sig_spsite", "beta", "p.g",
                              "mu_a1", "sig_a1", "mu_a2", "sig_a2", "mu_a3", "sig_a3",
                              "mu_cd", "sig_cd", "mu_fs", "sig_fs",
                              "sig.sig.ob", "mu_s", "sig_s"))

###########################################################################################

load("D:/PhD/Third chapter/Data/Results_model/15.2_DATA_allsp_bp(resiN).RData")
print(out)

summary <- as.data.frame(as.matrix(out$summary))


t <- traceplot(out, parameters = c("mu_l", "sig_l", "sig_spsite", "beta",
                                   "mu_a1", "sig_a1", "mu_a2", "sig_a2", "mu_a3", "sig_a3",
                                   "mu_cd", "sig_cd", "mu_fs", "sig_fs",
                                   "sig.sig.ob",
                                   "mu_s", "sig_s"))

# ---- Process results ----
# 1. ---- Coefficients----

# Create data frame with species - coefficients together
sp.df <- data.frame(sp = sp, b.a1 = rownames(summary)[grep("b.a1", rownames(summary))],
                    b.a2 = rownames(summary)[grep("b.a2", rownames(summary))],
                    b.a3 = rownames(summary)[grep("b.a3", rownames(summary))],
                    bCropdiv = rownames(summary)[grep("bCropdiv", rownames(summary))],
                    bFieldsize = rownames(summary)[grep("bFieldsize", rownames(summary))],
                    Bp.N.sp = rownames(summary)[grep("Bp.N.sp", rownames(summary))],
                    Bp.Obs.sp = rownames(summary)[grep("Bp.Obs.sp", rownames(summary))] )

# Process samples
outall <- do.call(rbind,out$samples) # 3 chains together
coeff <- c("b.a1", "b.a2", "b.a3", "bCropdiv", "bFieldsize")
names <- c("b.SG", "b.AES", "b.GREEN", "bCropdiv", "bFieldsize")


for (c in 1:length(coeff)){
  
  # Sort species by the mean value of each coefficient and keep track of name of species and coef
  values <- summary[grep(coeff[c], rownames(summary)), ]
  values$param<- rownames(values)
  colnames(values)[which(colnames(values) == "param")] <- coeff[c]
  values2 <- left_join(values, sp.df)
  values_sorted <- arrange(values2, mean)
  sp_sorted <-  values_sorted$sp
  coef_sorted <- values_sorted[,which(colnames(values_sorted) %in% coeff[c])]
  
  setwd("D:/PhD/Third chapter/Data/Results_species")
  pdf(paste("15.2_DATA_allsp_resiN", names[c], ".pdf"))
  par(mfrow = c(5,4),
      mar = c(2,1,2,0.5)) 
  
  for (i in 1:nSpecies){
    dens_obs <- density(outall[ ,which(colnames(outall) == coef_sorted[i])]) # Density of iterations for coefficient
    mean_obs <- mean(outall[ ,which(colnames(outall) == coef_sorted[i])] )
    lci_obs  <- quantile(outall[ ,which(colnames(outall) == coef_sorted[i])], probs = 0.025) 
    uci_obs  <- quantile(outall[ ,which(colnames(outall) == coef_sorted[i])], probs = 0.975)
    
    
    # Plot
    
    plot(dens_obs, xlab = " ", ylab = " ", main = sp_sorted[i], axes = FALSE) 
    axis(1, pos = 0, tck = -0.02, cex.axis = 0.9, mgp = c(3, 0.2, 0))
    
    x1 <- min(which(dens_obs$x  >= lci_obs))  
    x2 <- max(which(dens_obs$x  <  uci_obs))
    polygon(x = c(dens_obs$x[c(x1,x1:x2, x2)]), y= c(0, dens_obs$y[x1:x2], 0), col="gray")
    
    segments(x0 = mean_obs, y0 = 0, x1 = , mean_obs, y1 = max(dens_obs$y)+2, col = "black", lwd = 1.2) #abline( a = 0,  v = mean_obs, col = "red", lwd = 1.5)
    segments(x0 = 0, y0 = 0, x1 = 0, y1 = max(dens_obs$y)+2, col = "red", lwd = 1.2, lty = 5)
  }
  mtext(names[c], line = -1, side = 1, outer = TRUE, cex = 1) 
  dev.off()
}

# 2. ---- Bp-values ----

sp.df <- data.frame(sp = sp,
                    Bp.N.sp = rownames(summary)[grep("Bp.N.sp", rownames(summary))],
                    Bp.Obs.sp = rownames(summary)[grep("Bp.Obs.sp", rownames(summary))] )

# Bp.Obs.sp
values <- summary[grep("Bp.Obs.sp", rownames(summary)), ]
values$param<- rownames(values)
colnames(values)[which(colnames(values) == "param")] <- "Bp.Obs.sp"
values2 <- left_join(values, sp.df)
df.bp_obs <- values2[,colnames(values2) %in% c("Bp.Obs.sp", "sp", "mean")]
bad_bp_obs <- df.bp_obs[which(df.bp_obs$mean < 0.1 | df.bp_obs$mean > 0.9), ]
nrow(bad_bp_obs)

# Bp.N.sp
values <- summary[grep("Bp.N.sp", rownames(summary)), ]
values$param<- rownames(values)
colnames(values)[which(colnames(values) == "param")] <- "Bp.N.sp"
values2 <- left_join(values, sp.df)
df.bp_n <- values2[,colnames(values2) %in% c("Bp.N.sp", "sp", "mean")]
bad_bp_n <- df.bp_n[which(df.bp_n$mean < 0.1 | df.bp_n$mean > 0.9), ]

###########################################################################################