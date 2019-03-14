
# Load packages

rm(list=ls())

library(rjags)
library(jagsUI)
library(dplyr)
library(rtrim)


###################################################################
##                       Prepare data                           ###
###################################################################

#setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data")
setwd("S:/PhD/Second chapter/Data")

d <- read.csv("DataDS_ready_ALL.csv")
colnames(d)[which(colnames(d) == "Count")] <- "Cluster" 

# Load species names
s <- read.csv("sp_trend_dg.csv", sep = ";")
s_good <- as.vector(s$Species[which(s$include_samplesize == 1)])
s_good <- s_good[42]
s_doubt <- as.vector(s$Species[which(s$Doubt_samplesize == 1)])

# Start loop
for (xxx in 1:length(s_good)){

# To take into account transects with abundance 0
# 1. Select all transects IDs from all species observations
# 2. Join the observations of MECAL (for example) with all transects so that they remain with NA if the
# species was there but it wasnt sampled

d_tr <- d[ ,which(colnames(d) %in% c("Species",  "T_Y", "Observer"))]
d_tr_all <- data.frame(T_Y = unique(d_tr$T_Y), id = NA)

d_tr$Observer <- as.character(d_tr$Observer) 
d_tr_all_obs <- left_join(d_tr_all, d_tr)
d_tr_all_obs <- d_tr_all_obs[ ,c(1,4)]
d_tr_all_obs <- d_tr_all_obs[which(!duplicated(d_tr_all_obs)), ] # Table with all sampled fields and which observer sampled it


sp <- d[which(d$Species == s_good[xxx]), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "Observer", "Cluster"))] # Select species spAL and all years
sp <- arrange(sp, Year, transectID) #Ordered
sp_detec_transectID <- unique(sp$transectID)
sp$Observer <- as.character(sp$Observer) 

absent <- anti_join(d_tr_all,sp) # Transects with 0 abundance, add to sp.
colnames(absent)[2] <- "Banda" # Format it to add the rows to sp
absent$T_Y <- as.character(absent$T_Y)
absent$Species <- s_good[xxx]
absent$Cluster <- NA
absent <- left_join(absent, d_tr_all_obs)


for (i in 1:nrow(absent)){ # Format to join absent - detections
  cent <- substr(absent$T_Y[i], 10,10) # To include SI102 (cents)
  cent <- as.numeric(cent)
  if(is.na(cent)){
    
    absent$Year[i] <- substr(absent$T_Y[i], 6,9)
    absent$transectID[i] <- substr(absent$T_Y[i], 1,4)
    
  } else { absent$Year[i] <- substr(absent$T_Y[i], 7,10)
  absent$transectID[i] <- substr(absent$T_Y[i], 1,5)}
}
absent$count <- 0
sp$count <- 1
all_sp <- rbind(sp,absent) # Include transects with abundance 0
all_sp <- arrange(all_sp, Year, transectID) # Ordered

absent$count <- 0

###################################################################
##                       HDS ANALYSIS                           ###
###################################################################

# ---- Information: bins, years, sites ----

strip.width <- 200 				
dist.breaks <- c(0,25,50,100,200)
int.w <- diff(dist.breaks) # width of distance categories (v)
midpt <- diff(dist.breaks)/2+dist.breaks[-5]
nG <- length(dist.breaks)-1

yrs <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018) # I HAVE TO CONVERT THIS FROM 0-7 (but nyrs is still 8!)
nyrs <- length(yrs)

# ---- Distance observations ----

# Format
all.sites <- unique(all_sp$transectID)
all.sites <- sort(all.sites,descreasing = TRUE)
max.sites <- length(all.sites)

m <- matrix(NA, nrow = length(all.sites), ncol = nyrs)
rownames(m) <- all.sites
colnames(m) <- yrs

# Add counts > 0
count <- aggregate(Species ~ Year + transectID, FUN = length, data = sp)

for (i in 1:nrow(count)){
  m[which(rownames(m) %in% count$transectID[i]), which(colnames(m) %in% count$Year[i])] <- count$Species[i]
}

# Add absences (0)
for (i in 1:nrow(absent)){
  m[which(rownames(m) %in% absent$transectID[i]), which(colnames(m) %in% absent$Year[i])] <- absent$count[i]
}

# Only to check: Count of individuals per year
count.year <- colSums(m,na.rm = TRUE)

# Count of individuals per year corrected by cluster size
average_clus <- mean(sp$Cluster) # TO INCLUDE IN THE MODEL
count.year_clus <- count.year*average_clus


# Zone (Occidental = 0; Oriental = 1)
zone <- as.data.frame(m)
zone_codi <- as.vector(rownames(zone))
zone$Codi <- zone_codi

for (i in 1:nrow(zone)){
  if(substr(zone$Codi[i], 1,2) == "BA"){zone[i,1:9] <- 0}
  if(substr(zone$Codi[i], 1,2) == "BM"){zone[i,1:9] <- 1}
  if(substr(zone$Codi[i], 1,2) == "SI"){zone[i,1:9] <- 1}
  if(substr(zone$Codi[i], 1,2) == "AF"){zone[i,1:9] <- 0}
  if(substr(zone$Codi[i], 1,2) == "BE"){zone[i,1:9] <- 1}
  if(substr(zone$Codi[i], 1,2) == "GR"){zone[i,1:9] <- 0}
}
zone <- zone[,-10]

# Year
yrs2 <- c(0, 1, 2, 3, 4, 5, 6, 7, 8) # To make it as a continuous variable, otherwise it doesnt work
year <- matrix(NA,nrow = max.sites, ncol = nyrs)
colnames(year) <- yrs
for (i in 1:nyrs){
  year[ ,which(colnames(year) %in% yrs[i])] <- rep(yrs2[i], max.sites)
}

# Observer 
# Format
obs <- matrix(NA, nrow = max.sites, ncol = nyrs)
rownames(obs) <- all.sites
colnames(obs) <- yrs

# Add observers for fields with counts > 0
for (i in 1:nrow(sp)){
  obs[which(rownames(obs) %in% sp$transectID[i]), which(colnames(obs) %in% sp$Year[i])] <- sp$Observer[i]
}

# Add observers for fields with absences (0)
for (i in 1:nrow(absent)){
  obs[which(rownames(obs) %in% absent$transectID[i]), which(colnames(obs) %in% absent$Year[i])] <- absent$Observer[i]
}

# ---- Specify data in JAGS format ----

# Distance class and ind
nind <- nrow(sp)
dclass <- sp$Banda


# Get one long vector with counts per year and site
yLong <- unlist(as.data.frame(m), use.names = F)


sitesYears <- NULL
for (i in 1:nyrs){
  sitesYears <- c(sitesYears,c(1:length(all.sites)))}

# Get one long vector for each site-year combination of each dclass observation
###RS: Fixed index to map dclass onto site-year combinations
# For the index, create a vector of ylong where NA are 0 (because I need the same length)
yLong_index <- yLong
yLong_index[which(is.na(yLong_index))] <- 0

n.allSiteYear <- max.sites*nyrs 
siteYear.dclass <- NULL

for (i in 1:n.allSiteYear){
  siteYear.dclass <- c(siteYear.dclass,rep(i, yLong_index[i]))} 

# Get one vector per co-variate
zon <- NULL
for (i in 1:nyrs){
  zon <- c(zon,zone[1:length(all.sites),i])}

year1 <- NULL
for (i in 1:nyrs){
  year1 <- c(year1,year[1:length(all.sites),i])}

ob <- NULL
for (i in 1:nyrs){
  ob <- c(ob,obs[1:length(all.sites),i])}
ob <- as.numeric(factor(ob)) # JAGS doesn't accept categorical variables

obs_id <- unique(ob)[-1]
ob[which(is.na(ob))] <- sample(obs_id, length(which(is.na(ob))), replace = TRUE)

nobs <- length(unique(ob))

# Create one matrix for indexing year when calculating abundance per year in JAGS
allyears <- NULL 
for (i in 1:nyrs){
  allyears <- c(allyears,rep(yrs[i],length(all.sites)))
}
ye <- data.frame(allyears = allyears)
ye$allyears <- as.factor(ye$allyears)
indexYears <- model.matrix(~ allyears-1, data = ye)

# ---- Compile data for JAGS model ----

data1 <- list(nyears = nyrs, max.sites = max.sites, nG = nG, siteYear.dclass = siteYear.dclass, int.w=int.w, strip.width = strip.width, 
              y = yLong, nind = nind, dclass = dclass, midpt = midpt, sitesYears = sitesYears, indexYears = indexYears,
              zoneB = zon, ob = ob, nobs = nobs, db = dist.breaks, year1 = year1, average_clus = average_clus)

# ---- JAGS model ----

setwd("S:/PhD/Second chapter/Data/Model")
cat("model{
    
    # PRIORS
    
    # Priors for lambda
    bzB.lam ~ dnorm(0, 0.001)
    bYear.lam ~ dnorm(0, 0.001)
    
    mu.lam ~ dunif(-10, 10) # Random effects for lambda per site
    sig.lam ~ dunif(0, 10)
    tau.lam <- 1/(sig.lam*sig.lam)
    
    r ~ dunif(0,100) # Scale and shape from dgamma
    
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
    N[j] ~ dpois(lambda.star[j]) 
    lambda[j] <- exp(log.lambda[sitesYears[j]] + bzB.lam*zoneB[j] + bYear.lam*year1[j])
    rho[j] ~ dgamma(r,r) # Dispersion parameter
    lambda.star[j] <- rho[j]*lambda[j] # Lambda corrected by dispersion parameter(?)
    }
    
    # Derived parameters
    for (i in 1:nyears){
    Ntotal[i] <- sum(N*indexYears[,i]) 
    ### RS: added this in to calculate total expected abundance for the Poisson mean
    ###    and the effective NegBin mean
    lamtotal[i]<- sum(lambda.star*indexYears[,i]) 
    lamtotal.P[i]<- sum(lambda*indexYears[,i])
    }
    
    for (i in 1:nyears){
    Ntotal_clus[i] <- average_clus*(sum(N*indexYears[,i]))
    ### RS: added this in to calculate total expected abundance for the Poisson mean
    ###    and the effective NegBin mean
    lamtotal_clus[i] <- average_clus*(lamtotal[i])
    lamtotal_clus.P[i] <- average_clus*(lamtotal.P[i])
    }
    
    }",fill=TRUE, file = "s_sigma(integral)[obs(o,j,t)_covZone(j)]_lambda(PoisGam)[alpha(j)_covZone(j)_year(j)]_clustersize.txt")

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
params <- c("Ntotal", "Ntotal_clus", #"N", "sigma", "lambda", I remove it so that it doesnt save the lambdas and takes shorter. It still calculates them
            "mu.lam", "sig.lam", 'r',
            "bzB.lam", "bYear.lam",
            "mu.sig", "sig.sig", "bzB.sig",
            #### RS: added these in so you can plot them and compare to Ntotal
            "lamtotal_clus", "lamtotal_clus.P",
            "lamtotal", "lamtotal.P"
)

# MCMC settings
nc <- 3 ; ni <- 30000 ; nb <- 2000 ; nt <- 2

# With jagsUI 
out2 <- jags(data1, inits, params, "s_sigma(integral)[obs(o,j,t)_covZone(j)]_lambda(PoisGam)[alpha(j)_covZone(j)_year(j)]_clustersize.txt", n.chain = nc,
             n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
print(out2)
#s2 <- as.data.frame(out2$summary)
setwd("S:/PhD/Second chapter/Data/Results/TRIM/Try1")
save(out2, file = paste("HDS_",s_good[xxx],".RData", sep = ""))


# ---- Results ----

setwd("S:/PhD/Second chapter/Data/Results/TRIM/Try1")
load(paste("HDS_",s_good[xxx],".RData", sep = ""))


summary <- as.data.frame(as.matrix(out2$summary))

results <- summary[which(rownames(summary) %in% c("Ntotal_clus[1]", "Ntotal_clus[2]", "Ntotal_clus[3]", "Ntotal_clus[4]", "Ntotal_clus[5]", "Ntotal_clus[6]", "Ntotal_clus[7]", "Ntotal_clus[8]", "Ntotal_clus[9]",
                                                  "mu.lam", "sig.lam", "bzB.lam", "bYear.lam")), ]

## ---- POPULATION TREND PLOT ---- ##

# Based on expected N

yrs2 <- c(0, 1, 2, 3, 4, 5, 6, 7, 8) 

# 1. Calculate predictions for both zones

outall <- do.call(rbind,out2$samples) 

# Total population (since both follow the same trend)
pred.exp <- matrix(NA, dim(outall)[1], length(yrs2))
for(i in 1:dim(outall)[1]){ 
  ##calculate population, year 1
  pred.exp[i,1] <- as.vector(outall[i,"lamtotal_clus[1]"])
  ##calculate populations, year 2-8, based on beta(Year)
  for (t in 2:length(yrs2)){
    pred.exp[i,t] <- pred.exp[i,(t-1)] * # Here I add the starting population size as a baseline for the trend 
      exp(outall[i,"bYear.lam"])
  }
}

predall.exp <- pred.exp
lci.exp <- uci.exp <- mean.pred.exp <- 0 

for(i in 1:length(yrs2)){
  lci.exp[i]  <- quantile(predall.exp[,i],probs = 0.025) 
  uci.exp[i]  <- quantile(predall.exp[,i],probs = 0.975)
  mean.pred.exp[i]  <- mean(predall.exp[,i])
}


# 2. Plot

setwd("S:/PhD/Second chapter/Data/Results/Plots/8TRIM/Try1")
pdf(paste(s_good[xxx],"_TrimComp.pdf", sep = ""), height = 5, width = 9)

par(mfrow = c(1,2))

plot(-15, xlim=c(0,8), ylim=c(0,max(uci.exp)+20), main = " ", xlab = "Year", ylab = "Abundance")
mtext("HDS", side = 3, line = 1, cex = 1.2)


polygon( x = c(yrs2, rev(yrs2)),
         y = c(lci.exp, rev(uci.exp)), 
         col = adjustcolor(c("grey"),alpha.f = 0.6),
         border = NA)
points(mean.pred.exp ~ yrs2, type="l", col = "red")

##add in actual abundance estimates to check

points(yrs2, out2$summary[grep("Ntotal_clus", rownames(out2$summary)),1], pch = 19, type = "l", col = "blue")
points(yrs2, out2$summary[grep("Ntotal_clus", rownames(out2$summary)),1], pch = 19)

# Print estimate
est <- round(results[13,1],2)

significance_est <- ifelse(results[13,10] == 0, 
                           paste(est,"*"), 
                           est)
col_est <- ifelse(est>0, "blue", "red")

text(7.5,1.5, significance_est, col = col_est)

###################################################################
##                       TRIM ANALYSIS                          ###
###################################################################

# ---- Subset for trim analysis ----
sp <- all_sp[, which(colnames(all_sp) %in% c("Year", "transectID", "count"))] # Select species MECAL and all years
colnames(sp)[which(colnames(sp) %in% "transectID")] <- "site"
colnames(sp)[which(colnames(sp) %in% "Year")] <- "year"
sp$year <- as.integer(sp$year)

g <- aggregate(count ~ year, FUN = sum, data = sp)
sp <- aggregate(count ~ year + site, FUN = sum, data = sp)

check_observations(sp, model = 2)

# ---- MODEL 2 ----

m2 <- trim(count ~ site + year, data = sp, model = 2)
coefficients(m2)
i2<-index(m2, which="both")


#Extract the coefficients
coef <- coefficients(m2)
sig <- wald(m2)
tot <- totals(m2)

#Plot with overall slope

plot(overall(m2))
mtext("TRIM", side = 3, line = 1, cex = 1.2)

# Print estimate
est <- round(coef$add[1], 2)

significance_est <- ifelse(sig$slope$p < 0.05, 
                           paste(est,"*"), 
                           est)
col_est <- ifelse(est > 0, "blue", "red")

text(2017.5,1.5, significance_est, col = col_est)

title(s_good[xxx], line = -1, cex = 2, outer = TRUE)

dev.off()

print(s_good[xxx])

}