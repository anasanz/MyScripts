
# Load packages

rm(list=ls())

library(rjags)
library(jagsUI)
library(dplyr)
library(rtrim)

# 3.1. Tries to add vegetation height as covariate, bit is not finished because I decided to not do it

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
problems <- c("CIJUN", "COCOT", "OEHIS", "TUMER", "TUVIS")
s_good <- s_good[-which(s_good %in% problems)]
s_doubt <- as.vector(s$Species[which(s$Doubt_samplesize == 1)])

# Start loop
xxx = 2

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

# Load vegetation co-variate
setwd("S:/PhD/Second chapter/Data")
v <- read.csv("veg_variable.csv", sep = ",") 

# Cereal height. Use as habitat covariate in sigma (not fallow height because it has more NA)
vegCereal <- matrix(NA, nrow = length(all.sites), ncol = nyrs)
rownames(vegCereal) <- all.sites
colnames(vegCereal) <- yrs

for (i in 1:nrow(v)){
  vegCereal[which(rownames(vegCereal) %in% v$transectID[i]), which(colnames(vegCereal) %in% v$Year[i])] <- v$heightCereal[i]
}


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

m  # Counts per year and site

# Co-variates
zon <- as.vector(zone[,1])

yrs <- 1:9 
year_number <- 0:8


# Matrix with observers
ob <- matrix(as.numeric(factor(obs)), nrow = max.sites, ncol = nyrs) # JAGS doesn't accept categorical variables
unique(factor(ob))
obs_id <- unique(factor(ob))[-1]
ob[which(is.na(ob))] <- sample(obs_id, length(which(is.na(ob))), replace = TRUE) # No NA in covariate

nobs <- length(unique(factor(ob)))

# Index for random effects
site <- c(1:max.sites)
year <- c(1:nyrs)

sitesYears <- NULL
for (i in 1:nyrs){
  sitesYears <- c(sitesYears,c(1:length(all.sites)))}

# Fixed index to map dclass onto site and year 
# For the index, create a matrix m where NA are 0 (because I need the same length)

m_index <- m
m_index[which(is.na(m_index))] <- 0

site.dclass <- year.dclass <- NULL

for (t in 1:nyrs){ # sites has to be nested on years because dclass first indexes the sites on the same year
  for (j in 1:max.sites){
    site.dclass <- c(site.dclass, rep(j, m_index[j,t]))
    year.dclass <- c(year.dclass, rep(t, m_index[j,t]))
  } }


# ---- Compile data for JAGS model ----

data1 <- list(nyears = nyrs, nsites = max.sites, nG=nG, int.w=int.w, strip.width = strip.width, midpt = midpt, db = dist.breaks,
              year.dclass = year.dclass, site.dclass = site.dclass, y = m, nind = nind, dclass = dclass,
              zoneB = zon, ob = ob, nobs = nobs, year1 = year_number, site = site, year_index = yrs)

# ---- JAGS model ----

setwd("S:/PhD/Second chapter/Data/Model")
cat("model{
    
    # PRIORS
    
    # Priors for lambda
    rho ~ dunif(-1,1) # Autorregresive parameter (serial AC)
    tau <- pow(sd, -2) # Prior for overdispersion in eps
    sd ~ dunif(0, 3)
    
    bYear.lam ~ dnorm(0, 0.001) # Prior for the trend
    
    # Random effects for lambda per site
    mu.lam.site ~ dunif(-10, 10) 
    sig.lam.site ~ dunif(0, 10)
    tau.lam.site <- 1/(sig.lam.site*sig.lam.site)
    
    for (j in 1:nsites){
    log.lambda.site[j] ~ dnorm(mu.lam.site, tau.lam.site)
    }
    
    # Random effects for lambda per year
    sig.lam.year ~ dunif(0, 10) 
    tau.lam.year <- 1/(sig.lam.year*sig.lam.year)
    
    log.lambda.year[1] <- 0
    for (t in 2:nyears){
    log.lambda.year[t] ~ dnorm(0, tau.lam.year)
    }
    
    
    # Priors for sigma
    bzB.sig ~ dnorm(0, 0.001)
    
    mu.sig ~ dunif(-10, 10) # Random effects for sigma per observer
    sig.sig ~ dunif(0, 10)
    tau.sig <- 1/(sig.sig*sig.sig)
    
    # Random observer effect for sigma
    for (o in 1:nobs){
    sig.obs[o] ~ dnorm(mu.sig, tau.sig)
    }
    
    # Random effects for sigma per year
    sig.sig.year ~ dunif(0, 10) 
    tau.sig.year <- 1/(sig.sig.year*sig.sig.year)
    
    for (t in 1:nyears){
    log.sigma.year[t] ~ dnorm(0, tau.sig.year)
    }
    
    for(i in 1:nind){
    dclass[i] ~ dcat(fct[site.dclass[i], year.dclass[i], 1:nG]) 
    }
    
    # LIKELIHOOD
    
    # FIRST YEAR
    for(j in 1:nsites){ 
    
    sigma[j,1] <- exp(sig.obs[ob[j,1]] + bzB.sig*zoneB[j] + log.sigma.year[year_index[1]])
    
    # Construct cell probabilities for nG multinomial cells (distance categories) PER SITE
    
    for(k in 1:nG){ 
    
    up[j,1,k]<-pnorm(db[k+1], 0, 1/sigma[j,1]^2) ##db are distance bin limits
    low[j,1,k]<-pnorm(db[k], 0, 1/sigma[j,1]^2) 
    p[j,1,k]<- 2 * (up[j,1,k] - low[j,1,k])
    pi[j,1,k] <- int.w[k] / strip.width 
    f[j,1,k]<- p[j,1,k]/f.0[j,1]/int.w[k]                   ## detection prob. in distance category k                      
    fc[j,1,k]<- f[j,1,k] * pi[j,1,k]                 ## pi=percent area of k; drops out if constant
    fct[j,1,k]<-fc[j,1,k]/pcap[j,1] 
    }
    
    pcap[j,1] <- sum(fc[j,1, 1:nG]) # Different per site and year (sum over all bins)
    
    f.0[j,1] <- 2 * dnorm(0,0, 1/sigma[j,1]^2) # Prob density at 0
    
    
    y[j,1] ~ dbin(pcap[j,1], N[j,1]) 
    N[j,1] ~ dpois(lambda[j,1]) 
    
    lambda[j,1] <- exp(log.lambda.site[site[j]] + log.lambda.year[year_index[1]] + bYear.lam*year1[1] + w[j,1]) # year1 is t-1; year_index is t (to index properly the random effect)
    w[j,1] <- eps[j,1] / sqrt(1 - rho * rho)
    eps[j,1] ~ dnorm(0, tau)
    }
    
    #############
    # LATER YEARS
    for(j in 1:nsites){ 
    for (t in 2:nyears){
    
    sigma[j,t] <- exp(sig.obs[ob[j,t]] + bzB.sig*zoneB[j] + log.sigma.year[year_index[t]])
    
    # Construct cell probabilities for nG multinomial cells (distance categories) PER SITE
    
    for(k in 1:nG){ 
    
    up[j,t,k]<-pnorm(db[k+1], 0, 1/sigma[j,t]^2) ##db are distance bin limits
    low[j,t,k]<-pnorm(db[k], 0, 1/sigma[j,t]^2) 
    p[j,t,k]<- 2 * (up[j,t,k] - low[j,t,k])
    pi[j,t,k] <- int.w[k] / strip.width 
    f[j,t,k]<- p[j,t,k]/f.0[j,t]/int.w[k]                   ## detection prob. in distance category k                      
    fc[j,t,k]<- f[j,t,k] * pi[j,t,k]                 ## pi=percent area of k; drops out if constant
    fct[j,t,k]<-fc[j,t,k]/pcap[j,t] 
    }
    
    pcap[j,t] <- sum(fc[j,t, 1:nG]) # Different per site and year (sum over all bins)
    
    f.0[j,t] <- 2 * dnorm(0,0, 1/sigma[j,t]^2) # Prob density at 0
    
    
    y[j,t] ~ dbin(pcap[j,t], N[j,t]) 
    N[j,t] ~ dpois(lambda[j,t]) 
    
    lambda[j,t] <- exp(log.lambda.site[site[j]] + log.lambda.year[year_index[t]] + bYear.lam*year1[t] + w[j,t])
    w[j,t] <- rho * w[j,t-1] + eps[j,t]
    eps[j,t] ~ dnorm(0, tau)
    }
    }
    
    # Derived parameters
    
    for(t in 1:nyears){
    popindex[t] <- sum(lambda[,t])
    }
    
    # Expected abundance per year inside model
    
    lam.tot[1] <- popindex[1] # Expected abundance in year 1
    for (i in 2:nyears){
    lam.tot[i] <- lam.tot[i-1] * # Here I add the starting population size as a baseline for the trend 
    exp(bYear.lam)}
    
    
    }",fill=TRUE, file = "s_sigma(integral)[obs(o,j,t)_covZone(j)_year.random(t)]_lambda[alpha.site.random(j)_year.random(t)_beta.year(j)_w].txt")


# Inits
Nst <- m + 1
inits <- function(){list(mu.sig = runif(1, log(30), log(50)), sig.sig = runif(1), bzB.sig = runif(1),
                         mu.lam.site = runif(1), sig.lam.site = 0.2, sig.lam.year = 0.3, bYear.lam = runif(1),
                         N = Nst)} 

# Params
params <- c( "mu.sig", "sig.sig", "bzB.sig", "sig.obs", "log.sigma.year", # Save also observer effect
             "mu.lam.site", "sig.lam.site", "sig.lam.year", "bYear.lam", "log.lambda.year", # Save year effect
             "popindex", "sd", "rho", "lam.tot"
)

# MCMC settings
nc <- 3 ; ni <- 60000 ; nb <- 4000 ; nt <- 4

# With jagsUI 
out <- jags(data1, inits, params, "s_sigma(integral)[obs(o,j,t)_covZone(j)_year.random(t)]_lambda[alpha.site.random(j)_year.random(t)_beta.year(j)_w].txt", n.chain = nc,
            n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
summary <- out$summary
print(out)

setwd("S:/PhD/Second chapter/Data/Results/TRIM/3autoreg_simple")
save(out, file = paste("HDS_",s_good[xxx],".RData", sep = ""))


# ---- Results ----

setwd("S:/PhD/Second chapter/Data/Results/TRIM/3autoreg_simple")
load(paste("HDS_",s_good[xxx],".RData", sep = ""))


summary <- as.data.frame(as.matrix(out$summary))

results <- summary[which(rownames(summary) %in% c("popindex[1]", "popindex[2]", "popindex[3]", "popindex[4]", "popindex[5]", "popindex[6]", "popindex[7]", "popindex[8]", "popindex[9]",
                                                  "mu.lam.site", "sig.lam.site", "bzB.sig" , "bYear.lam")), ]

## ---- POPULATION TREND PLOT ---- ##

# Based on expected N

yrs2 <- c(0, 1, 2, 3, 4, 5, 6, 7, 8) 

# 1. Calculate predictions for both zones

outall <- do.call(rbind,out$samples) 

# Total population (since both follow the same trend)
pred.exp <- matrix(NA, dim(outall)[1], length(yrs2))
for(i in 1:dim(outall)[1]){ 
  ##calculate population, year 1
  pred.exp[i,1] <- as.vector(outall[i,"lam.tot[1]"])
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

points(yrs2, out$summary[grep("popindex", rownames(out$summary)),1], pch = 19, type = "l", col = "blue")
points(yrs2, out$summary[grep("popindex", rownames(out$summary)),1], pch = 19)

# Print estimate
est <- round(results[4,1],2)

significance_est <- ifelse(results[4,10] == 0, 
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

# ---- MODEL 2 ---- ONLY FOR SIGNIFICANCE
m2 <- trim(count ~ site + year, data = sp, model = 2)
sig <- wald(m2) # FOR THE MOMENT: Add the significance from the slope parameter, as in the HDS model (bYear)
coefficients(m2) 

# ---- MODEL 3 ----
m3 <- trim(count ~ site + year, data = sp, model = 3)
i3 <- index(m3, which="both")


#Extract the coefficients
coef <- coefficients(m3, representation = c("trend"))
sig_dev <- wald(m3) 
tot <- totals(m3)

# Save deviations
coef_dev <- coefficients(m3, representation = c("deviations"))

#Plot with overall slope

plot(overall(m3))
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
