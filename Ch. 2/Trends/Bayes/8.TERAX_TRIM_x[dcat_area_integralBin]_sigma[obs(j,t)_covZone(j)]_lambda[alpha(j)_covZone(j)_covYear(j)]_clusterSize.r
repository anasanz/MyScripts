rm(list=ls())

library(rjags)
library(jagsUI)
library(dplyr)

# Run model 8.2 in BUOED dataset 

# ---- Data ----
setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data")
d <- read.csv("DataDS_ready_ALL.csv")

# Information: bins, years, sites

strip.width <- 200 				
dist.breaks <- c(0,25,50,100,200)
int.w <- diff(dist.breaks) # width of distance categories (v)
midpt <- diff(dist.breaks)/2+dist.breaks[-5]
nG <- length(dist.breaks)-1

yrs <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
nyrs <- length(yrs)

# To take into account transects with abundance 0
# 1. Select all transects IDs from all species observations
# 2. Join the observations of BUOED (for example) with all transects so that they remain with NA if the
# species was there but it wasnt sampled

d_tr <- d[ ,which(colnames(d) %in% c("Species",  "T_Y", "Observer"))]
d_tr_all <- data.frame(T_Y = unique(d_tr$T_Y), id = NA)


# For observer variable
d_tr$Observer <- as.character(d_tr$Observer)
d_tr_all_obs <- left_join(d_tr_all, d_tr)
d_tr_all_obs <- d_tr_all_obs[ ,c(1,4)]
d_tr_all_obs <- d_tr_all_obs[which(!duplicated(d_tr_all_obs)), ] # Table with all sampled fields and which observer sampled it
d_tr_all_obs$Observer <- as.character(d_tr_all_obs$Observer)
d_tr_all_obs$T_Y <- as.character(d_tr_all_obs$T_Y)


mec <- d[which(d$Species == "TERAX"), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "Observer", "Count"))] # Select species BUOED and all years
colnames(mec)[which(colnames(mec) == "Count")] <- "Cluster" # To not be confused later
mec <- arrange(mec, Year, transectID) #Ordered
mec_detec_transectID <- unique(mec$transectID)
mec$Observer <- as.character(mec$Observer) 


absent <- anti_join(d_tr_all,mec) # Transects with 0 abundance, add to mec.
colnames(absent)[2] <- "Banda" # Format it to add the rows to mec
absent$T_Y <- as.character(absent$T_Y)
absent$Species <- "TERAX"
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

all_mec <- rbind(mec,absent) # Include transects with abundance 0
all_mec <- arrange(all_mec, Year, transectID) # Ordered

absent$count <- 0


# ---- Distance observations ----

# Format
all.sites <- unique(all_mec$transectID)
all.sites <- sort(all.sites,descreasing = TRUE)
max.sites <- length(all.sites)

m <- matrix(NA, nrow = length(all.sites), ncol = nyrs)
rownames(m) <- all.sites
colnames(m) <- yrs

# Add counts > 0
count <- aggregate(Species ~ Year + transectID, FUN = length, data = mec)

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
average_clus <- mean(mec$Cluster) # TO INCLUDE IN THE MODEL
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
for (i in 1:nrow(mec)){
  obs[which(rownames(obs) %in% mec$transectID[i]), which(colnames(obs) %in% mec$Year[i])] <- mec$Observer[i]
}

# Add observers for fields with absences (0)
for (i in 1:nrow(absent)){
  obs[which(rownames(obs) %in% absent$transectID[i]), which(colnames(obs) %in% absent$Year[i])] <- absent$Observer[i]
}

# ---- Specify data in JAGS format ----

# Distance class and ind
nind <- nrow(mec)
dclass <- mec$Banda


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
    
    for (i in 1:nyears){
    Ntotal_clus[i] <- average_clus*(sum(N*indexYears[,i]))
    }
    
    }",fill=TRUE, file = "s_sigma(integral)[obs(o,j,t)_covZone(j)]_lambda[alpha(j)_covZone(j)_year(j)]_clustersize.txt")

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
            "mu.lam", "sig.lam", 
            "bzB.lam", "bYear.lam",
            "mu.sig", "sig.sig", "bzB.sig"
)

# MCMC settings
nc <- 3 ; ni <- 15000 ; nb <- 2000 ; nt <- 2

# With jagsUI 
out <- jags(data1, inits, params, "s_sigma(integral)[obs(o,j,t)_covZone(j)]_lambda[alpha(j)_covZone(j)_year(j)]_clustersize.txt", n.chain = nc,
            n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
print(out)

summary <- as.data.frame(as.matrix(out$summary))

setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/Results/TRIM")
write.csv(summary, "8.TRIM_Terax.csv")

###################################################################

setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/Results/TRIM")
summary <- read.csv("8.TRIM_Terax.csv")

results <- summary[which(summary$X %in% c("Ntotal_clus[1]", "Ntotal_clus[2]", "Ntotal_clus[3]", "Ntotal_clus[4]", "Ntotal_clus[5]", "Ntotal_clus[6]", "Ntotal_clus[7]", "Ntotal_clus[8]", "Ntotal_clus[9]",
                                          "mu.lam", "sig.lam", "bzB.lam", "bYear.lam")), ]


# Plot the trend of the population
plot(-100,ylim = c(0,150), xlim=c(0,9),
     pch = 21, ylab = "N", xlab = " ", axes = FALSE)
mtext("DS", side = 3, line = 1, cex = 1.5)
axis(1, at = c(1,2,3,4,5,6,7,8,9), labels = yrs)
axis(2)
points(results[1:9,2],pch = 19) # Plot results
points(count.year_clus,pch = 19, col = "red") # Plot counts
x <- seq_along(results[1:9,2])
low_CI <- as.numeric(results$X2.5.[1:9])
up_CI <- as.numeric(results$X97.5.[1:9])
arrows(x, low_CI,x, up_CI, code=3, angle=90, length=0.04) 

title("LITTLE BUSTARD", line = -1, cex = 2, outer = TRUE)

# Plot YEAR effect
setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/Results/Plots/8TRIM")
pdf("Terax_Year.pdf")

pred <- exp(results[which(results$X == "mu.lam"),2]+ # Add the intercept (random effect), also fixed to the mean of the random effect
              results[which(results$X == "bzB.lam"),2]*1 + # Prediction for fixed zone 1 (ORIENTAL)
              results[which(results$X == "bYear.lam"),2]*yrs2) 

predlci <- exp(results[which(results$X == "mu.lam"),4]+ # Add the intercept (random effect), also fixed to the mean of the random effect
                 results[which(results$X == "bzB.lam"),4]*1 + # Prediction for fixed zone 1 (ORIENTAL)
                 results[which(results$X == "bYear.lam"),4]*yrs2) 

preduci <- exp(results[which(results$X == "mu.lam"),8]+ # Add the intercept (random effect), also fixed to the mean of the random effect
                 results[which(results$X == "bzB.lam"),8]*1 + # Prediction for fixed zone 1 (ORIENTAL)
                 results[which(results$X == "bYear.lam"),8]*yrs2) 

plot(pred ~ yrs2, ylim=c(0,1), type="l", main = "Little bustard", xlab = "Year", ylab = "Abundance")
#points(predlci ~ area_SGpred, pch=16, type="l",lty=2)
#points(preduci ~ area_SGpred, pch=16,type="l",lty=2)
polygon( x = c(yrs2, rev(yrs2)),
         y = c(predlci, rev(preduci)), 
         col = adjustcolor(c("grey"),alpha.f = 0.6),
         border = NA)


pred0 <- exp(results[which(results$X == "mu.lam"),2]+
               results[which(results$X == "bzB.lam"),2]*0 + # Prediction fixed for zone 0 (occidental)
               results[which(results$X == "bYear.lam"),2]*yrs2) 

pred0lci <- exp(results[which(results$X == "mu.lam"),4]+ # PREDICTION LOW CI FOR OCCIDENTAL
                  results[which(results$X == "bzB.lam"),4]*0 + 
                  results[which(results$X == "bYear.lam"),4]*yrs2) 

pred0uci <- exp(results[which(results$X == "mu.lam"),8]+ # PREDICTION UP CI FOR OCCIDENTAL
                  results[which(results$X == "bzB.lam"),8]*0 + 
                  results[which(results$X == "bYear.lam"),8]*yrs2) 

points(pred0 ~ yrs2, pch=16, type="l", col="red")
#points(pred0lci ~ area_SGpred, pch=16, type="l",lty=2, col="red")
#points(pred0uci ~ area_SGpred, pch=16,type="l",lty=2, col="red")
polygon( x = c(yrs2, rev(yrs2)),
         y = c(pred0lci, rev(pred0uci)), 
         col = adjustcolor(c("red"),alpha.f = 0.2),
         border = NA)
legend("topleft",fill=adjustcolor(c("red","black"),alpha.f = 0.8),
       border=c("red","black"),legend = c("Occidentals", "Orientals"),
       box.lwd=0.1,
       bty = "n")

#points(pred ~ area_SG_HA, pch=16, type="l")

dev.off()



