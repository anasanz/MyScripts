rm(list=ls())

library(rjags)
library(jagsUI)
library(dplyr)

# Run model 8.2 in MECAL dataset 
# ---- I ignore counts in each observation (cluster size)

# ---- Data ----
setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data")
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
# 2. Join the observations of MECAL (for example) with all transects so that they remain with NA if the
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


mec <- d[which(d$Species == "MECAL"), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "Observer", "Count"))] # Select species MECAL and all years
colnames(mec)[which(colnames(mec) == "Count")] <- "Cluster" # To not be confused later
mec <- arrange(mec, Year, transectID) #Ordered
mec_detec_transectID <- unique(mec$transectID)
mec$Observer <- as.character(mec$Observer) 


absent <- anti_join(d_tr_all,mec) # Transects with 0 abundance, add to mec.
colnames(absent)[2] <- "Banda" # Format it to add the rows to mec
absent$T_Y <- as.character(absent$T_Y)
absent$Species <- "MECAL"
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

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/Results/TRIM")
save(out, file = "8.TRIM_Mecal.RData")

###################################################################

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/Results/TRIM")
load("8.TRIM_Mecal.RData")

# Plot trend
summary <- as.data.frame(as.matrix(out$summary))
results <- summary[which(rownames(summary) %in% c("Ntotal_clus[1]", "Ntotal_clus[2]", "Ntotal_clus[3]", "Ntotal_clus[4]", "Ntotal_clus[5]", "Ntotal_clus[6]", "Ntotal_clus[7]", "Ntotal_clus[8]", "Ntotal_clus[9]",
                                             "mu.lam", "sig.lam", "bzB.lam", "bYear.lam")), ]

par(mfrow = c(1,2))
# Plot the trend of the population
plot(-100,ylim = c(0,1100), xlim=c(0,9),
     pch = 21, ylab = "N", xlab = " ", axes = FALSE)
mtext("Population counts and abundance", side = 3, line = 1, cex = 1.2)
axis(1, at = c(1,2,3,4,5,6,7,8,9), labels = yrs)
axis(2)
points(results[1:9,1],pch = 19) # Plot results
points(count.year_clus,pch = 19, col = "red") # Plot counts
x <- seq_along(results[1:9,1])
low_CI <- as.numeric(results$`2.5%`[1:9])
up_CI <- as.numeric(results$`97.5%`[1:9])
arrows(x, low_CI,x, up_CI, code=3, angle=90, length=0.04)
legend("bottomright",fill=adjustcolor(c("black","red"),alpha.f = 0.8),
       border=c("black","red"),legend = c("N", "Counts"),
       box.lwd=0.1,
       bty = "n")

title("CALANDRA LARK HDS", line = -1, cex = 2, outer = TRUE)


# Plot YEAR effect

yrs2 <- c(0, 1, 2, 3, 4, 5, 6, 7, 8) # To make it as a continuous variable, otherwise it doesnt work

# PREDICTION FOR N FROM ITERATIONS
# out$samples contains all the iterations for each of the 3 chains ([1],[2],[3])

outall <- do.call(rbind,out$samples) # Here I put the three chains together


# 1. Calculate predictions for both zones

# ORIENTALES (ZONE)
pred <- list()

for(i in 1:dim(outall)[1]){ # 1:number of rows/iterations
  #plot(-15, xlim=c(min(area_SG_HA),max(area_SG_HA)), ylim=c(0,5)) # area_SG_HA unscaled variable
  #for(i in 1:500){ # To visually see, because there is a lot of iterations
  pred[[i]] <- exp(outall[i,"mu.lam"]+ # Add the intercept (random effect), also fixed to the mean of the random effect
                     outall[i,"bzB.lam"]*1 + # Prediction for fixed zone 1 (ORIENTAL)
                     outall[i,"bYear.lam"]*yrs2) 
  #points(pred[[i]]~area_SG_HA, type="l" ) # This is just to show visually all lines (prediction per iteration)
}

# Pred contains the list of the prediction of abundance N for each iteration 
#(one prediction line per iteration)

predall <- do.call(rbind,pred) # All predictions/iterations together in one data frame (where columns are the prediction per each predictor (area) values)
lci <- uci <- mean.pred <- 0 
for(i in 1:length(yrs2)){
  
  lci[i]  <- quantile(predall[,i],probs = 0.025) # For each value of area, tells the prediction that is in the position of the lower confidence interval
  # : For the area value 1, sets cutting value below which are the 2.5 % of the predicted abundances (LOWER CI)
  uci[i]  <- quantile(predall[,i],probs = 0.975)
  mean.pred[i]  <- mean(predall[,i])
}


#OCCIDENTALES (ZONE)
pred_oc <- list()
for(i in 1:dim(outall)[1]){
  
  pred_oc[[i]] <- exp(outall[i,"mu.lam"]+ 
                        outall[i,"bzB.lam"]*0 + 
                        outall[i,"bYear.lam"]*yrs2) 
}

predall_oc <- do.call(rbind,pred_oc) 
lci_oc <- uci_oc <- mean.pred_oc <- 0 

for(i in 1:length(yrs2)){
  
  lci_oc[i]  <- quantile(predall_oc[,i],probs = 0.025) 
  uci_oc[i]  <- quantile(predall_oc[,i],probs = 0.975)
  mean.pred_oc[i]  <- mean(predall_oc[,i])
}

# 2. Plot

#setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/Results/Plots/8TRIM")
setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/Results/Plots/8TRIM")
pdf("Mecal_Year.pdf")

plot(-15, xlim=c(0,8), ylim=c(0,4), main = " ", xlab = "Year", ylab = "Abundance", axes = FALSE)
mtext("Year effect", side = 3, line = 1, cex = 1.2)
axis(1, at = c(0,1,2,3,4,5,6,7,8), labels = yrs)
axis(2)
#points(uci~area_SG_HA, type="l" )
#points(lci~area_SG_HA, type="l" )
polygon( x = c(yrs2, rev(yrs2)),
         y = c(lci, rev(uci)), 
         col = adjustcolor(c("grey"),alpha.f = 0.6),
         border = NA)
points(mean.pred~yrs2, type="l")


#points(uci_oc ~ area_SG_HA, type="l", col = "red" )
#points(lci_oc ~ area_SG_HA, type="l", col = "red" )
polygon( x = c(yrs2, rev(yrs2)),
         y = c(lci_oc, rev(uci_oc)), 
         col = adjustcolor(c("blue"),alpha.f = 0.3),
         border = NA)
points(mean.pred_oc~yrs2, type="l", col = "blue")

legend("bottomright",fill=adjustcolor(c("blue","black"),alpha.f = 0.8),
       border=c("blue","black"),legend = c("Zone 1", "Zone 2"),
       box.lwd=0.1,
       bty = "n")

dev.off()