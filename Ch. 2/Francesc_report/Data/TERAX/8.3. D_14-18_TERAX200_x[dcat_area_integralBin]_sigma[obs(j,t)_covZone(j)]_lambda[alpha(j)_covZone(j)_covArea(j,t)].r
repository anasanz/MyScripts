
rm(list=ls())

library(rjags)
library(jagsUI)
library(dplyr)

# Run model 8.2 in TERAX dataset 

# 2014 - 2018

# ---- I ignore counts in each observation (cluster size)

# ---- Data ----
setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data")
d <- read.csv("DataDS_ready_ALL.csv")

d <- d[which(d$Year %in% c(2014, 2015, 2016, 2017, 2018)), ]

# Information: bins, years, sites

strip.width <- 200 				
dist.breaks <- c(0,25,50,100,200)
int.w <- diff(dist.breaks) # width of distance categories (v)
midpt <- diff(dist.breaks)/2+dist.breaks[-5]
nG <- length(dist.breaks)-1

yrs <- c(2014, 2015, 2016, 2017, 2018)
nyrs <- length(yrs)

# To take into account transects with abundance 0
# 1. Select all transects IDs from all species observations
# 2. Join the observations of TERAX (for example) with all transects so that they remain with NA if the
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


mec <- d[which(d$Species == "TERAX"), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "Observer"))] # Select species MECAL and all years
mec <- arrange(mec, Year, transectID) #Ordered
mec_detec_transectID <- unique(mec$transectID)
mec$Observer <- as.character(mec$Observer) 


absent <- anti_join(d_tr_all,mec) # Transects with 0 abundance, add to mec.
colnames(absent)[2] <- "Banda" # Format it to add the rows to mec
absent$T_Y <- as.character(absent$T_Y)
absent$Species <- "TERAX"
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
# ---- Co-variates ----

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data")
#setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data")
manag <- read.csv("management_area_200.csv")

manag <- manag[ , c(1,2,11:15)] # Select years 2014 - 2018
#manag$area_sg13 <- 0 # Add 13 as a 0
#manag$area_sg13 <- as.numeric(manag$area_sg13)

#manag <- manag[ ,c(1,2,8,3:7)] # Order years

manag <- manag[which(manag$Codi %in% all.sites), ] # Select transects with census

# Be sure the fields are in the same order
order <- as.data.frame(m)
order_codi <- as.vector(rownames(order))
order$Codi <- order_codi
manag <- left_join(order,manag)


# Area SG
area_sg <- as.matrix(manag[ ,c(8:12)])

sg_mean <- mean(area_sg)
sg_sd <- sd(area_sg)
sg_sc <- (area_sg - sg_mean) / sg_sd

# Zone (Occidental = 0; Oriental = 1)
zone <- order
for (i in 1:nrow(zone)){
  if(substr(zone$Codi[i], 1,2) == "BA"){zone[i,1:5] <- 0}
  if(substr(zone$Codi[i], 1,2) == "BM"){zone[i,1:5] <- 1}
  if(substr(zone$Codi[i], 1,2) == "SI"){zone[i,1:5] <- 1}
  if(substr(zone$Codi[i], 1,2) == "AF"){zone[i,1:5] <- 0}
  if(substr(zone$Codi[i], 1,2) == "BE"){zone[i,1:5] <- 1}
  if(substr(zone$Codi[i], 1,2) == "GR"){zone[i,1:5] <- 0}
}
zone <- zone[,-6]

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

area_SG <- NULL
for (i in 1:nyrs){
  area_SG <- c(area_SG,sg_sc[1:length(all.sites),i])}

zon <- NULL
for (i in 1:nyrs){
  zon <- c(zon,zone[1:length(all.sites),i])}

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

obs_id <- unique(ob)[-4]
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
              area2 = area_SG, zoneB = zon, ob = ob, nobs = nobs, db = dist.breaks)

# ---- JAGS model ----

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/Model")
#setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/Model")
cat("model{
    
    # PRIORS
    
    # Priors for lambda
    bzB.lam ~ dnorm(0, 0.001)
    ba2.lam ~  dnorm(0, 0.001)
    
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
    lambda[j] <- exp(log.lambda[sitesYears[j]] + bzB.lam*zoneB[j]
    + ba2.lam*area2[j]) 
    }
    
    # Derived parameters
    for (i in 1:nyears){
    Ntotal[i] <- sum(N*indexYears[,i]) 
    }
    }",fill=TRUE, file = "s_sigma(integral)[obs(o,j,t)_covZone(j)]_lambda[alpha(j)_covZone(j)_covArea2(j,t)].txt")

# Inits
Nst <- yLong + 1
inits <- function(){list(mu.lam = runif(1), sig.lam = 0.2, #sigma = runif(624, 0, 50), I dont need sigma because I have already priors for his hyperparameters!!!!!
                         N=Nst,
                         bzB.lam = runif(1), ba2.lam = runif(1),
                         mu.sig = runif(1, log(30), log(50)), sig.sig = runif(1), bzB.sig = runif(1)
                         ###changed inits for mu.sig - don't start too small, better start too large
)}

# Params
params <- c("Ntotal", "N",# "sigma", "lambda", I remove it so that it doesnt save the lambdas and takes shorter. It still calculates them
            "mu.lam", "sig.lam", 
            "bzB.lam", "ba2.lam",
            "mu.sig", "sig.sig", "bzB.sig"
)

# MCMC settings
nc <- 3 ; ni <- 65000 ; nb <- 2000 ; nt <- 2

# With jagsUI 
out <- jags(data1, inits, params, "s_sigma(integral)[obs(o,j,t)_covZone(j)]_lambda[alpha(j)_covZone(j)_covArea2(j,t)].txt", n.chain = nc,
            n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)

print(out)
traceplot(out)
plot(out)


summary <- as.data.frame(as.matrix(out$summary))

#setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/Results/8.2.Francesc13-18")
setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/Results/8.2.Francesc13-18")
write.csv(summary, "8.3.Terax200_14-18.csv")
save(out, file = "8.3.Terax200_14-18.RData") # Save as RData to be able to access everything

###################################################################

#                             Plot results                             

#####    Method 1(WRONG?) 

#setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/Results/8.2.Francesc13-18")
setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/Results/8.2.Francesc13-18")

summary <- read.csv("8.3.Terax200_14-18.csv")

results200 <- summary[which(summary$X %in% c("Ntotal[1]", "Ntotal[2]", "Ntotal[3]", "Ntotal[4]", "Ntotal[5]", "mu.lam", "sig.lam", "bzB.lam", "ba1.lam", "ba2.lam")), ]

# SG POSITIVE EFFECT

# To plot the relation with the co-variates 
results200_2 <- summary[6:735, ]
plot(results200_2$mean ~ area_AES, ylab = "Abundance") 

# Plot using the un-scaled SG
# The estimate is in the scale of the variable. So you have to create the predicted abundance with the scale of the variable (area_SGpred)
# If you want to plot it against the unscaled variable, you need to plot the prediction(obtained with the scaled v.) against the un-scaled variable
# Both vector should have the same length

area_SG_HA <- NULL
for (i in 1:nyrs){
  area_SG_HA <- c(area_SG_HA,area_sg[1:length(all.sites),i])} # Create vector

#setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/Results/Plots/8.2/Report")
setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/Results/Plots/8.2/Report")

pdf("Terax_200_HA_SG_1418_Method1.pdf")

area_SG_HA <- seq(min(area_SG_HA), max(area_SG_HA),length.out = 500) # Create a sequence of values, from minimum to maximun of the covariate to plot the prediction
area_SGpred <- seq(min(area_SG), max(area_SG),length.out = 500) # Create a sequence of values, from minimum to maximun of the covariate to plot the prediction

pred <- exp(results200[which(results200$X == "mu.lam"),2]+ # Add the intercept (random effect), also fixed to the mean of the random effect
                   results200[which(results200$X == "bzB.lam"),2]*1 + # Prediction for fixed zone 1 (ORIENTAL)
                   results200[which(results200$X == "ba2.lam"),2]*area_SGpred) 


predlci <- exp(results200[which(results200$X == "mu.lam"),4]+ # Add the intercept (random effect), also fixed to the mean of the random effect
                 results200[which(results200$X == "bzB.lam"),4]*1 + # Prediction for fixed zone 1 (ORIENTAL)
                 results200[which(results200$X == "ba2.lam"),4]*area_SGpred) 

preduci <- exp(results200[which(results200$X == "mu.lam"),8]+ # Add the intercept (random effect), also fixed to the mean of the random effect
                 results200[which(results200$X == "bzB.lam"),8]*1 + # Prediction for fixed zone 1 (ORIENTAL)
                 results200[which(results200$X == "ba2.lam"),8]*area_SGpred) 

plot(pred ~ area_SG_HA, ylim=c(0,5),xlim = c(0,20), type="l", main = "Terax_m1", xlab = "Area_SG", ylab = "Abundance")
#points(predlci ~ area_SGpred, pch=16, type="l",lty=2)
#points(preduci ~ area_SGpred, pch=16,type="l",lty=2)
polygon( x = c(area_SG_HA, rev(area_SG_HA)),
         y = c(predlci, rev(preduci)), 
         col = adjustcolor(c("grey"),alpha.f = 0.6),
         border = NA)


pred0 <- exp(results200[which(results200$X == "mu.lam"),2]+
               results200[which(results200$X == "bzB.lam"),2]*0 + # Prediction fixed for zone 0 (occidental)
               results200[which(results200$X == "ba2.lam"),2]*area_SGpred) 

pred0lci <- exp(results200[which(results200$X == "mu.lam"),4]+ # PREDICTION LOW CI FOR OCCIDENTAL
                  results200[which(results200$X == "bzB.lam"),4]*0 + 
                  results200[which(results200$X == "ba2.lam"),4]*area_SGpred) 

pred0uci <- exp(results200[which(results200$X == "mu.lam"),8]+ # PREDICTION UP CI FOR OCCIDENTAL
                  results200[which(results200$X == "bzB.lam"),8]*0 + 
                  results200[which(results200$X == "ba2.lam"),8]*area_SGpred) 

points(pred0 ~ area_SG_HA, pch=16, type="l", col="red")
#points(pred0lci ~ area_SGpred, pch=16, type="l",lty=2, col="red")
#points(pred0uci ~ area_SGpred, pch=16,type="l",lty=2, col="red")
polygon( x = c(area_SG_HA, rev(area_SG_HA)),
         y = c(pred0lci, rev(pred0uci)), 
         col = adjustcolor(c("red"),alpha.f = 0.2),
         border = NA)
legend("topleft",fill=adjustcolor(c("red","black"),alpha.f = 0.8),
       border=c("red","black"),legend = c("Occidentals", "Orientals"),
       box.lwd=0.1,
       bty = "n")

points(pred ~ area_SG_HA, pch=16, type="l")

dev.off()


####       Method 2 

# RAHEL: PREDICTION FOR N FROM ITERATIONS
# out$samples contains all the iterations for each of the 3 chains ([1],[2],[3])

outall <- do.call(rbind,out$samples) # Here I put the three chains together

area_SGpred <- seq(min(area_SG), max(area_SG),length.out = 100) 
area_SG_HA <- seq(min(area_SG_HA), max(area_SG_HA),length.out = 100) # To plot with unscaled values

# 1. Calculate predictions for both zones

# ORIENTALES (ZONE)
pred <- list()
for(i in 1:dim(outall)[1]){ # 1:number of rows/iterations
#plot(-15, xlim=c(min(area_SG_HA),max(area_SG_HA)), ylim=c(0,5)) # area_SG_HA unscaled variable
#for(i in 1:500){ # To visually see, because there is a lot of iterations
  pred[[i]] <- exp(outall[i,"mu.lam"]+ # Add the intercept (random effect), also fixed to the mean of the random effect
                     outall[i,"bzB.lam"]*1 + # Prediction for fixed zone 1 (ORIENTAL)
                     outall[i,"ba2.lam"]*area_SGpred) 
  #points(pred[[i]]~area_SG_HA, type="l" ) # This is just to show visually all lines (prediction per iteration)
}
# Pred contains the list of the prediction of abundance N for each iteration 
#(one prediction line per iteration)

predall <- do.call(rbind,pred) # All predictions/iterations together in one data frame (where columns are the prediction per each predictor (area) values)
lci <- uci <- mean.pred <- 0 
for(i in 1:length(area_SGpred)){
  
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
                     outall[i,"ba2.lam"]*area_SGpred) 
}

predall_oc <- do.call(rbind,pred_oc) 
for(i in 1:length(area_SGpred)){
  
  lci_oc[i]  <- quantile(predall_oc[,i],probs = 0.025) 
  uci_oc[i]  <- quantile(predall_oc[,i],probs = 0.975)
  mean.pred_oc[i]  <- mean(predall_oc[,i])
}

# 2. Plot

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/Results/Plots/8.2/Report")
pdf("Terax_200_HA_SG_1418_Method2.pdf")

plot(-15, xlim=c(min(area_SG_HA),20), ylim=c(0,5), main = "Terax_m2", xlab = "Area_SG", ylab = "Abundance") # area_SG_HA unscaled variable

#points(uci~area_SG_HA, type="l" )
#points(lci~area_SG_HA, type="l" )
polygon( x = c(area_SG_HA, rev(area_SG_HA)),
         y = c(lci, rev(uci)), 
         col = adjustcolor(c("grey"),alpha.f = 0.6),
         border = NA)
points(mean.pred~area_SG_HA, type="l")


#points(uci_oc ~ area_SG_HA, type="l", col = "red" )
#points(lci_oc ~ area_SG_HA, type="l", col = "red" )
polygon( x = c(area_SG_HA, rev(area_SG_HA)),
         y = c(lci_oc, rev(uci_oc)), 
         col = adjustcolor(c("red"),alpha.f = 0.6),
         border = NA)
points(mean.pred_oc~area_SG_HA, type="l", col = "red")

legend("topleft",fill=adjustcolor(c("red","black"),alpha.f = 0.8),
       border=c("red","black"),legend = c("Oc", "Or"),
       box.lwd=0.1,
       bty = "n")

dev.off()
