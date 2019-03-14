
rm(list=ls())

library(rjags)
library(jagsUI)
library(dplyr)

# Run model 7.1 in MECAL dataset 
# ---- I ignore counts in each observation (cluster size)

# ---- Data ----
setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data")
d <- read.csv("DataDS_ready.csv")

# Information: bins, years, sites

strip.width <- 200 				
dist.breaks <- c(0,25,50,100,200)
int.w <- diff(dist.breaks) # width of distance categories (v)
midpt <- diff(dist.breaks)/2+dist.breaks[-5]
nG <- length(dist.breaks)-1

yrs <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
nyrs <- length(yrs)

# To take into account transects with abundance 0
# 1. Select all transects IDs from all species observations
# 2. Join the observations of MECAL (for example) with all transects so that they remain with NA if the
# species was there but it wasnt sampled

d_tr <- d[ ,which(colnames(d) %in% c("Species",  "T_Y"))]
d_tr_all <- data.frame(T_Y = unique(d_tr$T_Y), id = NA)


mec <- d[which(d$Species == "MECAL"), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species"))] # Select species MECAL and all years
mec <- arrange(mec, Year, transectID) #Ordered
mec_detec_transectID <- unique(mec$transectID)


absent <- anti_join(d_tr_all,mec) # Transects with 0 abundance, add to mec.
colnames(absent)[2] <- "Banda" # Format it to add the rows to mec
absent$T_Y <- as.character(absent$T_Y)
absent$Species <- "MECAL"


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


# ---- Co-variates ----

setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data")
manag <- read.csv("management_area.csv")

manag <- manag[which(manag$Codi %in% all.sites), ] # Select transects with census

# Be sure the fields are in the same order
order <- as.data.frame(m)
order_codi <- as.vector(rownames(order))
order$Codi <- order_codi
manag <- left_join(order,manag)

# Area AES
area_aes <- as.matrix(manag[ ,c(11:18)])

aes_mean <- mean(area_aes)
aes_sd <- sd(area_aes)
aes_sc <- (area_aes - aes_mean) / aes_sd

# Area SG
area_sg <- manag[ ,c(19:22)]
area_sg$area_sg10 <- 0
area_sg$area_sg11 <- 0
area_sg$area_sg12 <- 0
area_sg$area_sg13 <- 0
area_sg <- as.matrix(area_sg[ ,c(5:8,1:4)])

sg_mean <- mean(area_sg)
sg_sd <- sd(area_sg)
sg_sc <- (area_sg - sg_mean) / sg_sd

# Zone (Occidental = 0; Oriental = 1)
zone <- order
for (i in 1:nrow(zone)){
if(substr(zone$Codi[i], 1,2) == "BA"){zone[i,1:8] <- 0}
  if(substr(zone$Codi[i], 1,2) == "BM"){zone[i,1:8] <- 1}
  if(substr(zone$Codi[i], 1,2) == "SI"){zone[i,1:8] <- 1}
  if(substr(zone$Codi[i], 1,2) == "AF"){zone[i,1:8] <- 0}
  if(substr(zone$Codi[i], 1,2) == "BE"){zone[i,1:8] <- 1}
  if(substr(zone$Codi[i], 1,2) == "GR"){zone[i,1:8] <- 0}
}
zone <- zone[,-9]


# ---- Specify data in JAGS format ----

# Distance class and ind
nind <- nrow(mec)
dclass <- mec$Banda


# Get one long vector with counts per year and site
yLong <- unlist(as.data.frame(m), use.names = F)

sitesYears <- NULL
for (i in 1:nyrs){
  sitesYears <- c(sitesYears,c(1:length(all.sites)))}


# Get one vector per co-variate

area_AES <- NULL
for (i in 1:nyrs){
  area_AES <- c(area_AES,aes_sc[1:length(all.sites),i])}

area_SG <- NULL
for (i in 1:nyrs){
  area_SG <- c(area_SG,sg_sc[1:length(all.sites),i])}

zon <- NULL
for (i in 1:nyrs){
  zon <- c(zon,zone[1:length(all.sites),i])}

# Create one matrix for indexing year when calculating abundance per year in JAGS
allyears <- NULL 
for (i in 1:nyrs){
  allyears <- c(allyears,rep(yrs[i],length(all.sites)))
}
m <- data.frame(allyears = allyears)
m$allyears <- as.factor(m$allyears)
indexYears <- model.matrix(~ allyears-1, data = m)

# ---- Compile data for JAGS model ----

data1 <- list(nyears = nyrs, max.sites = max.sites, nG=nG, int.w=int.w, strip.width = strip.width, 
              y = yLong, nind=nind, dclass=dclass, midpt = midpt, sitesYears = sitesYears, indexYears = indexYears,
              area1 = area_AES, area2 = area_SG, zoneB = zon)

# ---- JAGS model ----

setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/Model")
cat("model{
    # Priors
    
    # Coefficients for lambda
    bzB.lam ~ dnorm(0, 0.001)
    ba1.lam ~ dnorm(0, 0.001)
    ba2.lam ~  dnorm(0, 0.001)
    
    
    mu.lam ~ dunif(-10, 10) # Random effects for lambda per site
    # I allow it to have negative values because the log of lambda can have
    sig.lam ~ dunif(0, 10)
    tau.lam <- 1/(sig.lam*sig.lam)
    sigma ~ dunif(0, 1000)
    
    #RANDOM TRANSECT LEVEL EFFECT FOR LAMBDA (doesn't change over time) # takes care of the dependence in data when you repeatedly visit the same transect
    for (s in 1:max.sites){
    log.lambda[s] ~ dnorm(mu.lam, tau.lam)
    }
    
    for(i in 1:nind){
    dclass[i] ~ dcat(fc[]) 
    }
    
    # Construct cell probabilities for nG multinomial cells (distance categories)
    for(k in 1:nG){ 
    log(p[k]) <- -midpt[k] * midpt[k] / (2*sigma*sigma)
    pi[k] <- int.w[k] / strip.width 
    f[k] <- p[k] * pi[k] 
    fc[k] <- f[k] / pcap 
    }
    pcap <- sum(f[]) # Same for all sites all years
    
    for(j in 1:length(y)){ # sites*years. Because in my data there is different number of sites per year
    y[j] ~ dbin(pcap, N[j]) 
    N[j] ~ dpois(lambda[j]) 
    lambda[j] <- exp(log.lambda[sitesYears[j]]  
    + bzB.lam*zoneB[j]
    + ba1.lam*area1[j] + ba2.lam*area2[j]) 
    }
    
    # Derived parameters
    for (i in 1:nyears){
    Ntotal[i] <- sum(N*indexYears[,i]) 
    }
    }",fill=TRUE, file = "s_sigma_lambda[alpha(j)_covZones(j)_covAreas(jt)].txt")

# Inits
Nst <- yLong + 1
inits <- function(){list(mu.lam = runif(1), sig.lam = 0.2, sigma = runif(1, 20, 100), N=Nst,
                         bzB.lam = runif(1), ba1.lam = runif(1), ba2.lam = runif(1) )}

# Params
params <- c("Ntotal", "N", "sigma", "lambda", "mu.lam", "sig.lam", 
            "bzB.lam", "ba1.lam", "ba2.lam")

# MCMC settings
nc <- 3 ; ni <- 10000 ; nb <- 2000 ; nt <- 2

# With jagsUI 
out <- jags(data1, inits, params, "s_sigma_lambda[alpha(j)_covZones(j)_covAreas(jt)].txt", n.chain = nc,
            n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
print(out)

for (i in 1:nyrs){
  plot(density(out$sims.list$Ntotal[,i]), xlab="Population size", ylab="Frequency", 
       frame = F, main = paste("year",i)) 
  abline(v = mean(out$sims.list$Ntotal[,i]), col = "red", lwd = 3)
}

summary <- as.data.frame(as.matrix(out$summary))

setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/Results")
write.csv(summary, "7.1.Mecal.csv")

##############################################################################
summary <- read.csv("7.1.Mecal.csv")




results <- summary[which(summary$X %in% c("Ntotal[1]", "Ntotal[2]", "Ntotal[3]", "Ntotal[4]", "Ntotal[5]", "Ntotal[6]",
                                                   "Ntotal[7]", "Ntotal[8]", "mu.lam", "sig.lam", "bzB.lam", "ba1.lam", "ba2.lam")), ]


plot(-100,ylim = c(0,1000), xlim=c(0,8),
     pch = 21, ylab = "N", xlab = " ", axes = FALSE, main = "MECAL")
axis(1, at = c(1,2,3,4,5,6,7,8), labels = yrs)
axis(2)
points(results[1:8,2],pch = 19)
x <- seq_along(results[1:8,2])
low_CI <- as.numeric(results$X2.5.[1:8])
up_CI <- as.numeric(results$X97.5.[1:8])
arrows(x, low_CI,x, up_CI, code=3, angle=90, length=0.04)

results2 <- summary[9:1336, ]
plot(results2$mean~area_SG, pch = 16, ylab = "Abundance")

