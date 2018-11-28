
rm(list=ls())

library(rjags)
library(jagsUI)
library(rgdal)


# Run model 4.1 in MECAL dataset (1 year)
  # ---- I ignore counts in each observation (cluster size)

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data")

d <- read.csv("DataDS_ready.csv")
mec17 <- d[which(d$Species == "MECAL" & d$Year == 2017), ] # Select species MECAL and year 2017
m <- mec17[ ,which(colnames(mec17) %in% c("Banda", "transectID"))]

# ---- Specify data for the model ----

# Number of sites obtained from the layer (so that sampled sites with 0 abundance also appear)
tr <- readOGR("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS", "Transects_2010_2017_EPSG23031") # Contains transects sampled each year (1/0)
tr2017 <- as.character(tr@data[which(tr@data$FETS2017 == 1), which(colnames(tr@data) %in% "Codi")])
nSites <- length(tr2017) 

# Bin information
strip.width <- 200 				
dist.breaks <- c(0,25,50,100,200)
int.w <- diff(dist.breaks) # width of distance categories (v)
midpt <- diff(dist.breaks)/2+dist.breaks[-5]
nG <- length(dist.breaks)-1	

# Distance observations

y <- as.data.frame(array(0, c(nSites, length(dist.breaks)-1)))
rownames(y) <- tr2017

ID <- unique(m$transectID) # order transects = dclass
bin.breaks <- c(0,1,2,3,4)

for (i in 1:length(ID)){
  transect <- m[which(m$transectID == ID[i]), ] # Select all observations from one transect
  counts <- table(cut(transect$Banda, bin.breaks, include.lowest = TRUE)) # Classify them in bins
  y[which(rownames(y) == ID[i]), ] <- counts # Place them where the transectID is
  }
  
y.sum <- apply(y, 1, sum) # Total count per site
nind <- sum(y)
#dclass <- m$Banda
#length(dclass) #394 and it should be 353. There are sampled transects not included in the layer? (41obs)
                # PROBLEMS: SI02 (it is transect SI102, the SI02 doesnt exist and wasnt sampled) --> Error mio (4obs)
                #           AF27 (doesnt appear en layer, like if it wasnt sampled but I have data) (8obs)
                #           BE29 " (8obs)
                #           AF02 " (11obs)
                #           BE46 " (6obs)
                #           SI98 " (1obs)
y[which(rownames(y) == "SI98"),]
tr@data[tr@data$Codi == "AF27", ]
which(m$transectID == "BE29")
length(which(m$transectID == "SI02"))

# Provisionaly: Keep only the ones present in the layer
m2 <- m[which(m$transectID %in% tr2017), ]
dclass <- m2$Banda


# ---- Compile data for JAGS model ----

data1 <- list(nsites=nSites, nG=nG, int.w=int.w, strip.width = strip.width, 
              y = y.sum, nind=nind, dclass=dclass, midpt = midpt)

# ---- JAGS model ----

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/Model")
cat("model{
    # Priors
    mu.lam ~ dunif(-10, 10) # I allow it to have negative values because the log of lambda can have
    sig.lam ~ dunif(0, 10)
    sigma ~ dunif(0, 1000)
    tau.lam <- 1/(sig.lam*sig.lam)
    
    for(i in 1:nind){
    dclass[i] ~ dcat(fc[]) # Part 1 of HM
    }
    
    # Construct cell probabilities for nG multinomial cells (distance categories)
    for(k in 1:nG){ 
    log(p[k]) <- -midpt[k] * midpt[k] / (2*sigma*sigma)
    pi[k] <- int.w[k] / strip.width # Probability per interval
    f[k] <- p[k] * pi[k] # f = p*pi;  pi is the probability of occurring in the interval (pi = delta/B)
    # p  is the integral under the detection function over the bin h 
    fc[k] <- f[k] / pcap # Prob of detection in that cell relative to the total pdetection in the site (sum of p of all bins)
    }
    pcap <- sum(f[]) # Pr(capture): sum of rectangular areas
    
    for(j in 1:nsites){
    y[j] ~ dbin(pcap, N[j]) # Part 2 of HM
    N[j] ~ dpois(lambda[j]) # Part 3 of HM
    lambda[j] <- exp(log.lambda[j])
    log.lambda[j] ~ dnorm(mu.lam, tau.lam)
    }
    # Derived parameters
    Ntotal <- sum(N[])
    area <- nsites*1*2*strip.width # Unit length == 1, half-width = B
    D <- Ntotal/area
    }",fill=TRUE, file = "D_MECAL_sigma_lambda_norm_j.txt")

# Inits
Nst <- y.sum + 1
inits <- function(){list(mu.lam = runif(1), sig.lam = 0.2, sigma = runif(1, 20, 100), N=Nst)}

# Params
params <- c("Ntotal", "N", "D", "sigma", "lambda", "mu.lam", "sig.lam")

# MCMC settings
nc <- 3 ; ni <- 100000 ; nb <- 2000 ; nt <- 2

# With jagsUI 
out <- jags(data1, inits, params, "D_MECAL_sigma_lambda_norm_j.txt", n.chain = nc,
            n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
print(out)
traceplot(out)
