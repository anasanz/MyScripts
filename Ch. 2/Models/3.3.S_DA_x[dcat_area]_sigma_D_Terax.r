rm(list=ls())

library(jagsUI)

# ---- DATA: MECAL ----

setwd("C:/OneDrive/PhD/Second chapter/Data")
d <- read.csv("DataDS_ready.csv")
sp <- d[which(d$Species == "TERAX"), ]

# Study: Abundance of Calandra Lark in transects of 500 m long and 200 m strip width divided in unequal bins

strip.width <- 200
break.points <- c(0,25,50,100,200)
nbins <- length(unique(sp$Banda)) # Number of bins
midpt <- diff(break.points)/2+break.points[-5]
bin.width <- diff(break.points)

# ---- MODEL ----

# Data augmentation to provide NAs to the model to estimate
xbin <- sp$Banda
n <- length(xbin) # Number of observed
nau <- 3000 # Augment observed data with 300 observations more (we dont know if are detections or not)
y <- c(rep(1, n), rep(0, nau)) # Vector describing observed (1) and augmented (0) individuals
xbin1 <- c(sp$Banda, rep(NA, nau)) # Vector with distances observed (x, known) and augmented (NA)

# Data

win.data <- list (n=n, nau=nau, xbin=xbin1, y=y, strip.width=strip.width,
                  bin.width=bin.width, nbins=nbins, midpt=midpt)

# Model

setwd("C:/OneDrive/PhD/Second chapter/Data/Model")
cat("
    model{
    
    # Priors # DA and g(x,sig) Parameters 
    psi ~ dunif(0, 1)
    sigma ~ dunif(0, 5000)
    #sigma <- 2 # For example if it doesnt work, you fix a parameter and try to estimate it.
    
    # Likelihood
    
    # Construct conditional detection probability and Pr(x) for each bin 
    
    
    for(g in 1:nbins){ # midpt = mid point of each cell
    log(p[g]) <- -midpt[g] * midpt[g] / (2 * sigma * sigma) # p.detection at midbin distances
    pi[g] <- bin.width[g] / strip.width # Probability of occuring at each interval (related to area)
    } 
    
    for(i in 1:(n+nau)){
    
    z[i] ~ dbern(psi) # model for individual covariates (DA)
    
    xbin[i] ~ dcat(pi[]) # Distribution of distances (distance classes because its binned)
    
    y[i] ~ dbern(mu[i])
    mu[i] <- z[i] * p[xbin[i]] # the probability that an individual occurs and is detected in distance class h
    } 
    
    # Derived quantities: Population size and density
    N <- sum(z[]) # The sum of all estimated z is what you ultimately want
    D <- N / 0.1
    }",fill=TRUE, file = "model_different_binsize_D_Terax.txt")

# Inits
zst <- y
inits <- function(){ list (psi=runif(1), z=zst, sigma=runif(1,40,200)) } # One inits per parameter and z

# Params to save
params <- c("N", "sigma", "D")

# MCMC settings
nc <- 3 ; ni <- 10000 ; nb <- 2000 ; nt <- 2 

out <- jags(win.data, inits, params, "model_different_binsize_D_Terax.txt", n.chains = nc,
            n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
traceplot(out, param = c("N", "sigma", "D"))
print(out, 2)

setwd("C:/OneDrive/PhD/Second chapter/Data/Model/Plots")
pdf(file = "3.1S_x[dcat_area]_sigma_D_Terax.pdf")

par(mfrow = c(1,2))
plot(density(out$sims.list$N), xlab="Population size", ylab="Frequency", frame = F) 
abline(v = 6201.09, col = "blue", lwd = 3)

plot(density(out$sims.list$sigma), xlab="Sigma", ylab="Frequency", frame = F) 
abline(v = 111.17, col = "blue", lwd = 3) 

dev.off()
