


set.seed(2013)

# ---- Data simulation ----
#### Simulate abundance for one species:
#8 years (unbalanced number of transects per year); lambda site specific(Zone variable and 2 areas variables)

g <- function(x, sig) exp(-x^2/(2*sig^2))

# Number of transects per year (unbalanced)
nSites <- seq(50,106, by = 8)				# number of line transect surveys (DIFFERENT BY YEAR)
max.sites <- max(nSites)            # Maximun number of sites is the last year

strip.width <- 200 				# strip half-width, w (in this example only one side of the line transect is surveyed)
dist.breaks <- c(0,25,50,100,200)
int.w <- diff(dist.breaks) # width of distance categories (v)
midpt <- diff(dist.breaks)/2+dist.breaks[-5]
nG <- length(dist.breaks)-1	

# Year effect 
yrs <- 1:8 # eight years
nyrs <- length(yrs)


# ----  Abundance component: random effect accross sites, zone covariate and 2 area covariates


# RANDOM EFFECT IN SITE (INDEPENDENT OF THE YEAR)
# Mean abundance and sd across sites
mu.lam.alpha.site <- log(1.5)				
sig.lam.alpha.site <- 0				
##Site effect in lambda
lam.alpha.site <- rnorm(max.sites, mu.lam.alpha.site, sig.lam.alpha.site) 


#ZONE COVARIATE (SITE)
#b.zoneA <- rnorm(1,0,0.05) #I wont include it (its in the intercept)
b.lam.zoneB <- 4
# Site specific binary co-variate
z <- data.frame(var = sample(c("A", "B"), max.sites, replace = TRUE))
z$var <- as.factor(z$var)
zone <- model.matrix(~ var-1, z)


#AREA COVARIATE (SITE AND YEAR)
#Coefficients
b.a1 <- 2
b.a2 <- 2
#Covariates
a1 <- abs(rnorm(max.sites*nyrs, 10, 5)) # Although it makes sense to make them positive, it wouldnt matter (you put them on the exp)
a2 <- abs(rnorm(max.sites*nyrs, 5, 2.5))

#SCALED
area1_mean <- mean(a1)
area1_sd <- sd(a1)
area1_sc <- (a1 - area1_mean) / area1_sd

area2_mean <- mean(a2)
area2_sd <- sd(a2)
area2_sc <- (a2 - area2_mean) / area2_sd


lam <- exp(matrix(lam.alpha.site, nrow = max.sites, ncol = nyrs)  
             + matrix(b.lam.zoneB*zone[,2], nrow = max.sites, ncol = nyrs, byrow=F)  
             + matrix(b.a1*area1_sc, nrow = max.sites, ncol = nyrs, byrow=F)
             +matrix(b.a2*area2_sc, nrow = max.sites, ncol = nyrs, byrow=F)
           )


############################################################################
# PLOT
plot(lam,a1)
plot(lam,area2_sc)

### CYRIL 
# I DONT SEE ANY PROBLEMS HERE
# SO HERE ARE YOUR DIFFERENT PARAMETERS
# THIS IS YOUR AVERAGE INTERCEPT 
mu.lam.alpha.site <- log(1.5)				
# THIS IS THE VARIATION AROUND THE INTERCEPT 
# IF IT IS EQUAL TO 0, IT MEANS THAAT THERE IS NO VARIATION IN THE INTERCEPT DUE TO THE SITE. 
# IF YOU SET THIS VALUE TO HIGH, IT MEANS THAT THE INTERCEPT CHANGES A LOT WHIT DIFFERENT SITES. THIS ADDS SOME NOISE IN THE SIGMA. 
sig.lam.alpha.site <- 0				
lam.alpha.site <- rnorm(max.sites, mu.lam.alpha.site, sig.lam.alpha.site) 

## THIS ARE YOUR BETA COEFFICIENTS, THE SLOPE, 
# IF YOU HAVE A LOT OF NOISE (sig.lam.alpha.site = HIGH VALUE), IT MIGHT BE HARD TO SEE THE RELATIONSHIP VISUALLY WHEN YOU PLOT IT.
# I SET THE BETAS HIGH TO SEE THE RELATIONSHIO CLEARLY.
b.lam.zoneB <- 4
b.a1 <- -2
b.a2 <- 2

#==== I. ====
# WE START WITH AN INTERCEPT THAT DOESNT CHANGE WITH THE SITE (sig.lam.alpha.site=0) AND ONLY ONE COVARIATE (area2_sc). 
lam <- exp(matrix(lam.alpha.site, nrow = max.sites, ncol = nyrs)  
           # + matrix(b.lam.zoneB*zone[,2], nrow = max.sites, ncol = nyrs, byrow=F)  
           # + matrix(b.a1*area1_sc, nrow = max.sites, ncol = nyrs, byrow=F)
           +matrix(b.a2*area2_sc, nrow = max.sites, ncol = nyrs, byrow=F)
)

# THE RELATIONSHIP IS CLEAR, POSTIVE EFFECT OF AREA2_SC
plot(as.vector(lam)~ a2)

#==== II. ====
## LET'S TRY WITH AN INTERCEPT THAT DOESNT CHANGE WITH THE SITE (sig.lam.alpha.site=0) AND ONLY ONE COVARIATE (area1_sc)
lam <- exp(matrix(lam.alpha.site, nrow = max.sites, ncol = nyrs)  
           # + matrix(b.lam.zoneB*zone[,2], nrow = max.sites, ncol = nyrs, byrow=F)  
            + matrix(b.a1*area1_sc, nrow = max.sites, ncol = nyrs, byrow=F)
           # +matrix(b.a2*area2_sc, nrow = max.sites, ncol = nyrs, byrow=F)
)
# THE RELATIONSHIP IS CLEAR, NEGATIVE EFFECT OF AREA1_SC
plot(as.vector(lam)~ a1)


#==== III. ====
## LET'S TRY WITH AN INTERCEPT THAT  CHANGEs WITH THE SITE
## WE SET A SD : sig.lam.alpha.site
sig.lam.alpha.site <- 0.5				
lam.alpha.site <- rnorm(max.sites, mu.lam.alpha.site, sig.lam.alpha.site) 
#AND ONLY ONE COVARIATE (area1_sc)
lam <- exp(matrix(lam.alpha.site, nrow = max.sites, ncol = nyrs)  
           # + matrix(b.lam.zoneB*zone[,2], nrow = max.sites, ncol = nyrs, byrow=F)  
           + matrix(b.a1*area1_sc, nrow = max.sites, ncol = nyrs, byrow=F)
           # +matrix(b.a2*area2_sc, nrow = max.sites, ncol = nyrs, byrow=F)
)
# THE RELATIONSHIP IS NOT AS CLEAR, NEGATIVE EFFECT OF AREA1_SC BUT THERE IS SOME NOISE
plot(as.vector(lam)~ a1)

#==== IV. ====
## LET'S TRY WITH AN INTERCEPT THAT  CHANGEs WITH THE SITE
## WE SET A SD : sig.lam.alpha.site
sig.lam.alpha.site <- 0.5				
lam.alpha.site <- rnorm(max.sites, mu.lam.alpha.site, sig.lam.alpha.site) 
#AND TWO COVARIATES (area1_sc)
lam <- exp(matrix(lam.alpha.site, nrow = max.sites, ncol = nyrs)  
           # + matrix(b.lam.zoneB*zone[,2], nrow = max.sites, ncol = nyrs, byrow=F)  
           +matrix(b.a1*area1_sc, nrow = max.sites, ncol = nyrs, byrow=F)
            +matrix(b.a2*area2_sc, nrow = max.sites, ncol = nyrs, byrow=F)
)
# THE RELATIONSHIP IS NOT CLEAR.
# THE REASON IS THAT YOU HAVE SOME NOISE IN THE INTERCEPT AND 2 COVARIATES AFFECT lam. SO IT IS HARD TO SEE VISUALLY...
plot(as.vector(lam)~ a2)
plot(as.vector(lam)~ a1)

# MAYBE IF WE TRY A 3D PLOT
library("scatterplot3d") # load
scatterplot3d(as.vector(lam), a2, a1, angle=40 ) # no we dont see. 



#==== V. ====
## LET'S TRY WITH AN INTERCEPT THAT  DOESNT CHANGE WITH THE SITE
## WE SET A SD : sig.lam.alpha.site
sig.lam.alpha.site <- 0			
lam.alpha.site <- rnorm(max.sites, mu.lam.alpha.site, sig.lam.alpha.site) 
#AND TWO COVARIATES (area1_sc)
## TRY OTHER BETA VALUES
b.a1 <- 1
b.a2 <- 1

lam <- exp(matrix(lam.alpha.site, nrow = max.sites, ncol = nyrs)  
           # + matrix(b.lam.zoneB*zone[,2], nrow = max.sites, ncol = nyrs, byrow=F)  
           +matrix(b.a1*area1_sc, nrow = max.sites, ncol = nyrs, byrow=F)
           +matrix(b.a2*area2_sc, nrow = max.sites, ncol = nyrs, byrow=F)
)
# THE RELATIONSHIP IS NOT CLEAR.
# THE REASON IS THAT YOU HAVE SOME NOISE IN THE INTERCEPT AND 2 COVARIATES AFFECT lam. SO IT IS HARD TO SEE VISUALLY...
plot(as.vector(lam)~ a2)
plot(as.vector(lam)~ a1)

# MAYBE IF WE TRY A 3D PLOT
library("scatterplot3d") # 
scatterplot3d(as.vector(lam), a2, a1, angle=60 ) ### YES!!!!!! 


