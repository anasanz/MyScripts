


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
sig.lam.alpha.site <- 1				
##Site effect in lambda
lam.alpha.site <- rnorm(max.sites, mu.lam.alpha.site, sig.lam.alpha.site) 


#ZONE COVARIATE (SITE)
#b.zoneA <- rnorm(1,0,0.05) #I wont include it (its in the intercept)
b.lam.zoneB <- 0.5
# Site specific binary co-variate
z <- data.frame(var = sample(c("A", "B"), max.sites, replace = TRUE))
z$var <- as.factor(z$var)
zone <- model.matrix(~ var-1, z)


#AREA COVARIATE (SITE AND YEAR)
#Coefficients
b.a1 <- 0.3
b.a2 <- 0.8
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


lam <- exp(matrix(lam.alpha.site, nrow = max.sites, ncol = nyrs) + 
             matrix(b.lam.zoneB*zone[,2], nrow = max.sites, ncol = nyrs, byrow=T) + 
             matrix(b.a1*area1_sc, nrow = max.sites, ncol = nyrs, byrow=T) +
             matrix(b.a2*area2_sc, nrow = max.sites, ncol = nyrs, byrow=T) )


############################################################################
# PLOT
plot(lam,a1)
plot(lam,a2)
