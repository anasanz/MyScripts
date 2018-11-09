###===================================================================###
###              Case study: King Fire bird data                      ###
###                 Bayesian N-mixture model                          ###
###===================================================================### 

rm(list=ls())
#install.packages("rjags")
library(rjags)

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/Examples")

counts <- as.matrix(read.csv("HEWA_counts.csv"))
burn <- c(as.matrix(read.csv("Site_cov.csv")))
wind <- as.matrix(read.csv("Survey_cov.csv"))

# ===================== MODEL IN JAGS ======================================

## Specify data that will be used in the model

n.site = nrow(counts)
n.obs = ncol(counts)

field.data <- list(n.site = n.site,  n.obs = n.obs, y = counts, burn = burn, wind = wind)
1/100*100

# tau is precision. Low precision means that is very spread (very uninformative)

## Write model

model1.string <-"
    model {

    # Priors
  B0 ~ dnorm(0,100)
  B1 ~ dnorm(0,0.01)
  A0 ~ dnorm(0,0.1)
  A1 ~ dnorm(0,0.001)

    # Likelihood    
    
for (j in 1:n.site){
  N[j] ~ dpois(lambda[j])
  log(lambda[j]) = B0 + B1*burn[j]
for (k in 1:n.obs){
y[j,k] ~ dbin(p[j,k],N[j])
logit(p[j,k]) = A0 + A1*wind[j,k]
}}


    # Derived quantities
	totalN <- sum(N) # Estimate total population size across all sites

	}"

Nmixture_m<-textConnection(model1.string)

## Inits function

inits <- function(){list(B0 = rnorm(1,0,1) , B1 = rnorm(1,0,1), 
                         A0 = rnorm(1,0,1), A1 = rnorm(1,0,1), 
                         N = apply(counts,1,max)+1)}
# Apply: calculates the maximum value per row (thats why you have a 1)
inits() # It is a function to draw different values all the time

## Parameters to estimate
params <- c("A0", "A1", "B0", "B1", "N", "totalN")

## MCMC settings
nc <- 3
nb <- 1000
ni <- 10000
nt <- 1 # dont thin unless it is essential to make output manageable

## Start Gibbs sampling
jags.mod <- jags.model(Nmixture_m ,data = field.data ,n.chains = nc ,n.adapt = 1000, inits = inits)

chains <- coda.samples(jags.mod, params, ni, nt, n.burnin=nb)

length(chains)
head(chains[[1]])
nrow(chains[[1]])

## Results
summary(chains)

## Trace plots (see book p.173)
traceplot(chains[[1]][,"A0"])

## Convrgence check via R-hat (see book p.173)
...

## Chains convergence graphic (see book p.171)
...

