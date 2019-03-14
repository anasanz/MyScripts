library(rjags)


set.seed(432104)
n <- 1000
x <- rnorm(n, 0, 5)

model1.string <-"
model {
for (i in 1:N){
x[i] ~ dnorm(mu, tau)
}
mu ~ dnorm(0,.0001)
tau <- pow(sigma, -2)
sigma ~ dunif(0,100)
}
"
model1.spec<-textConnection(model1.string)


jags.mod <- jags.model(model1.spec,
                   data = list('x' = x,
                               'N' = n),
                   n.chains=4,
                   n.adapt=100)

samps.jags<-jags.samples(jags.mod,
             c('mu', 'tau'),
             1000)

samps.coda <- coda.samples(jags.mod,
                           c('mu', 'tau'),
                           1000)

print(samps.jags)

summary(samps.coda)

plot(samps.coda)







n <- 16 # Number of years
a <-40 # Intercept
b<- -1.5 # Slope
sigma2 <-25 # Residual variance
x <- 1:16 # Values of covariate year
eps <- rnorm(n, mean =0, sd =sqrt(sigma2))
y <- a + b*x + eps # Assemble data set
plot((x+1989), y, xlab= "Year", las= 1, ylab= "Prop. occupied (%)", cex= 1.2)

print(summary(lm(y ~ I(x+1989))))
abline(lm(y~ I(x+1989)), col = "blue", lwd = 2)

#define model

model.string <-"
model {
  # Priors
  alpha ~ dnorm(0,0.001)
  beta ~ dnorm(0,0.001)
  sigma ~ dunif(0,100)
  # Likelihood
  for (i in 1:n) {
    y[i] ~ dnorm(mu[i], tau) # When you put the sd in jags you have to put the precision (tau)
    mu[i] <- alpha + beta*x[i]
  }
  
  # Derived quantities
  tau <- 1/ (sigma * sigma)
  p.decline <- 1-step(beta) # Probability of decline}"
  
  # Assess model fit using a sums-of-squares-type discrepancy
  for (i in 1:n) {
    residual[i] <- y[i]-mu[i] # Residuals for observed data
    predicted[i] <- mu[i] # Predicted values
    sq[i] <- pow(residual[i], 2) # Squared residuals for observed data
    
    # Generate replicate data and compute fit stats for them
    y.new[i] ~ dnorm(mu[i], tau) # one new data set at each MCMC iteration
    sq.new[i] <- pow(y.new[i]-predicted[i], 2) # Squared residuals for new data
  }
  fit <- sum(sq[]) # Sum of squared residuals for actual data set
  fit.new <- sum(sq.new[]) # Sum of squared residuals for new data set
  test <- step(fit.new - fit) # Test whether new data set more extreme
  bpvalue <- mean(test) # Bayesian p-value
}

modelspec<-textConnection(model.string)

# Bundle data
jags.data <- list("x"=x,"y"=y,"n"=n)
# Inits function
inits <- function(){ list(alpha= rnorm(1), beta =rnorm(1), sigma= rlnorm(1))} # rlnorm because it cant be negative
# Parameters to estimate
params <- c("alpha","beta", "p.decline", "sigma")

# MCMC settings
nc =3 ; ni= 1200 ; nb =200 ; nt= 1

# Start Gibbs sampler
sat.jags <- jags.model(modelspec,data=jags.data,n.chains=3,n.adapt =1000,inits=inits)

samps.coda <- coda.samples(sat.jags, params, ni, nt,n.burnin=nb)

samps.jags <- jags.samples(sat.jags, params, ni, nt,n.burnin=nb)

print(samps.jags)

summary(samps.coda)


plot(apply(samps.jags$predicted, 1, mean), apply(samps.jags$residual, 1, mean), main ="Residuals vs. predicted
values", las= 1, xlab= "Predicted values", ylab= "Residuals")
abline(h= 0)

lim <- c(0, 3200)
plot(samps.jags$fit, samps.jags$fit.new, main= "Graphical posterior predictive check", las= 1, xlab= "SSQ for actual data set", ylab ="SSQ for ideal (new) data sets", xlim= lim, ylim= lim)
abline(0, 1)

mean(samps.jags$fit.new > samps.jags$fit) # Bayesian p-value

plot((x+1989), y, xlab= "Year", las= 1, ylab ="Prop. occupied (%)", cex= 1.2)
abline(lm(y~ I(x+1989)), col= "blue", lwd= 2)
pred.y <- apply(samps.jags$alpha,1,mean) + apply(samps.jags$beta,1,mean) * x
points(1990:2005, pred.y, type= "l", col= "red", lwd= 2)
text(1994, 20, labels= "blue – ML; red - MCMC", cex= 1.2)





predictions <- array(dim =c(length(x), length(samps.jags$alpha)))
for(i in 1:length(x)){
predictions[i,] <- samps.jags$alpha + samps.jags$beta*i
}
LPB <- apply(predictions, 1, quantile, probs =0.025) # Lower bound
UPB <- apply(predictions, 1, quantile, probs =0.975) # Upper bound
plot((x+1989), y, xlab= "Year", las= 1, ylab= "Prop. occupied (%)", cex =1.2)

points(1990:2005, apply(samps.jags$alpha,1,mean) + apply(samps.jags$beta,1,mean) * x, type ="l", col= "black",lwd= 2)

points(1990:2005, LPB, type= "l", col= "grey", lwd= 2)
points(1990:2005, UPB, type ="l", col ="grey", lwd= 2)



hist(samps.jags$beta, main ="", col ="grey", xlab= "Trend estimate", xlim=
c(-2, 8))
abline(v= 0, col= "black", lwd= 2)

######################################################

l<-c(10,20,23,32,35)
w<-c(5,7,10,12,15)

print(summary(lm(l ~ w)))

n <- 5 # Number of frogs

#define model

model2.string <-"
model {
  # Priors
  alpha ~ dnorm(0,0.001)
  beta ~ dnorm(0,0.001)
  sigma ~ dunif(0,100)
  # Likelihood
  for (i in 1:n) {
    l[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha + beta*w[i]
  }
  
  # Derived quantities
  tau <- 1/ (sigma * sigma)
  p.decline <- 1-step(beta) # Probability of decline
  
  # Assess model fit using a sums-of-squares-type discrepancy
  for (i in 1:n) {
    residual[i] <- l[i]-mu[i] # Residuals for observed data
    predicted[i] <- mu[i] # Predicted values
    sq[i] <- pow(residual[i], 2) # Squared residuals for observed data
    
    # Generate replicate data and compute fit stats for them
    l.new[i] ~ dnorm(mu[i], tau) # one new data set at each MCMC iteration
    sq.new[i] <- pow(l.new[i]-predicted[i], 2) # Squared residuals for new data
  }
  fit <- sum(sq[]) # Sum of squared residuals for actual data set
  fit.new <- sum(sq.new[]) # Sum of squared residuals for new data set
  test <- step(fit.new - fit) # Test whether new data set more extreme
  bpvalue <- mean(test) # Bayesian p-value
}
"
modelspec<-textConnection(model2.string)

# Bundle data
jags.data <- list("w"=w,"l"=l,"n"=n)
# Inits function
inits <- function(){ list(alpha= rnorm(1), beta =rnorm(1), sigma= rlnorm(1))}
# Parameters to estimate
params <- c("alpha","beta", "p.decline", "sigma", "fit", "fit.new", "bpvalue","residual", "predicted")

# MCMC settings
nc =3 ; ni= 1200 ; nb =200 ; nt= 1

# Start Gibbs sampler
sat.jags <- jags.model(modelspec,data=jags.data,n.chains=3,n.adapt =1000,inits=inits)

samps.coda <- coda.samples(sat.jags, params, ni, nt,n.burnin=nb)

samps.jags <- jags.samples(sat.jags, params, ni, nt,n.burnin=nb)

print(samps.jags)

summary(samps.coda)


##############################################

l<-c(10,20,23,32,35,NA,NA,NA,NA,NA)
w<-c(5,7,10,12,15,16,17,18,19,20)

print(summary(lm(l ~ w)))

n <- 10 # Number of frogs

#define model

model2.string <-"
model {
  # Priors
  alpha ~ dnorm(0,0.001)
  beta ~ dnorm(0,0.001)
  sigma ~ dunif(0,100)
  # Likelihood
  for (i in 1:n) {
    l[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha + beta*w[i]
  }
  
  # Derived quantities
  
  tau <- 1/ (sigma * sigma)
  p.decline <- 1-step(beta) # Probability of decline
  
  # Assess model fit using a sums-of-squares-type discrepancy
  for (i in 1:n) {
    residual[i] <- l[i]-mu[i] # Residuals for observed data
    predicted[i] <- mu[i] # Predicted values
    sq[i] <- pow(residual[i], 2) # Squared residuals for observed data
    
    # Generate replicate data and compute fit stats for them
    l.new[i] ~ dnorm(mu[i], tau) # one new data set at each MCMC iteration
    sq.new[i] <- pow(l.new[i]-predicted[i], 2) # Squared residuals for new data
  }
  fit <- sum(sq[]) # Sum of squared residuals for actual data set
  fit.new <- sum(sq.new[]) # Sum of squared residuals for new data set
  test <- step(fit.new - fit) # Test whether new data set more extreme
  bpvalue <- mean(test) # Bayesian p-value
}
"
modelspec<-textConnection(model2.string)

# Bundle data
jags.data <- list("w"=w,"l"=l,"n"=n)
# Inits function
inits <- function(){ list(alpha= rnorm(1), beta =rnorm(1), sigma= rlnorm(1))}
# Parameters to estimate
params <- c("alpha","beta", "p.decline", "sigma", "fit", "fit.new", "bpvalue","residual", "predicted","l")

# MCMC settings
nc =3 ; ni= 1200 ; nb =200 ; nt= 1

# Start Gibbs sampler
sat.jags <- jags.model(modelspec,data=jags.data,n.chains=3,n.adapt =1000,inits=inits)
samps.coda <- coda.samples(sat.jags, params, ni, nt,n.burnin=nb)
samps.jags <- jags.samples(sat.jags, params, ni, nt,n.burnin=nb)
print(samps.jags)
summary(samps.coda)

##############################################################

