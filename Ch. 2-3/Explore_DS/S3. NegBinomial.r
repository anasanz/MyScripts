
# Exploring the negative binomial distribution
# Example: Does race help explain how many homicide victims a person knows? 

black <- c(119,16,12,7,3,2,0)
white <- c(1070,60,14,4,0,0,1)
resp <- c(rep(0:6,times=black), rep(0:6,times=white))
race <- factor(c(rep("black", sum(black)), rep("white", sum(white))),
                levels = c("white","black"))
victim <- data.frame(resp, race)

table(race) # Most respondents are white
race

with(victim, tapply(resp, race, mean)) # Blacks have a higher mean count than whites
with(victim, tapply(resp, race, var)) # For each race the sample variance is roughly double the mean. It appears we have overdispersion.

table(resp, race)
pGLM <- glm(resp ~ race, data=victim, family = poisson)
summary(pGLM) # Race is very significant. It appears blacks are much more likely to know someone who was a victim of a homicide

exp(coef(pGLM)[2]) # If we exponentiate the coefficient we get a ratio of sample means
# same thing
mean(victim$resp[victim$race=="black"])/mean(victim$resp[victim$race=="white"])

#In fact if we make a prediction with this model and exponentiate the results, we get the sample means
exp(predict(pGLM, newdata = data.frame(race=c("white","black"))))

#This says the count of known victims for whites is distributed as a Poisson with mean and variance equal to 0.09, 
# while the count of known victims for blacks is distributed as a Poisson with mean and variance equal to 0.52. 
# We already know from our exploratory analysis that the observed variances were much larger, so we shouldn’t be too pleased with the model’s estimated variances.
# If we examine the fitted counts, we’ll see even more evidence for the lack of fit

# fitted counts for Poisson GLM:
fmeans <- exp(predict(pGLM, newdata = data.frame(race = c("white","black"))))
fmeans

# To see difference observed and fitted:
# We generate fitted counts by using the dpois function along with the estimated means to predict the probability of getting 0 through 6. 
# We then multiplied those probabilities by the number of respondents to obtain fitted counts
fittedW <- dpois(0:6,lambda = fmeans[1]) * sum(victim$race=="white") 
fittedB <- dpois(0:6,lambda = fmeans[2]) * sum(victim$race=="black") 
data.frame(Response=0:6,BlackObs=black, BlackFit=round(fittedB,1), 
                                WhiteObs=white, WhiteFit=round(fittedW,1))

#Two of the more dramatic things to note is that we’re underfitting the 0 counts and overfitting the 1 counts.

# Now let’s try fitting a negative binomial model. We noticed the variability of the counts were larger for both races. 
# It would appear that the negative binomial distribution would better approximate the distribution of the counts.

library(MASS)
nbGLM <- glm.nb(resp ~ race, data=victim)
summary(nbGLM)

# First notice the coefficients are the same as before. Once again we can exponentiate the race coefficient to get a ratio of sample 
# means and make predictions to get the original sample means.

# fitted counts for Negative Binomial GLM:
fmeans <- exp(predict(pGLM, newdata = data.frame(race = c("white","black"))))
fmeans # same as pGLM

 # But notice the standard error for the race coefficient is larger, indicating more uncertainty in our estimate (0.24 versus 0.15). 
# This makes sense given the observed variability in our counts. Also notice the estimate of Theta. That is our dispersion parameter

nbGLM$theta

# And we can use it to get estimated variances for the counts:

fmeans + fmeans^2 * (1/nbGLM$theta)
with(victim, tapply(resp, race, var))

# Data simulation to understand the negative binomial distribution
library(magrittr)

library(magrittr) # for %>% operator
op <- par(mfrow=c(1,2))
set.seed(1)

victim$resp %>% `[`(victim$race=="white") %>% 
  table() %>% barplot(main = "Observed White")
rnbinom(n = 1149, size = nbGLM$theta, mu = exp(coef(nbGLM)[1])) %>% 
  table() %>%  barplot(main = "Simulated White")

victim$resp %>% `[`(victim$race=="black") %>% 
  table() %>% barplot(main = "Observed Black")
rnbinom(n = 159, size = nbGLM$theta, mu = exp(sum(coef(nbGLM)))) %>% 
  table() %>%  barplot(main = "Simulated Black")
par(op)

#The negative binomial distribution of the counts depends, or is conditioned on, race. 
# Each race has a different mean but a common dispersion parameter.

# Experiment with dispersion parameter in white observations:
victim$resp %>% `[`(victim$race=="white") %>% 
  table() %>% barplot(main = "Observed White")
rnbinom(n = 1149, size = nbGLM$theta, mu = exp(coef(nbGLM)[1])) %>% 
  table() %>%  barplot(main = "Simulated White")
rnbinom(n = 1149, size = 5, mu = exp(coef(nbGLM)[1])) %>% 
  table() %>%  barplot(main = "Simulated White")

# POISSON-GAMMA MIXTURE DISTRIBUTION (ANOTHER WAY TO WRITE NEGATIVE BINOMIAL)

# Treat lambda as a random variable: It has to be positive, so a good candidate is the gamma distribution
# The gamma distribution has the parameters shape (k) and scale (theta): lamda ~ gamma (k,theta)

gamma_theta <- 0.5 # Scale
gamma_k <- 0.85 # Shape

mean(rgamma(1000, scale = gamma_theta,
            shape = gamma_k))

set.seed(1024)
# simulate the parameter for the Poisson distribution,
# which are then also used for the color scale
alpha_values <- rgamma(10000, 
                       scale = gamma_theta,
                       shape = gamma_k)
random_poissons <- rpois(10000, alpha_values)
