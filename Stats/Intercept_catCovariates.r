

###########################################################
# Understanding intercept logistic regression #
###########################################################

# Adding categorical variables into a linear regression


used <- rbinom(100,1,0.5)

cov <- rbinom(100,1,0.5)

cov1 <- 1-cov

cov1[1] <- 0

cov1+cov

cov2 <- rnorm(100,0,1)



inv.logit <- function(inValues) {
  
  1.0 / (1.0 + exp(-inValues))
  
}



m1 <- glm(used~1 ,family=binomial(link='logit'))

inv.logit(m1$coefficients[1])

mean(used)





m2 <-glm(used~cov, family=binomial(link='logit'))



inv.logit(m2$coefficients[1])

mean(used[cov==0])



m3 <-glm(used~cov +cov1, family=binomial(link='logit'))

m3

summary(m3)

inv.logit(m3$coefficients[1])

mean(used[cov1+cov==0])



m4 <-glm(used~cov +cov1 +cov2, family=binomial(link='logit'))

m4

summary(m4)