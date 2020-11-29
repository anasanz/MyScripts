
################################################################
###   UNDERSTAND USE-AVAILABLE STRUCTURE FROM RSPF PACKAGE   ###
################################################################


n.used <- 1000
m <- 10
n <- n.used * m
set.seed(1234)
x <- data.frame(x1=rnorm(n), x2=runif(n))
cfs <- c(1.5,-1,0.5)
## fitting Exponential RSF model
dat1 <- simulateUsedAvail(x, cfs, n.used, m, link="log")
m1 <- rsf(status ~ .-status, dat1, m=0, B=0)
summary(m1)
## fitting Logistic RSPF model
dat2 <- simulateUsedAvail(x, cfs, n.used, m, link="logit")
m2 <- rspf(status ~ .-status, dat2, m=0, B=0)
summary(m2)


#sIMULATE IDs
dat1$ID <- c(rep("A", 500), rep("B", 500), rep("A", 5000), rep("B", 5000))
m 

