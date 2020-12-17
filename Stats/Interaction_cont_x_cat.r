## -------------------------------------------------
##          Understand and plot interactions
## ------------------------------------------------- 

## ---- Simulate data ----

set.seed(123)
n.sample <- 200

used <- rbinom(n.sample,1,0.5)
x1 <- data.frame(rep(c("A", "B", "C"), length.out = 200))
land <- model.matrix(~ x1$rep.c..A....B....C....length.out...200.-1, data = x1)
colnames(land) <- c("x0","x1","x2")
x3 <- runif(n.sample, -5, 5)
x4 <- 
a <- 5
b1 <- 3
b2 <- 4
b3 <- -3

y <- a + b1 * x1 + b2 * x2 + b3 * x1 * x2 + e
sim.dat <- data.frame(y, x1, x2)

## ---- Run model ----

mod.sim <- lm(y ~ x1 * x2, dat = sim.dat)
summary(mod.sim)

## ---- Plot ----

# 1. Plot the relation between x2(cont) and y for all observations
#     when x1(bin) = 0


plot(x = sim.dat[sim.dat$x1 == 0, ]$x2, y = sim.dat[sim.dat$x1 == 0, ]$y, 
     col = rgb(red = 0, green = 0, blue = 1, alpha = 0.25), pch = 19,
     xlab = "x2", ylab = "y")

abline(a = coef(mod.sim)[1], b = coef(mod.sim)[3], col = "blue", pch = 19, lwd = 2)

# 2. Add the data points where x1=1, 
#     and add the separate regression line for these points:

points(x = sim.dat[sim.dat$x1 == 1, ]$x2, y = sim.dat[sim.dat$x1 == 1, ]$y, 
       col = rgb(red = 1, green = 0, blue = 0, alpha = 0.25), pch = 19)

abline(a = coef(mod.sim)[1] + coef(mod.sim)[2], b = coef(mod.sim)[3] + coef(mod.sim)[4], 
       col = "red", lwd = 2)
