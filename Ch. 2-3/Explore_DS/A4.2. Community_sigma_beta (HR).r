
rm(list=ls())

n <- 30 # Population of 30 species


#
lsig <- rnorm(n,3.4,0.5) # Here I simulate a different value of sigma for each species
                        #So, sigma Comes from the same distribution but there is one 
                      # different value for each species
hist(lsig)
sig <- exp(lsig) # You make the exponential because sigma cant be negative
                  # Then sig is the value you introduce in the det.function
beta <- rnorm(n,0.5,0.25)

x <- runif(40, 0, 100) # Simulation of distances at which we detect individuals

#♣g <- function(x, sig) exp(-x^2/(2*sig^2)) # Define detection function HN (p = g(x,sig))
g <- function(x, sig, b) 1 - exp(-(x/sig)^-b) # HR

p <- g(x, sig=sig[1], beta[1]) # Prob of one species (only to see an example)

# The detection functions would be different because sigma is different for each species
curve(g(x, sig=sig[1], b = beta[1]), 0, 200, xlab="Distance (x)", ylab="Detection prob.", lwd = 2, frame = F)

c <- colors()
for (i in 2:n){ # For each species
  curve(g(x, sig=sig[i], b = beta[i]), 0, 200, add=TRUE, lwd = 2, col = c[i])
}

# Try different things to see how the curve changes with sigma and beta

sigma <- c(5,5,50,50)
beta <- c(0.3, 3, 0.3, 3)
c <- c("red", "orange", "blue", "green")

curve(g(x, sig=sigma[1], b = beta[1]), 0, 200, xlab="Distance (x)", ylab="Detection prob.", lwd = 2, frame = F, col = c[1], ylim = c(0,1))

for (i in 1:n){ # For each species
  curve(g(x, sig=sigma[i], b = beta[i]), 0, 200, add=TRUE, lwd = 2, col = c[i])
}




# With sthis we see, that the values of sigma come from the same distribution (lsig),
# but can still vary between species

# OTHER WAY TO DO IT (NOT RUN)
# Calculate detection probabilities from detection function
g <- function(x, sig) exp(-x^2/(2*sig^2)) # Define detection function (p = g(x,sig))
p <- g(x, sig=sig)

m <- as.data.frame(matrix(NA, nrow = n, ncol = length(x)))

plot(x,m[2,], pch = 20)#, type = "l")
for (i in 1:n){ # For each individual
  p <- g(x, sig=sig[i]) # Different detection prob
  m[i,] <- p
}

plot(x,m[2,], pch = 20) # Detection probability decreases differently for each species


