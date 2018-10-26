


n <- 30 # Population of 30 species


#
lsig <- rnorm(n,3.4,0.5) # Sigma. Comes from the same distribution but there is one 
                    # different value for each species
hist(sig)
sig <- exp(lsig) # You make the exponential because sigma cant be negative
                  # Then sig is the value you introduce in the det.function

x <- runif(40, 0, 100) # Simulation of distances

g <- function(x, sig) exp(-x^2/(2*sig^2)) # Define detection function (p = g(x,sig))
p <- g(x, sig=sig[1]) # Prob of one species

# The detection functions would be different because sigma is different for each species
curve(g(x, sig=sig[1]), 0, 200, xlab="Distance (x)", ylab="Detection prob.", lwd = 2, frame = F)

for (i in 2:n){ # For each species
  curve(g(x, sig=sig[i]), 0, 200, add=TRUE, lwd = 2, col = c[i])
}

# OTHER WAY TO DO IT
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


