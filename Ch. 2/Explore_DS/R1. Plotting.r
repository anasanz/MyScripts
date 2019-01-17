
summary <- read.csv("8.2.Mecal.csv")

results <- summary[which(summary$X %in% c("Ntotal[1]", "Ntotal[2]", "Ntotal[3]", "Ntotal[4]", "Ntotal[5]", "Ntotal[6]",
                                          "Ntotal[7]", "Ntotal[8]", "mu.lam", "sig.lam", "bzB.lam", "ba1.lam", "ba2.lam")), ]

# Plot the trend of the population
plot(-100,ylim = c(0,1000), xlim=c(0,8),
     pch = 21, ylab = "N", xlab = " ", axes = FALSE, main = "MECAL")
axis(1, at = c(1,2,3,4,5,6,7,8), labels = yrs)
axis(2)
points(results[1:8,2],pch = 19)
x <- seq_along(results[1:8,2])
low_CI <- as.numeric(results$X2.5.[1:8])
up_CI <- as.numeric(results$X97.5.[1:8])
arrows(x, low_CI,x, up_CI, code=3, angle=90, length=0.04) # I guess this is ok? But Im sure there are better ways
# Is it usual to add like a trend line or something like that?

# To plot the relation with the co-variates (then I guess I need to take all the N right?)
results2 <- summary[9:1336, ]
plot(results2$mean ~ area_SG, ylab = "Abundance") 
plot(results2$mean ~ area_AES, pch = 16, ylab = "Abundance")
barplot(zon)

plot(results2$mean~area_AES, pch = 16, ylab = "Abundance")
