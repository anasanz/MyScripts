

# Regression correlation Cover (vegetation) and food availability measures

setwd("C:/Users/ana.sanz/OneDrive/PhD/First chapter/Datos/Datos barbechos arrendados/Variables")

sp <- read.csv("Data_path_submission2_sp.csv", sep = ",", header=TRUE, fill = TRUE)
sp <- sp[which(sp$Species == "SC"), ]

g <- sp[ ,c(10:22)]
c <- cor(g) # Here, the pearson correlation matrix doesn't show high correlation
write.csv(c, "correlations.csv")

m1 <- lm(Cover ~ LAI_sd, data = sp)
summary(m1)
plot(Cover ~ LAI_sd, pch = 16, data = sp)
abline(m1, col = "red") 

m2 <- lm(Cover ~ SAI_sd, data = sp)
summary(m2)
plot(Cover ~ SAI_sd, pch = 16, data = sp)
abline(m2, col = "red")

# But here there is clear correlation....