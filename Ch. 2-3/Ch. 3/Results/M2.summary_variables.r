

#######################################################################################
#####                 METHODS: Explanatory table MEASURES                 ##############
#######################################################################################

library(dplyr)
library(tidyr)


setwd("D:/PhD/Third chapter/Data")
green <- read.csv("GREEN_15_19.csv")
aes <- read.csv("AES_15_19.csv")
tfm <- read.csv("SG_15_19.csv")


mean(as.matrix(green[,c(3:6)]))
mean(as.matrix(aes[,c(3:6)]))
mean(as.matrix(tfm[,c(3:6)]))

sd(as.matrix(green[,c(3:6)]))
sd(as.matrix(aes[,c(3:6)]))
sd(as.matrix(tfm[,c(3:6)]))

max(as.matrix(green[,c(3:6)]))
max(as.matrix(aes[,c(3:6)]))
max(as.matrix(tfm[,c(3:6)]))

sum(as.matrix(green[,c(3:6)]))
sum(as.matrix(aes[,c(3:6)]))
sum(as.matrix(tfm[,c(3:6)]))


hist(as.matrix(green[,c(3:6)]))
hist(as.matrix(aes[,c(3:6)]))
hist(as.matrix(tfm[,c(3:6)]))

# Area per year

colMeans(green[,c(3:6)])
colMeans(aes[,c(3:6)])
colMeans(tfm[,c(3:6)])


# Anova to see differences among means
df <- data.frame(green = as.vector(as.matrix(green[,c(3:6)])), 
                 aes = as.vector(as.matrix(aes[,c(3:6)])), 
                 tfm = as.vector(as.matrix(tfm[,c(3:6)])))

df2 <- gather(df, measure, area)

m1 <- aov(area ~ measure, df2) # There are differences
summary(m1) # There are differences

tapply(df2$area,df2$measure,mean)

boxplot(df2$area ~ df2$measure)

m1Tukey <- TukeyHSD(m1,"measure") # Check the levels where there are differences
plot(m1Tukey)
