

# Check outliers

# SG 14 - 18
setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data")
manag <- read.csv("management_area_200.csv")

manag <- manag[ , c(1,2,11:15)] # Select years 2014 - 2018
area_sg <- as.matrix(manag[ ,c(3:7)])

# Outliers
area_SG_HA <- NULL
for (i in 1:nyrs){
  area_SG_HA <- c(area_SG_HA,area_sg[,i])} # Create vector

boxplot.stats(area_SG_HA)$out
boxplot(area_SG_HA) # Remove observations of more than 20 in models
