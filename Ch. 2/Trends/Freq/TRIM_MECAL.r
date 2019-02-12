
rm(list=ls())

library(rtrim)

setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data")
d <- read.csv("DataDS_ready_ALL.csv")
sp <- d[which(d$Species == "MECAL"), which(colnames(d) %in% c("Year", "transectID", "Count"))] # Select species MECAL and all years
colnames(sp)[which(colnames(sp) %in% "Count")] <- "count"
colnames(sp)[which(colnames(sp) %in% "transectID")] <- "site"
colnames(sp)[which(colnames(sp) %in% "Year")] <- "year"

sp_year <- aggregate(count ~ year, FUN = sum, data = sp)
sp <- aggregate(count ~ year + site, FUN = sum, data = sp)

check_observations(sp, model = 2)

# MODEL 2

m1 <- trim(count ~ site + year, data = sp, model = 2)

summary(m1)

i1<-index(m1, which="both")
i1

# summarize the model
summary(m1)

#return time totals
totals(m1)
wald(m1)

#Retrieve goodness-of-fit
gof(m1)

#Extract the coefficients
coefficients(m1)

#Plot with overall slope
plot(overall(m1))

plot(i1)

# MODEL 3
m3 <- trim(count ~ site + year, data = sp, model = 3)

summary(m3)

i1 <- index(m3, which="both")

totals(m3)

wald(m3)

coefficients(m3)

plot(overall(m3))

plot(i1)
