

library(rtrim)

setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data")
d <- read.csv("DataDS_ready_ALL.csv")
sp <- d[which(d$Species == "BUOED"), which(colnames(d) %in% c("Year", "transectID", "Count"))] # Select species MECAL and all years
colnames(sp)[which(colnames(sp) %in% "Count")] <- "count"
colnames(sp)[which(colnames(sp) %in% "transectID")] <- "site"
colnames(sp)[which(colnames(sp) %in% "Year")] <- "year"

sp <- aggregate(count ~ year + site, FUN = sum, data = sp)

check_observations(sp, model = 2)

#m1 <- trim(count ~ site + time, data=ALRUF,model=2)
# Site = Transect
# Time = Year

m1 <- trim(count ~ site + year, data = sp, model = 3)
confint(m1)


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
index(m1, which="both")

#Plot with overall slope

plot(overall(m1))

plot(i1)
