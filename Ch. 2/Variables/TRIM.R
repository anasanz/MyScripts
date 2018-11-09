
install.packages("rtrim")

library(rtrim)

check_observations(ALRUF, model=2, changepoints=c(1,7))

#m1 <- trim(count ~ site + time, data=ALRUF,model=2)
# Site = Transect
# Time = Year

m1 <- trim(count ~ site + time, data=ALRUF, model=2, serialcor=TRUE, 
           overdisp=TRUE)

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