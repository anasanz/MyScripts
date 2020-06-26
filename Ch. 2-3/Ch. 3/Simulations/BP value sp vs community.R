####here, I simulate random numbers and see whether they are larger or smaller
### than 0, as an analog to the BP value calculations where we look at wheteher
### an observed residual is larger or smaller than a new residual

#x<-50 ##community of 50 species
x <- 36

#generate BP values, here, the probability of having a value >0
#pr<-runif(x, 0.1, 0.9) ##even distribution of BP values across community
#pr<-c(rep(0.15, 40), runif(10, 0.2, 0.9)) #skeewd distribution with many close to 0.1
pr <- df.bp_obs$mean

##set up matrix to hold random numbers, generating 1000 "iterations"
## these correspond to the MCMC iterations over which we average to get the BP value
res<-matrix(NA, x, 1000)

for (i in 1:x){
  for (j in 1:1000){
    ##generate a random number between 0 and 1, then determine if it's 
    ##smaller than a species' BP value, pr. If smaller, generate a positive
    ##number, if larger, generate a negative number
    ##this procedure ensures that for each species, in pr % of the 1000 simulations
    ##the number in "res" will be >0
    u<-runif(1) 
    res[i,j]<-ifelse(u<=pr[i], runif(1,0.1,10), runif(1,-10,-0.1))
  }
}

###calculate species level BP value: for each species, check how many values
### in res are >0, then divide by number of iterations
spBP<-apply(res, 1, function(x){sum(x>0)/length(x)})

##calculate community BP level; sum values in "res" over all species and calculate
## in how many iterations the resulting sum is >0
BP<-mean(apply(res,2,sum)>0)

##for the even community, the community BP value will always indicate fit
##for the extreme community, it will be 0, even though each individual species
##indicates fit


