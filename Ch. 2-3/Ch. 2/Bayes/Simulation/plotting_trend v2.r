

# PLOT YEAR EFFECT

###################################################################
##                       NORMAL PLOT                            ###
###################################################################

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/Results/TRIM")
load("8.TRIM_Terax.RData")
yrs2 <- c(0, 1, 2, 3, 4, 5, 6, 7, 8) 

# 1. Calculate predictions for both zones
# PREDICTION FOR N FROM ITERATIONS
# out$samples contains all the iterations for each of the 3 chains ([1],[2],[3])
outall <- do.call(rbind,out$samples) # Here I put the three chains together

# ORIENTALES (ZONE)
pred <- list() # Pred contains the list of the prediction of abundance N for each iteration 
                #(one prediction line per iteration)

for(i in 1:dim(outall)[1]){ 
  pred[[i]] <- exp(outall[i,"mu.lam"]+ 
                     outall[i,"bzB.lam"]*1 + 
                     outall[i,"bYear.lam"]*yrs2) 
}

predall <- do.call(rbind,pred) # All predictions/iterations together in one data frame (where columns are the prediction per each predictor (area) values)
lci <- uci <- mean.pred <- 0 
for(i in 1:length(yrs2)){
  
  lci[i]  <- quantile(predall[,i],probs = 0.025) 
  uci[i]  <- quantile(predall[,i],probs = 0.975)
  mean.pred[i]  <- mean(predall[,i])
}

#OCCIDENTALES (ZONE)
pred_oc <- list()
for(i in 1:dim(outall)[1]){
  
  pred_oc[[i]] <- exp(outall[i,"mu.lam"]+ 
                        outall[i,"bzB.lam"]*0 + 
                        outall[i,"bYear.lam"]*yrs2) 
}

predall_oc <- do.call(rbind,pred_oc) 
lci_oc <- uci_oc <- mean.pred_oc <- 0 

for(i in 1:length(yrs2)){
  
  lci_oc[i]  <- quantile(predall_oc[,i],probs = 0.025) 
  uci_oc[i]  <- quantile(predall_oc[,i],probs = 0.975)
  mean.pred_oc[i]  <- mean(predall_oc[,i])
}

# 2. Plot

plot(-15, xlim=c(0,8), ylim=c(0,1), main = "Terax_HDS", xlab = "Year", ylab = "Abundance")

polygon( x = c(yrs2, rev(yrs2)),
         y = c(lci, rev(uci)), 
         col = adjustcolor(c("grey"),alpha.f = 0.6),
         border = NA)
points(mean.pred~yrs2, type="l")


polygon( x = c(yrs2, rev(yrs2)),
         y = c(lci_oc, rev(uci_oc)), 
         col = adjustcolor(c("red"),alpha.f = 0.3),
         border = NA)
points(mean.pred_oc~yrs2, type="l", col = "red")

legend("topleft",fill=adjustcolor(c("red","black"),alpha.f = 0.8),
       border=c("red","black"),legend = c("Oc", "Or"),
       box.lwd=0.1,
       bty = "n")


###################################################################
##                       POPULATION TREND PLOT                  ###
###################################################################


setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/Results/TRIM")
load("8.TRIM_Terax.RData")
yrs2 <- c(0, 1, 2, 3, 4, 5, 6, 7, 8) 

# 1. Calculate predictions for both zones

outall <- do.call(rbind,out$samples) 

# Total population (since both follow the same trend)
pred <- matrix(NA, dim(outall)[1], length(yrs2))
for(i in 1:dim(outall)[1]){ 
  ##calculate population, year 1
  pred[i,1] <- as.vector(outall[i,"Ntotal_clus[1]"])
  ##calculate populations, year 2-8, based on beta(Year)
  for (t in 2:length(yrs2)){
  pred[i,t] <- pred[i,(t-1)] * # Here I add the starting population size as a baseline for the trend 
    exp(outall[i,"bYear.lam"])
  }
}

predall <- pred
lci <- uci <- mean.pred <- 0 

for(i in 1:length(yrs2)){
  lci[i]  <- quantile(predall[,i],probs = 0.025) 
  uci[i]  <- quantile(predall[,i],probs = 0.975)
  mean.pred[i]  <- mean(predall[,i])
}

##technically, you'd want to start the population at the expected population size, 
##so technically you'd want to base your plot on lambda. not N.
##Calculate total exp pop size based on exp(lam) times number of sites surveyed in each zone
##times number of animals in a cluster (see code below)
##I don't know how many sites you simulated in each zone so I am making up numbers here
##I also don't know the average cluster size, so I also made that up
##You'd have to substitute these with the real values
Jzone1<-60
Jzone2<-60
clust<-3.5

# Valores reales
Jzone1 <- 69 # Intercept. Occidental (0)
Jzone2 <- 97 # Oriental (1)
clust <- 1.136076

# Total expected population

pred.exp <- matrix(NA, dim(outall)[1], length(yrs2))
for(i in 1:dim(outall)[1]){ 
  pred.exp[i,1] <- ( exp(outall[i,"mu.lam"]+ 
                  outall[i,"bzB.lam"] )*Jzone2 + ##exp pop size in zone 1 yr 1
              exp(outall[i,"mu.lam"])*Jzone1) * clust  ##exp pop size in zone 2 yr 1
  ###this piece calculates the expected populations in year 2-8 based on
  ###beta(Year)
  for (t in 2:length(yrs2)){
    pred.exp[i,t] <- pred.exp[i,(t-1)] * # Here I add the starting population size as a baseline for the trend 
      exp(outall[i,"bYear.lam"])
  }
}

predall.exp <- pred.exp
lci.exp <- uci.exp <- mean.pred.exp <- 0 

for(i in 1:length(yrs2)){
  lci.exp[i]  <- quantile(predall.exp[,i],probs = 0.025) 
  uci.exp[i]  <- quantile(predall.exp[,i],probs = 0.975)
  mean.pred.exp[i]  <- mean(predall.exp[,i])
}


# #OCCIDENTALES (ZONE)
# pred_oc <- list()
# for(i in 1:dim(outall)[1]){
#   
#   pred_oc[[i]] <- outall[i,"Ntotal_clus[1]"] + 
#     exp(outall[i,"mu.lam"] + 
#           outall[i,"bzB.lam"]*0 + 
#           outall[i,"bYear.lam"]*yrs2) 
# }
# 
# predall_oc <- do.call(rbind,pred_oc) 
# lci_oc <- uci_oc <- mean.pred_oc <- 0 
# 
# for(i in 1:length(yrs2)){
#   
#   lci_oc[i]  <- quantile(predall_oc[,i],probs = 0.025) 
#   uci_oc[i]  <- quantile(predall_oc[,i],probs = 0.975)
#   mean.pred_oc[i]  <- mean(predall_oc[,i])
# }

# 2. Plot

plot(-15, xlim=c(0,8), ylim=c(0,200), main = "Terax_HDS_N", xlab = "Year", ylab = "Abundance")

polygon( x = c(yrs2, rev(yrs2)),
         y = c(lci, rev(uci)), 
         col = adjustcolor(c("grey"),alpha.f = 0.6),
         border = NA)
points(mean.pred~yrs2, type="l")

##add in actual abundance estimates to check

points(yrs2, out$summary[grep("Ntotal_clus", rownames(out$summary)),1])

# 
# polygon( x = c(yrs2, rev(yrs2)),
#          y = c(lci_oc, rev(uci_oc)), 
#          col = adjustcolor(c("red"),alpha.f = 0.3),
#          border = NA)
# points(mean.pred_oc~yrs2, type="l", col = "red")

###not adjusted to new plot
#legend("topleft",fill=adjustcolor(c("red","black"),alpha.f = 0.8),
#       border=c("red","black"),legend = c("Oc", "Or"),
#       box.lwd=0.1,
#       bty = "n")

##########################################################################################
##### plot expected abundance
plot(-15, xlim=c(0,8), ylim=c(0,200), main = "Terax_HDS_expectedN", xlab = "Year", ylab = "Abundance")

polygon( x = c(yrs2, rev(yrs2)),
         y = c(lci.exp, rev(uci.exp)), 
         col = adjustcolor(c("grey"),alpha.f = 0.6),
         border = NA)
points(mean.pred.exp~yrs2, type="l")

##add in actual abundance estimates from the model
points(yrs2, out$summary[grep("Ntotal_clus", rownames(out$summary)),1], pch=19)


