### Plot 2 results ####

# Significant species model 6: 
# From: COPAL, MEAPI, MICAL, TERAX
# From these, the ones with differences in both (wald and CI): 


##############################################################################################################
################################           Output TRIM              ##########################################
###########################################################################################################
rm(list=ls())

library(dplyr)
library(rtrim)

# ---- PREPARE DATA ----

setwd("S:/PhD/Second chapter/Data")

d <- read.csv("DataDS_ready_ALL.csv")
colnames(d)[which(colnames(d) == "Count")] <- "Cluster" 

# Load species names

s_good <- c("MICAL", "COPAL", "MEAPI", "TERAX")

setwd("S:/PhD/Second chapter/Data/Results/TRIM/Final")
pdf("Fig2.pdf")

par(mfrow = c(2,2),
    mar = c(1,1.5,1,1.5),
    oma = c(3,3,2,3))

#1. Plot 2 first plots

for (xxx in 1:3){
  d_tr <- d[ ,which(colnames(d) %in% c("Species",  "T_Y", "Observer"))]
  d_tr_all <- data.frame(T_Y = unique(d_tr$T_Y), id = NA)
  
  d_tr$Observer <- as.character(d_tr$Observer) 
  d_tr_all_obs <- left_join(d_tr_all, d_tr)
  d_tr_all_obs <- d_tr_all_obs[ ,c(1,4)]
  d_tr_all_obs <- d_tr_all_obs[which(!duplicated(d_tr_all_obs)), ] # Table with all sampled fields and which observer sampled it
  
  
  sp <- d[which(d$Species == s_good[xxx]), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "Observer", "Cluster"))] # Select species spAL and all years
  sp <- arrange(sp, Year, transectID) #Ordered
  sp_detec_transectID <- unique(sp$transectID)
  sp$Observer <- as.character(sp$Observer) 
  
  absent <- anti_join(d_tr_all,sp) # Transects with 0 abundance, add to sp.
  colnames(absent)[2] <- "Banda" # Format it to add the rows to sp
  absent$T_Y <- as.character(absent$T_Y)
  absent$Species <- s_good[xxx]
  absent$Cluster <- NA
  absent <- left_join(absent, d_tr_all_obs)
  
  
  for (i in 1:nrow(absent)){ # Format to join absent - detections
    cent <- substr(absent$T_Y[i], 10,10) # To include SI102 (cents)
    cent <- as.numeric(cent)
    if(is.na(cent)){
      
      absent$Year[i] <- substr(absent$T_Y[i], 6,9)
      absent$transectID[i] <- substr(absent$T_Y[i], 1,4)
      
    } else { absent$Year[i] <- substr(absent$T_Y[i], 7,10)
    absent$transectID[i] <- substr(absent$T_Y[i], 1,5)}
  }
  absent$count <- 0
  sp$count <- 1
  all_sp <- rbind(sp,absent) # Include transects with abundance 0
  all_sp <- arrange(all_sp, Year, transectID) # Ordered
  
  absent$count <- 0
  
  
  sp <- all_sp[, which(colnames(all_sp) %in% c("Year", "transectID", "count"))] # Select species MECAL and all years
  colnames(sp)[which(colnames(sp) %in% "transectID")] <- "site"
  colnames(sp)[which(colnames(sp) %in% "Year")] <- "year"
  sp$year <- as.integer(sp$year)
  
  g <- aggregate(count ~ year, FUN = sum, data = sp)
  sp <- aggregate(count ~ year + site, FUN = sum, data = sp)
  
  check_observations(sp, model = 2)
  
  # Model 3
  m3 <- trim(count ~ site + year, data = sp, model = 3)
  i3 <- index(m3, which="both")
  
  #Extract the coefficients
  coef <- coefficients(m3, representation = c("trend"))
  coef_st <- coefficients(m3, representation = c("standard"))
  coef_dev <- coefficients(m3, representation = c("deviations"))
  sig_dev <- wald(m3) 
  tot <- totals(m3)
  sig <- overall(m3) # The p-value of this is the significant value for the overall trend in m3, = p value of the slope of m2 with all change points
  
  # Calculate 95% CI from se
  lci <- coef$add - 2*coef$se_add
  uci <- coef$add + 2*coef$se_add
  cont_zero <- between(0,lci,uci)
  
  est_trim <- round(coef$add[1], 2)
  
  significance_est_ci_trim <- ifelse(cont_zero == FALSE, 
                                     paste(est_trim,"*", sep = ""), 
                                     est_trim)
  
  # Calculate 95% CI for standard deviations
  lci_dev <- vector()
  uci_dev <- vector()
  cont_zero <- vector()
  cont_zero_dev <- vector()
  for (i in 1:9){
    lci_dev[i] <- coef_dev$add[i] - 2*coef_dev$se_add[i]
    uci_dev[i] <- coef_dev$add[i] + 2*coef_dev$se_add[i]
    cont_zero[i] <- between(0,lci_dev[i],uci_dev[i])
    cont_zero_dev[i]<- as.numeric(cont_zero[i]) #if its FALSE (0) is significant deviations (it does not conatain 0)
  }
  cont_zero_dev <- ifelse(cont_zero_dev == 0, 1, 0) # swith for plot (significant deviations are represented by 1)
  
  ##############################################################################################################
  ################################           Output HDS              ##########################################
  ###########################################################################################################
  
  setwd("S:/PhD/Second chapter/Data/Results/TRIM/compiled_final")
  load(paste("HDS_",s_good[xxx],".RData", sep = ""))
  summary <- as.data.frame(out$summary)
  
  results <- summary[which(rownames(summary) %in% c("popindex[1]", "popindex[2]", "popindex[3]", "popindex[4]", "popindex[5]", "popindex[6]", "popindex[7]", "popindex[8]", "popindex[9]",
                                                    "mu.lam.site", "sig.lam.site", "bzB.sig" , "bYear.lam")), ]
  year_dev <- summary[grep("log.lambda.year", rownames(summary)), c(1,3,7,10)]
  year_dev_hds <- year_dev$overlap0
  year_dev_hds <- ifelse(year_dev_hds == 0, 1, 0) 
  year_dev_hds[1] <- 0
  # CALCULATE CI
  # Based on expected N
  
  yrs2 <- c(0, 1, 2, 3, 4, 5, 6, 7, 8) 
  # 1. Calculate predictions for both zones
  outall <- do.call(rbind,out$samples) 
  # Total population (since both follow the same trend)
  pred.exp <- matrix(NA, dim(outall)[1], length(yrs2))
  for(i in 1:dim(outall)[1]){ 
    ##calculate population, year 1
    pred.exp[i,1] <- as.vector(outall[i,"lam.tot[1]"])
    ##calculate populations, year 2-8, based on beta(Year)
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
  
  est_hds <- round(results[3,1],2)
  
  significance_est_hds <- ifelse(results[3,10] == 0, 
                                 paste(est_hds,"*", sep = ""), 
                                 est_hds)
  
  
  ##############################################################################################################
  ################################           PLOT                   ##########################################
  ###########################################################################################################
  
  #Plot Both
  
  max_range <- max(uci.exp,tot$imputed, out$summary[grep("popindex", rownames(out$summary)),1])
  yrs2 <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
  
  # Load function with both graphs. You should have loaded first all results from both trim and hds and then load this function
  setwd("S:/PhD/MyScripts/Ch. 2-3/Ch. 2/Results")
  source("plot_trim_hds1.R")
  plot.trim.hds.overall_yo1(overall(m3), yrange = c(0, max_range))
  
  ################################################################
  # If only the function for trim is runned(plot.trim.overall_yo), you have to run this for the hds model:
  # PLOT HDS
  
  #polygon( x = c(yrs2, rev(yrs2)),
  #         y = c(lci.exp, rev(uci.exp)), 
  #         col = adjustcolor(c("grey"),alpha.f = 0.6),
  #         border = NA)
  #
  #points(yrs2, out$summary[grep("popindex", rownames(out$summary)),1], pch = 19, type = "l", col= adjustcolor("black",alpha.f = 0.4))
  #points(yrs2, out$summary[grep("popindex", rownames(out$summary)),1], pch = 19, type = "p", col= "white", cex = 1.5)
  #points(yrs2, out$summary[grep("popindex", rownames(out$summary)),1], pch = 19, type = "p", col= adjustcolor("black",alpha.f = 0.4), cex = 1.5)
  #
  #points(mean.pred.exp ~ yrs2, type="l", col= "black", lwd = 2)
  
}

#1. Plot last plot with function plot.trim.hds.overall_yo2 (that contains x-axes)

for (xxx in 4:5){
  d_tr <- d[ ,which(colnames(d) %in% c("Species",  "T_Y", "Observer"))]
  d_tr_all <- data.frame(T_Y = unique(d_tr$T_Y), id = NA)
  
  d_tr$Observer <- as.character(d_tr$Observer) 
  d_tr_all_obs <- left_join(d_tr_all, d_tr)
  d_tr_all_obs <- d_tr_all_obs[ ,c(1,4)]
  d_tr_all_obs <- d_tr_all_obs[which(!duplicated(d_tr_all_obs)), ] # Table with all sampled fields and which observer sampled it
  
  
  sp <- d[which(d$Species == s_good[xxx]), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "Observer", "Cluster"))] # Select species spAL and all years
  sp <- arrange(sp, Year, transectID) #Ordered
  sp_detec_transectID <- unique(sp$transectID)
  sp$Observer <- as.character(sp$Observer) 
  
  absent <- anti_join(d_tr_all,sp) # Transects with 0 abundance, add to sp.
  colnames(absent)[2] <- "Banda" # Format it to add the rows to sp
  absent$T_Y <- as.character(absent$T_Y)
  absent$Species <- s_good[xxx]
  absent$Cluster <- NA
  absent <- left_join(absent, d_tr_all_obs)
  
  
  for (i in 1:nrow(absent)){ # Format to join absent - detections
    cent <- substr(absent$T_Y[i], 10,10) # To include SI102 (cents)
    cent <- as.numeric(cent)
    if(is.na(cent)){
      
      absent$Year[i] <- substr(absent$T_Y[i], 6,9)
      absent$transectID[i] <- substr(absent$T_Y[i], 1,4)
      
    } else { absent$Year[i] <- substr(absent$T_Y[i], 7,10)
    absent$transectID[i] <- substr(absent$T_Y[i], 1,5)}
  }
  absent$count <- 0
  sp$count <- 1
  all_sp <- rbind(sp,absent) # Include transects with abundance 0
  all_sp <- arrange(all_sp, Year, transectID) # Ordered
  
  absent$count <- 0
  
  
  sp <- all_sp[, which(colnames(all_sp) %in% c("Year", "transectID", "count"))] # Select species MECAL and all years
  colnames(sp)[which(colnames(sp) %in% "transectID")] <- "site"
  colnames(sp)[which(colnames(sp) %in% "Year")] <- "year"
  sp$year <- as.integer(sp$year)
  
  g <- aggregate(count ~ year, FUN = sum, data = sp)
  sp <- aggregate(count ~ year + site, FUN = sum, data = sp)
  
  check_observations(sp, model = 2)
  
  # Model 3
  m3 <- trim(count ~ site + year, data = sp, model = 3)
  i3 <- index(m3, which="both")
  
  #Extract the coefficients
  coef <- coefficients(m3, representation = c("trend"))
  coef_st <- coefficients(m3, representation = c("standard"))
  coef_dev <- coefficients(m3, representation = c("deviations"))
  sig_dev <- wald(m3) 
  tot <- totals(m3)
  sig <- overall(m3) # The p-value of this is the significant value for the overall trend in m3, = p value of the slope of m2 with all change points
  
  # Calculate 95% CI from se
  lci <- coef$add - 2*coef$se_add
  uci <- coef$add + 2*coef$se_add
  cont_zero <- between(0,lci,uci)
  
  est_trim <- round(coef$add[1], 2)
  
  significance_est_ci_trim <- ifelse(cont_zero == FALSE, 
                                     paste(est_trim,"*", sep = ""), 
                                     est_trim)
  
  # Calculate 95% CI for standard deviations
  lci_dev <- vector()
  uci_dev <- vector()
  cont_zero <- vector()
  cont_zero_dev <- vector()
  for (i in 1:9){
    lci_dev[i] <- coef_dev$add[i] - 2*coef_dev$se_add[i]
    uci_dev[i] <- coef_dev$add[i] + 2*coef_dev$se_add[i]
    cont_zero[i] <- between(0,lci_dev[i],uci_dev[i])
    cont_zero_dev[i]<- as.numeric(cont_zero[i]) #if its FALSE (0) is significant deviations (it does not conatain 0)
  }
  cont_zero_dev <- ifelse(cont_zero_dev == 0, 1, 0) # swith for plot (significant deviations are represented by 1)
  
  ##############################################################################################################
  ################################           Output HDS              ##########################################
  ###########################################################################################################
  
  setwd("S:/PhD/Second chapter/Data/Results/TRIM/compiled_final")
  load(paste("HDS_",s_good[xxx],".RData", sep = ""))
  summary <- as.data.frame(out$summary)
  
  results <- summary[which(rownames(summary) %in% c("popindex[1]", "popindex[2]", "popindex[3]", "popindex[4]", "popindex[5]", "popindex[6]", "popindex[7]", "popindex[8]", "popindex[9]",
                                                    "mu.lam.site", "sig.lam.site", "bzB.sig" , "bYear.lam")), ]
  year_dev <- summary[grep("log.lambda.year", rownames(summary)), c(1,3,7,10)]
  year_dev_hds <- year_dev$overlap0
  year_dev_hds <- ifelse(year_dev_hds == 0, 1, 0) 
  year_dev_hds[1] <- 0
  
  # CALCULATE CI
  # Based on expected N
  
  yrs2 <- c(0, 1, 2, 3, 4, 5, 6, 7, 8) 
  # 1. Calculate predictions for both zones
  outall <- do.call(rbind,out$samples) 
  # Total population (since both follow the same trend)
  pred.exp <- matrix(NA, dim(outall)[1], length(yrs2))
  for(i in 1:dim(outall)[1]){ 
    ##calculate population, year 1
    pred.exp[i,1] <- as.vector(outall[i,"lam.tot[1]"])
    ##calculate populations, year 2-8, based on beta(Year)
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
  
  est_hds <- round(results[3,1],2)
  
  significance_est_hds <- ifelse(results[3,10] == 0, 
                                 paste(est_hds,"*", sep = ""), 
                                 est_hds)
  
  
  ##############################################################################################################
  ################################           PLOT                   ##########################################
  ###########################################################################################################
  
  #Plot Both
  
  max_range <- max(uci.exp,tot$imputed, out$summary[grep("popindex", rownames(out$summary)),1])
  yrs2 <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
  
  # Load function with both graphs. You should have loaded first all results from both trim and hds to run this function
  setwd("S:/PhD/MyScripts/Ch. 2-3/Ch. 2/Results")
  source("plot_trim_hds2.R")
  plot.trim.hds.overall_yo2(overall(m3), yrange = c(0, max_range))
  
  ################################################################
  # If only the function for trim is runned(plot.trim.overall_yo), you have to run this for the hds model:
  # PLOT HDS
  
  #polygon( x = c(yrs2, rev(yrs2)),
  #         y = c(lci.exp, rev(uci.exp)), 
  #         col = adjustcolor(c("grey"),alpha.f = 0.6),
  #         border = NA)
  #
  #points(yrs2, out$summary[grep("popindex", rownames(out$summary)),1], pch = 19, type = "l", col= adjustcolor("black",alpha.f = 0.4))
  #points(yrs2, out$summary[grep("popindex", rownames(out$summary)),1], pch = 19, type = "p", col= "white", cex = 1.5)
  #points(yrs2, out$summary[grep("popindex", rownames(out$summary)),1], pch = 19, type = "p", col= adjustcolor("black",alpha.f = 0.4), cex = 1.5)
  #
  #points(mean.pred.exp ~ yrs2, type="l", col= "black", lwd = 2)
  
  mtext("Number of individuals", side = 2, line = 1, cex = 0.8, outer = TRUE)
  mtext("Year", side = 1, line = 2, cex = 0.8, outer = TRUE)
}

dev.off()
