
# Prediction of model for community parameter FallowTFM

rm(list=ls())


library(rjags)
library(jagsUI)
library(dplyr)

# ---- LOAD DATA ----

setwd("D:/PhD/Third chapter/Data")
d <- read.csv("DataDS_ch3_15_19_READY_FIXED.csv")

# To restrict distribution of PTALC and CABRA: remove observations out of the distr.range (probably a mistake)
unique(d$Region.Label)
d <- d[-which(d$Species == "CABRA" & d$Region.Label %in% c("BA", "SI", "BM", "AL")), ]
d[which(d$Species == "PTALC" & d$Region.Label %in% c("BA", "SI", "BM", "AL", "BE")), ]
d[which(d$Species == "PTORI" & d$Region.Label %in% c("AF","SI", "BM", "AL", "BE")), ]

# Information: bins, years, sites, species

strip.width <- 500 				# strip half-width, w (in this example only one side of the line transect is surveyed)
dist.breaks <- c(0,25,50,100,200,500)
int.w <- diff(dist.breaks) # width of distance categories (v)
midpt <- (int.w/2) + dist.breaks[-6]
nG <- length(dist.breaks)-1	

yrs <- c(2015, 2016, 2017, 2018, 2019)
yrs2 <- c(1, 2, 3, 4, 5)
nyrs <- length(yrs)

all.sites <- unique(d$transectID)
all.sites <- sort(all.sites, descreasing = TRUE)
max.sites <- length(all.sites)
total.sites <- max.sites*nyrs # Total number of site-year combinations


#   All detections all species (to detect sites not sampled)   

m <- matrix(NA, nrow = max.sites, ncol = nyrs)
rownames(m) <- all.sites
colnames(m) <- yrs

# Add counts > 0
count <- aggregate(Species ~ Year + transectID, FUN = length, data = d)

for (i in 1:nrow(count)){
  m[which(rownames(m) %in% count$transectID[i]), which(colnames(m) %in% count$Year[i])] <- count$Species[i]
}

not_sampled <- is.na(m) # These are the sites not sampled in a given year. There are errors (NA por fichas no pasadas)

#  Select the species that I want to analyze 

# Remove species with bad bp-values in 
bad_bp <- c("GACRI", "GATHE", "PADOM", "STSSP") # to remove all species with bad bp except MICAL and MECAL
d <- d[-which(d$Species %in% bad_bp), ]

sp <- as.character(unique(d$Species))
sp <- sort(sp)
nSpecies <- length(sp)


#  Counts per transect y (from distance observations) 

data_sp <- array(0, c(max.sites, nyrs, nSpecies)) # Array to store all species all counts


for (s in 1:nSpecies){
  d_sp <- d[which(d$Species %in% sp[s]), ] # Select SP
  count_sp <- aggregate(Species ~ Year + transectID, FUN = length, data = d_sp) # Group counts per year and site
  
  m_sp <- matrix(0, nrow = max.sites, ncol = nyrs) # df to store data of individual species in loop
  rownames(m_sp) <- all.sites
  colnames(m_sp) <- yrs
  
  for (i in 1:nrow(count_sp)){ # Fill counts per transect and year in df
    m_sp[which(rownames(m_sp) %in% count_sp$transectID[i]), which(colnames(m_sp) %in% count_sp$Year[i])] <- count_sp$Species[i] 
  }
  m_sp[is.na(m)] <- NA # NA in sites not sampled that year
  print(sum(m_sp, na.rm = TRUE)) 
  data_sp[,,s] <- m_sp # Store in array with all species
}

### ---- Co-variates ----

setwd("D:/PhD/Third chapter/Data")
aes <- read.csv("AES_15_19.csv")
sg <- read.csv("SG_15_19.csv")
green <- read.csv("GREEN_15_19.csv")
crop_diversity <- read.csv("crop_richness_500.csv")
field_size <- read.csv("av_fieldsize_500.csv")


aes <- aes[which(aes$Codi %in% all.sites), ] # Select transects with census
sg <- sg[which(sg$Codi %in% all.sites), ] 
green <- green[which(green$Codi %in% all.sites), ] 
crop_diversity <- crop_diversity[which(crop_diversity$Codi %in% all.sites), ] 
field_size <- field_size[which(field_size$Codi %in% all.sites), ] 


# Be sure the fields are in the same order
order <- as.data.frame(m)
order_codi <- as.vector(rownames(order))
order$Codi <- order_codi
aes <- left_join(order,aes, by = "Codi")
sg <- left_join(order,sg, by = "Codi")
green <- left_join(order,green, by = "Codi")
crop_diversity <- left_join(order,crop_diversity, by = "Codi")
field_size <- left_join(order,field_size, by = "Codi")

# ABUNDANCE MODEL #

# Need to log-transform the FALLOW variables (AES and SG) because of extreme values (see script Explore_corr.r)

# Area AES
area_aes_real <- as.matrix(aes[ ,c(8:12)])
area_aes_real <- area_aes_real + 0.001 # Add a constant for log-transform (log(0) = Inf)
area_aes <- log(area_aes_real)

aes_mean <- mean(area_aes) # Also scale to unify with different variables like crop diversity
aes_sd <- sd(area_aes)
aes_sc <- (area_aes - aes_mean) / aes_sd

# Area SG
area_sg_real <- as.matrix(sg[ ,c(8:12)])
area_sg_real <- area_sg_real + 0.001 # Add a constant for log-transform (log(0) = Inf)
area_sg <- log(area_sg_real)

sg_mean <- mean(area_sg)
sg_sd <- sd(area_sg)
sg_sc <- (area_sg - sg_mean) / sg_sd

# Area GREEN
area_green_real <- as.matrix(green[ ,c(8:12)])
area_green_real <- area_green_real + 0.001 # Add a constant for log-transform (log(0) = Inf)
area_green <- log(area_green_real)


green_mean <- mean(area_green)
green_sd <- sd(area_green)
green_sc <- (area_green - green_mean) / green_sd

# Crop diversity
crop_diversity <- as.matrix(crop_diversity[ ,c(8:12)])

cd_mean <- mean(crop_diversity)
cd_sd <- sd(crop_diversity)
cd_sc <- (crop_diversity - cd_mean) / cd_sd

# Field size
field_size <- as.matrix(field_size[ ,c(8:12)])

fs_mean <- mean(field_size)
fs_sd <- sd(field_size)
fs_sc <- (field_size - fs_mean) / fs_sd



# ---- LOAD RESULTS ----

# Load the three chains
load("D:/PhD/Third chapter/Data/model/15.1.1/JagsOutFOR15.1.1a.RData")
outa <- out
load("D:/PhD/Third chapter/Data/model/15.1.1/JagsOutFOR15.1.1b.RData")
outb <- out
load("D:/PhD/Third chapter/Data/model/15.1.1/JagsOutFOR15.1.1c.RData")
outc <- out
class(outc)


out.list<- list()
out.list[[1]] <- as.mcmc(outa$samples[[1]])
out.list[[2]] <- as.mcmc(outb$samples[[1]])
out.list[[3]] <- as.mcmc(outc$samples[[1]])

out.list <- as.mcmc.list(out.list)

source("D:/PhD/MyScripts/Ch. 2-3/Ch. 3/Results/Functions/ProcessCodaOutput.R")

out <- ProcessCodaOutput(out.list)
outall <- out$sims.list # 3 chains together 
outall <- as.data.frame(outall)
names(outall)

# PREDICTION OF COMMUNITY MEAN SG

# Variables for predictions:

# SG (variable that I want to predict)
area_SGpred <- seq(min(sg_sc), max(sg_sc),length.out = 100) # Prediction: log_scale and scaled
area_SG_HA <- seq(min(area_sg_real), max(area_sg_real),length.out = 100) # To plot with unscaled, backtransformed values

# Fixed variables
area_AES_forpred <- rep(mean(aes_sc),100) 
area_GREEN_forpred <- rep(mean(green_sc),100) 
FS_forpred <- rep(mean(fs_sc),100) 
CD_forpred <- rep(mean(cd_sc),100) 

# Model: 

pred_community <- list()
for(i in 1:dim(outall)[1]){ 
  
  pred_community[[i]] <- exp(outall[i,"mu_l"] 
                           + outall[i,"mu_a1"] * area_SGpred 
                           + outall[i,"mu_a2"] * area_AES_forpred
                           + outall[i,"mu_a3"] * area_GREEN_forpred
                           + outall[i,"mu_cd"] * FS_forpred
                           + outall[i,"mu_fs"] * CD_forpred )
   } # Pred contains the list of the prediction of the community lambda for each iteration #(one prediction line per iteration)

predall <- do.call(rbind,pred_community) # All predictions/iterations together in one data frame (where columns are the prediction per each predictor (area) values)
lci <- uci <- mean.pred <- 0 

for(i in 1:length(area_SGpred)){
  lci[i]  <- quantile(predall[,i],probs = 0.025) 
  uci[i]  <- quantile(predall[,i],probs = 0.975)
  mean.pred[i]  <- mean(predall[,i])
}

# Plot:

setwd("D:/PhD/Third chapter/Data/Results")
pdf("Fig1b.pdf",6,4)

par(mfrow = c(1,1),
    mar = c(5,5,5,4))
plot(-15, xlim=c(min(area_SG_HA),80), ylim=c(0,1), xaxs= "i", main = " ", xlab = " ", ylab = "Mean community abundance (transect)") # area_SG_HA unscaled variable
mtext("Mean community abundance (transect)", side = 2, line = 3, cex = 1)
mtext("Fallow TFM (ha)", side = 1, line = 3, cex = 1)


polygon( x = c(area_SG_HA, rev(area_SG_HA)),
         y = c(lci, rev(uci)), 
         col = adjustcolor(c("grey"),alpha.f = 0.6),
         border = NA)
points(mean.pred~area_SG_HA, type="l")

dev.off()

