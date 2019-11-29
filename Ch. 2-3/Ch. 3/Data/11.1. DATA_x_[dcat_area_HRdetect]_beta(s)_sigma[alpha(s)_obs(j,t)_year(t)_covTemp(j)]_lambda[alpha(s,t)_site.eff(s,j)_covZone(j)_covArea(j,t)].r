

rm(list=ls())

library(rjags)
library(jagsUI)
library(dplyr)


# MODEL 11.1. in Community data

# sigma = exp(alpha(s) + observer(j,t) + year(t) + b*Temp(j,t))
# lambda = exp(alpha(s,t) + sp.site(s,j) + b1*fallowSG(j,t) + b2*fallowAES(j,t) + b3*Zone(j,t)

# ---- Data ----

setwd("C:/Users/ana.sanz/Documents/PhD/Third chapter/Data")
d <- read.csv("DataDS_ch3_20_18_final.csv")
d <- d[ ,colnames(d) %in% c("Species2", "Zone", "distance", "transectID", "Temp", "Observer", "Banda", "Year", "Sample.Label", "Count")]
d <- d[-which(d$Species2 == "TERAX_ind"), ] # To consider F and M as different species
d <- d[which(d$Year %in% c(2015,2016,2017)), ] # For now only 3 years, because I don't have the data for 2019 and the AES from 2018 and 2019

# Information: bins, years, sites, species

strip.width <- 500 				# strip half-width, w (in this example only one side of the line transect is surveyed)
dist.breaks <- c(0,25,50,100,200,500)
int.w <- diff(dist.breaks) # width of distance categories (v)
midpt <- (int.w/2) + dist.breaks[-6]
nG <- length(dist.breaks)-1	

yrs <- c(2015, 2016, 2017)
nyrs <- length(yrs)

all.sites <- unique(d$transectID)
all.sites <- sort(all.sites, descreasing = TRUE)
max.sites <- length(all.sites)
total.sites <- max.sites*nyrs # Total number of site-year combinations

sp <- unique(d$Species2)
sp <- sort(sp, descreasing = TRUE)
nSpecies <- length(sp)


# ----  All detections all species (to detect sites not sampled)   ---- 

m <- matrix(NA, nrow = max.sites, ncol = nyrs)
rownames(m) <- all.sites
colnames(m) <- yrs

# Add counts > 0
count <- aggregate(Species2 ~ Year + transectID, FUN = length, data = d)

for (i in 1:nrow(count)){
  m[which(rownames(m) %in% count$transectID[i]), which(colnames(m) %in% count$Year[i])] <- count$Species2[i]
}

not_sampled <- is.na(m) # These are the sites not sampled in a given year. There are errors (NA por fichas no pasadas)

# ---- Counts per transect y (from distance observations) ----

data_sp <- array(0, c(max.sites, nyrs, nSpecies)) # Array to store all species all counts

m_sp <- matrix(0, nrow = max.sites, ncol = nyrs) # df to store data of individual species in loop
rownames(m_sp) <- all.sites
colnames(m_sp) <- yrs

for (s in 1:nSpecies){
  d_sp <- d[which(d$Species2 %in% sp[s]), ] # Select SP
  count_sp <- aggregate(Species2 ~ Year + transectID, FUN = length, data = d_sp) # Group counts per year and site
  
  for (i in 1:nrow(count_sp)){ # Fill counts per transect and year in df
    m_sp[which(rownames(m_sp) %in% count_sp$transectID[i]), which(colnames(m_sp) %in% count_sp$Year[i])] <- count_sp$Species2[i] 
    }
  m_sp[is.na(m)] <- NA # NA in sites not sampled that year
  data_sp[,,s] <- m_sp # Store in array with all species
}

# ---- Co-variates ----

setwd("C:/Users/ana.sanz/Documents/PhD/Third chapter/Data")
manag <- read.csv("management_area_15_17.csv")

manag <- manag[which(manag$Codi %in% all.sites), ] # Select transects with census

# Be sure the fields are in the same order
order <- as.data.frame(m)
order_codi <- as.vector(rownames(order))
order$Codi <- order_codi
manag <- left_join(order,manag)

# ABUNDANCE MODEL #
# Area AES
area_aes <- as.matrix(manag[ ,c(6:8)])

aes_mean <- mean(area_aes)
aes_sd <- sd(area_aes)
aes_sc <- (area_aes - aes_mean) / aes_sd

# Area SG
area_sg <- as.matrix(manag[ ,c(9:11)])

sg_mean <- mean(area_sg)
sg_sd <- sd(area_sg)
sg_sc <- (area_sg - sg_mean) / sg_sd

# Zone (Occidental = 0; Oriental = 1)
zone <- order
for (i in 1:nrow(zone)){
  if(substr(zone$Codi[i], 1,2) == "BA"){zone[i,1:4] <- 0}
  if(substr(zone$Codi[i], 1,2) == "BM"){zone[i,1:4] <- 1}
  if(substr(zone$Codi[i], 1,2) == "SI"){zone[i,1:4] <- 1}
  if(substr(zone$Codi[i], 1,2) == "AF"){zone[i,1:4] <- 0}
  if(substr(zone$Codi[i], 1,2) == "BE"){zone[i,1:4] <- 1}
  if(substr(zone$Codi[i], 1,2) == "GR"){zone[i,1:4] <- 0}
  if(substr(zone$Codi[i], 1,2) == "AL"){zone[i,1:4] <- 0}
}
zone <- zone[,-4]

# OBSERVATION MODEL #

# Observer 
obs <- matrix(NA, nrow = max.sites, ncol = nyrs)
rownames(obs) <- all.sites
colnames(obs) <- yrs

d$Observer <- as.character(d$Observer)

for (i in 1:nrow(d)){
  obs[which(rownames(obs) %in% d$transectID[i]), which(colnames(obs) %in% d$Year[i])] <- d$Observer[i]}

# Temperature
temp <- matrix(NA, nrow = max.sites, ncol = nyrs)
rownames(temp) <- all.sites
colnames(temp) <- yrs

for (i in 1:nrow(d)){
  temp[which(rownames(temp) %in% d$transectID[i]), which(colnames(temp) %in% d$Year[i])] <- d$Temp[i]}

# Year
year <- matrix(NA, nrow = max.sites, ncol = nyrs)
rownames(year) <- all.sites
colnames(year) <- yrs

for (i in 1:nrow(d)){
  year[which(rownames(year) %in% d$transectID[i]), which(colnames(year) %in% d$Year[i])] <- d$Year[i]}

# ---- Specify data in JAGS format ----

# Distance class and ind

nind.sp <- list()
for (s in 1:nSpecies){
  nind.sp[[s]] <- sum(unlist(data_sp[,,s], use.names = F), na.rm = TRUE) # Just to know, but jags only wants the sum
}
nind <- do.call(sum, nind.sp)

dclass <- d$Banda # Right order?or it doesnt matter because it is to build the bin probabilities?

# Get one long vector with counts per year and site
yLong <- unlist(as.data.frame(m), use.names = F)

# Get one long matrix with counts and sites per species (columns)
yLong.sp <- matrix(NA, nrow = total.sites, ncol = nSpecies)
for (s in 1:nSpecies){
  yLong.sp[ ,s] <- unlist(as.data.frame(data_sp[,,s]), use.names = F) # With NA included (model estimating abundance in sites with no information)
}

# All this index and variables are site-speficic (not species specific) so they stay like this
sitesYears <- NULL # I did that loop but the normal version actually works, since is an index per site-year
for (i in 1:nyrs){
  sitesYears <- c(sitesYears,c(1:max.sites))
}


