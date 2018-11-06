
rm(list=ls())


# ---- Load ----
setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data")
dat <- read.csv("DataDS_ready.csv") # All data

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/Explore_species_occurrence/All")
num <- read.csv("NumberTrans_sp_year.csv") # Data to see most frequent species
prop <- read.csv("PropTrans_sp_year.csv")

# Most detected species are: 
# - Target: Mecal, terax, buoed
# - Other: Gacri, Mical, Pipic, Copal, Hirus, Padom
target <- c("MECAL", "TERAX", "BUOED")
other <- c("GACRI", "MICAL", "PIPIC", "COPAL", "HIRUS", "PADOM")
interesting <- c("MEAPI", "ALRUF", "UPEPO", "COGAR", "CABRA", "PTALC")

# ---- Temperature ----
# A. All Species 
dat_temp <- dat[-which(dat$Temp > 40),] # Remove from 40, its mistakes
hist(dat_temp$Temp, main = "All sp - Temperature") # 15ºC looks like the best

# Corrected by the number of transects:
# First, see how many transects were sampled at that temperature
prop <- as.data.frame(xtabs(~Temp + transectID, dat_temp))
prop$tmp <- NA
for (i in 1:nrow(prop)){
  if(prop$Freq [i] >= 1){prop$tmp[i] <- 1} else {prop$tmp[i] <- 0}}
transects <- tapply(prop$tmp,prop$Temp,sum)
# Second, divide observations/number of transects
observations <- xtabs(~Temp, dat_temp) # Number of observations with each temperature

freq <- observations/transects
par(mfrow = c(1,1))
plot(freq) # It seems like it is influenced by 23ºC
hist(freq, main = "All sp - Temperature") # 15ºC looks like the best

# B. Target species 

dat_temp <- dat[-which(dat$Temp > 40),]
par(mfrow = c(1,3))
for (i in 1:3){
  hist(dat_temp$Temp[which(dat_temp$Species %in% target[i])], main = paste(target[i], "- Temperature")) 
}

# Corrected by the number of transects for BUOED
dat_temp_buo <- dat_temp[which(dat_temp$Species == "BUOED"), ]
prop <- as.data.frame(xtabs(~Temp + transectID, dat_temp_buo))

prop$tmp <- NA
for (i in 1:nrow(prop)){
  if(prop$Freq [i] >= 1){prop$tmp[i] <- 1} else {prop$tmp[i] <- 0}}
transects <- tapply(prop$tmp,prop$Temp,sum)
observations <- xtabs(~Temp, dat_temp_buo) # Number of observations with each temperature

freq <- observations/transects
par(mfrow = c(1,1))
plot(freq, main = "buoed")

# Corrected by the number of transects for TERAX
dat_temp_ter <- dat_temp[which(dat_temp$Species == "TERAX"), ]
prop <- as.data.frame(xtabs(~Temp + transectID, dat_temp_ter))

prop$tmp <- NA
for (i in 1:nrow(prop)){
  if(prop$Freq [i] >= 1){prop$tmp[i] <- 1} else {prop$tmp[i] <- 0}}
transects <- tapply(prop$tmp,prop$Temp,sum)
observations <- xtabs(~Temp, dat_temp_ter) # Number of observations with each temperature

freq <- observations/transects
par(mfrow = c(1,1))
plot(freq, main = "terax")

# Corrected by the number of transects for ALRUF
dat_temp_ter <- dat_temp[which(dat_temp$Species == "ALRUF"), ]
prop <- as.data.frame(xtabs(~Temp + transectID, dat_temp_ter))

prop$tmp <- NA
for (i in 1:nrow(prop)){
  if(prop$Freq [i] >= 1){prop$tmp[i] <- 1} else {prop$tmp[i] <- 0}}
transects <- tapply(prop$tmp,prop$Temp,sum)
observations <- xtabs(~Temp, dat_temp_ter) # Number of observations with each temperature

freq <- observations/transects
par(mfrow = c(1,1))
plot(freq, main = "alruf")

# Conclusion: It seems like the fact that there was more in 15ºC was because
# there were more transects recorded in that temperature, and now the different
# species seem to react differently


# C. Other species 

dat_temp <- dat[-which(dat$Temp > 40),]
par(mfrow = c(2,3))
for (i in 1:length(other)){
  hist(dat_temp$Temp[which(dat_temp$Species %in% other[i])], main = paste(other[i], "- Temperature")) 
}

# Accounting for number of transects at each temperature
dat_temp_ter <- dat_temp[which(dat_temp$Species == "MECAL"), ]
prop <- as.data.frame(xtabs(~Temp + transectID, dat_temp_ter))

prop$tmp <- NA
for (i in 1:nrow(prop)){
  if(prop$Freq [i] >= 1){prop$tmp[i] <- 1} else {prop$tmp[i] <- 0}}
transects <- tapply(prop$tmp,prop$Temp,sum)
observations <- xtabs(~Temp, dat_temp_ter) # Number of observations with each temperature

freq <- observations/transects
par(mfrow = c(1,1))
plot(freq)

# There are many 0 temperature!!
check_0 <- as.data.frame(xtabs(~Temp + T_Y, dat_temp))
zero <- check_0[which(check_0$Temp == 0), ]
zero$ones <- NA
for (i in 1:nrow(zero)){
  if (zero$Freq[i] >= 1){zero$ones[i] = 1}
  else {zero$ones[i] = 0}
}
sum(zero$ones)

# ---- Wind ----

# Frequency - Distance with different winds
xtabs(~Wind, dat)
wind <- c(0:6)
# Check with only seen (dat)

# All species
par(mfrow = c(3,3))
for (i in 1:length(wind)){
w <- dat[which(dat$Wind == wind[i]), ]
hist(w$distance, breaks = c(0,25,50,99,200), xlab = "Distance bins (x)", col = "grey", main = paste("All sp - Wind ",wind[i]),
     freq = FALSE)
}

# Target species

for (j in 1:length(target)){
  par(mfrow = c(3,3))
  sp <- dat[which(dat$Species == target[j]),]
  
  for (i in 1:length(wind)){
    w <- sp[which(sp$Wind == wind[i]), ]
    
    if(nrow(w)>0){
    hist(w$distance, breaks = c(0,25,50,99,200), xlab = "Distance bins (x)", col = "grey", main = paste(target[j], "- Wind ",wind[i]),
       freq = FALSE)}}
}

# Other species

for (j in 1:length(other)){
  par(mfrow = c(3,3))
  sp <- dat[which(dat$Species == other[j]),]
  
  for (i in 1:length(wind)){
    w <- sp[which(sp$Wind == wind[i]), ]
    
    if(nrow(w)>0){
      hist(w$distance, breaks = c(0,25,50,99,200), xlab = "Distance bins (x)", col = "grey", main = paste(other[j], "- Wind ",wind[i]),
           freq = FALSE)}}
}

## Summary: Wind dont seem to affect

# ---- Clouds ----

# Frequency - Distance with different clouds
# Check with only seen data, because heard is not supossed to be influenced by this

xtabs(~Clouds, dat)
cloud <- c(0:5)

# All species
par(mfrow = c(2,3))
for (i in 1:length(cloud)){
  w <- dat[which(dat$Clouds == cloud[i]), ]
  hist(w$distance, breaks = c(0,25,50,99,200), xlab = "Distance bins (x)", col = "grey", main = paste("All sp - cloud ",cloud[i]),
       freq = FALSE)
}

# Target species

for (j in 1:length(target)){
  par(mfrow = c(2,3))
  sp <- dat[which(dat$Species == target[j]),]
  
  for (i in 1:length(cloud)){
    w <- sp[which(sp$Clouds == cloud[i]), ]
    
    if(nrow(w)>0){
      hist(w$distance, breaks = c(0,25,50,99,200), xlab = "Distance bins (x)", col = "grey", main = paste(target[j], "- cloud ",cloud[i]),
           freq = FALSE)}}
}

# Other species

for (j in 1:length(other)){
  par(mfrow = c(2,3))
  sp <- dat[which(dat$Species == other[j]),]
  
  for (i in 1:length(cloud)){
    w <- sp[which(sp$Clouds == cloud[i]), ]
    
    if(nrow(w)>0){
      hist(w$distance, breaks = c(0,25,50,99,200), xlab = "Distance bins (x)", col = "grey", main = paste(other[j], "- cloud ",cloud[i]),
           freq = FALSE)}}
}

## Summary: Clouds dont seem to affect


# ---- Observer ----

xtabs(~Observer, dat)
obs1 <- c("Albert Petit", "David Guixé", "Ferran Broto", "Ferran González", "Joan Castelló", "Sergi Sales")


# All species
par(mfrow = c(2,3))
for (i in 1:length(obs1)){
  w <- dat[which(dat$Observer == obs1[i]), ]
  hist(w$distance, breaks = c(0,25,50,99,200), xlab = "Distance bins (x)", col = "grey", main = paste("All sp - obs ",obs1[i]),
       freq = FALSE)}


# Target species

for (j in 1:length(target)){
  par(mfrow = c(2,3))
  sp <- dat[which(dat$Species == target[j]),]
  
  for (i in 1:length(obs1)){
    w <- sp[which(sp$Observer == obs1[i]), ]
    
    if(nrow(w)>0){
      hist(w$distance, breaks = c(0,25,50,99,200), xlab = "Distance bins (x)", col = "grey", main = paste(target[j], "- obs ",obs1[i]),
           freq = FALSE)}}
}

# Other species

for (j in 1:length(other)){
  par(mfrow = c(2,3))
  sp <- dat[which(dat$Species == other[j]),]
  
  for (i in 1:length(obs1)){
    w <- sp[which(sp$Observer == obs1[i]), ]
    
    if(nrow(w)>0){
      hist(w$distance, breaks = c(0,25,50,99,200), xlab = "Distance bins (x)", col = "grey", main = paste(other[j], "- obs ",obs1[i]),
           freq = FALSE)}}
}
