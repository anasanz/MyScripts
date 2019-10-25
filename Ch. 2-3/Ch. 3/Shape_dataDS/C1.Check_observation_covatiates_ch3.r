
#### Check observation co-variates ch. 3 ####
# Check again because now Heard and Seen are together

setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/Data")
dat <- read.csv("DataDS_ch3_20_18_final.csv")

target <- c("MECAL", "TERAX", "BUOED")
other <- c("GACRI", "MICAL", "PIPIC", "COPAL", "HIRUS", "PADOM")
interesting <- c("MEAPI", "ALRUF", "UPEPO", "COGAR", "CABRA", "PTALC")

# ---- Observer ----
# Accordying to the last paper,
# We know it has an effect and we will include it for sure as a random effect per species
# ---- Year -----
# Accordying to the last paper,
# We know it has an effect and we will include it for sure as a random effect per species

# ---- Temperature ----
hist(dat$Temp)
unique(dat$Temp)
dat[which(dat$Temp == 2), ] # 14 Mayo en BE 6:40?
dat[which(dat$Temp == 3), ] # 25 Mayo SI 7:07?
dat[which(dat$Temp == 4), ] # 14 Mayo BE 7:25?
dat[which(dat$Temp == 5), ] # 14 y 15 Mayo Al y Be 6 - 8 am?

# Corrected by the number of transects:
# First, see how many transects were sampled at that temperature
prop <- as.data.frame(xtabs(~Temp + transectID, dat))
prop$tmp <- NA
for (i in 1:nrow(prop)){
  if(prop$Freq [i] >= 1){prop$tmp[i] <- 1} else {prop$tmp[i] <- 0}}
transects <- tapply(prop$tmp,prop$Temp,sum)

# Second, divide observations/number of transects
observations <- xtabs(~Temp, dat) # Number of observations with each temperature

freq <- observations/transects
par(mfrow = c(1,1))
plot(freq) # It seems like it is influenced by 23ºC
hist(freq, main = "All sp - Temperature") # 20-25ºC looks like the best

# B. Target species 

par(mfrow = c(1,3))
for (i in 1:3){
  hist(dat$Temp[which(dat$Species %in% target[i])], main = paste(target[i], "- Temperature")) 
}

# Corrected by the number of transects for BUOED
dat_temp_buo <- dat[which(dat$Species == "BUOED"), ]
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
dat_temp_ter <- dat[which(dat$Species == "TERAX"), ]
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
dat_temp_ter <- dat[which(dat$Species == "ALRUF"), ]
prop <- as.data.frame(xtabs(~Temp + transectID, dat_temp_ter))

prop$tmp <- NA
for (i in 1:nrow(prop)){
  if(prop$Freq [i] >= 1){prop$tmp[i] <- 1} else {prop$tmp[i] <- 0}}
transects <- tapply(prop$tmp,prop$Temp,sum)
observations <- xtabs(~Temp, dat_temp_ter) # Number of observations with each temperature

freq <- observations/transects
par(mfrow = c(1,1))
plot(freq, main = "alruf")


# ---- Wind ---- 
# Frequency - Distance with different winds
xtabs(~Wind, dat)
wind <- c(0:6)
# Check with only seen (dat)

# All species
par(mfrow = c(3,3))
for (i in 1:length(wind)){
  w <- dat[which(dat$Wind == wind[i]), ]
  hist(w$distance, breaks = c(0,25,50,99,200,500), xlab = "Distance bins (x)", col = "grey", main = paste("All sp - Wind ",wind[i]),
       freq = FALSE)
}

# Target species

for (j in 1:length(target)){
  par(mfrow = c(3,3))
  sp <- dat[which(dat$Species == target[j]),]
  
  for (i in 1:length(wind)){
    w <- sp[which(sp$Wind == wind[i]), ]
    
    if(nrow(w)>0){
      hist(w$distance, breaks = c(0,25,50,99,200, 500), xlab = "Distance bins (x)", col = "grey", main = paste(target[j], "- Wind ",wind[i]),
           freq = FALSE)}}
}

# Other species

for (j in 1:length(other)){
  par(mfrow = c(3,3))
  sp <- dat[which(dat$Species == other[j]),]
  
  for (i in 1:length(wind)){
    w <- sp[which(sp$Wind == wind[i]), ]
    
    if(nrow(w)>0){
      hist(w$distance, breaks = c(0,25,50,99,200, 500), xlab = "Distance bins (x)", col = "grey", main = paste(other[j], "- Wind ",wind[i]),
           freq = FALSE)}}
}
# ---- Zone ----