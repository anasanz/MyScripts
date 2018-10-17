
rm(list=ls())

# Load
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

# ---- Temperature ----

#  All Species 

dat_temp <- dat[-which(dat$Temp > 40),]
hist(dat_temp$Temp, main = "All sp - Temperature") # 15ºC looks like the best

# Target species 

dat_temp <- dat[-which(dat$Temp > 40),]
par(mfrow = c(1,3))
for (i in 1:3){
  hist(dat_temp$Temp[which(dat_temp$Species %in% target[i])], main = paste(target[i], "- Temperature")) 
}

# Other species

dat_temp <- dat[-which(dat$Temp > 40),]
par(mfrow = c(2,3))
for (i in 1:length(other)){
  hist(dat_temp$Temp[which(dat_temp$Species %in% other[i])], main = paste(other[i], "- Temperature")) 
}


# ---- Wind ----
# Frequency - Distance with different winds
xtabs(~Wind, dat)
wind <- c(0:6)

# All species
par(mfrow = c(3,3))
for (i in 1:length(wind)){
w <- dat[which(dat$Wind == wind[i]), ]
hist(w$distance, breaks = c(0,25,50,99,200), xlab = "Distance bins (x)", col = "grey", main = paste("All sp - Wind ",wind[i]),
     freq = FALSE)
}

#








