
rm(list=ls())

library(rtrim)
library(dplyr)

setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data")
d <- read.csv("DataDS_ready_ALL.csv")
colnames(d)[which(colnames(d) == "Count")] <- "Cluster" 

# To take into account transects with abundance 0
# 1. Select all transects IDs from all species observations
# 2. Join the observations of MECAL (for example) with all transects so that they remain with NA if the
# species was there but it wasnt sampled

d_tr <- d[ ,which(colnames(d) %in% c("Species",  "T_Y", "Observer"))]
d_tr_all <- data.frame(T_Y = unique(d_tr$T_Y), id = NA)

d_tr$Observer <- as.character(d_tr$Observer) 
d_tr_all_obs <- left_join(d_tr_all, d_tr)
d_tr_all_obs <- d_tr_all_obs[ ,c(1,4)]
d_tr_all_obs <- d_tr_all_obs[which(!duplicated(d_tr_all_obs)), ] # Table with all sampled fields and which observer sampled it


sp <- d[which(d$Species == "BUOED"), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "Observer", "Cluster"))] # Select species spAL and all years
sp <- arrange(sp, Year, transectID) #Ordered
sp_detec_transectID <- unique(sp$transectID)
sp$Observer <- as.character(sp$Observer) 

absent <- anti_join(d_tr_all,sp) # Transects with 0 abundance, add to sp.
colnames(absent)[2] <- "Banda" # Format it to add the rows to sp
absent$T_Y <- as.character(absent$T_Y)
absent$Species <- "BUOED"
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

# Subset for trim analysis
sp <- all_sp[, which(colnames(all_sp) %in% c("Year", "transectID", "count"))] # Select species MECAL and all years
colnames(sp)[which(colnames(sp) %in% "transectID")] <- "site"
colnames(sp)[which(colnames(sp) %in% "Year")] <- "year"
sp$year <- as.integer(sp$year)

g <- aggregate(count ~ year, FUN = sum, data = sp)
sp <- aggregate(count ~ year + site, FUN = sum, data = sp)

check_observations(sp, model = 2)

# ---- MODEL 2 ----

m2 <- trim(count ~ site + year, data = sp, model = 2)
coefficients(m2)
i2<-index(m2, which="both")

# summarize the model
summary(m2)

wald(m2)

#Retrieve goodness-of-fit
gof(m2)

#Extract the coefficients
coefficients(m2)

#Plot with overall slope
par(mfrow = c(1,2))
plot(overall(m2))
mtext("TRIM", side = 3, line = 1, cex = 1.5)
plot(i2)



# ----  MODEL 3 ----

m3 <- trim(count ~ site + year, data = sp, model = 3)

summary(m3)

i1 <- index(m3, which="both")

totals(m3)

wald(m3)

coefficients(m3)

plot(overall(m3))

plot(i1)
