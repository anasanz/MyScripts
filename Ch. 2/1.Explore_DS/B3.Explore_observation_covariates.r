
rm(list=ls())

library(rgdal)
library(dplyr)
library(tidyr)
library(sp)

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data")
dat <- read.csv("DataDS_ready.csv")


# ---- All Species ----

# All sp - Temperature
dat_temp <- dat[-which(dat$Temp > 40),]
hist(dat_temp$Temp, main = "All sp - Temperature") # 15ºC looks like the best

# All sp - Distance with different winds

xtabs(~Wind, dat)

par(mfrow = c(3,3),
    oma = c(3,3,4,1) + 0.1,
    mar = c(2,2,2,0) + 0.1)

w0 <- dat[which(dat$Wind == 0), ]
hist(w0$distance, breaks = c(0,25,50,99,200), xlab = "Distance bins (x)", col = "grey", main = "All sp - Wind 0",
     freq = FALSE)

w1 <- dat[which(dat$Wind == 1), ]
hist(w1$distance, breaks = c(0,25,50,99,200), xlab = "Distance bins (x)", col = "grey", main = "All sp - Wind 1",
     freq = FALSE) 

w2 <- dat[which(dat$Wind == 2), ]
hist(w2$distance, breaks = c(0,25,50,99,200), xlab = "Distance bins (x)", col = "grey", main = "All sp - Wind 2",
     freq = FALSE)

w3 <- dat[which(dat$Wind == 3), ]
hist(w3$distance, breaks = c(0,25,50,99,200), xlab = "Distance bins (x)", col = "grey", main = "All sp - Wind 3",
     freq = FALSE)

w4 <- dat[which(dat$Wind == 4), ]
hist(w4$distance, breaks = c(0,25,50,99,200), xlab = "Distance bins (x)", col = "grey", main = "All sp - Wind 4",
     freq = FALSE)

w5 <- dat[which(dat$Wind == 5), ]
hist(w5$distance, breaks = c(0,25,50,99,200), xlab = "Distance bins (x)", col = "grey", main = "All sp - Wind 5",
     freq = FALSE)

w6 <- dat[which(dat$Wind == 5), ]
hist(w5$distance, breaks = c(0,25,50,99,200), xlab = "Distance bins (x)", col = "grey", main = "All sp - Wind 6",
     freq = FALSE)

# The number of detections slightly decreases for all distances except the 1sr bin

# ---- Most important species ----
# 1. Identify:
#  - Target species abundance and occurence through years
#  - Rest of the species abundance and occurrence through years

# ---- NUMBER OF DETECTIONS PER YEAR ----

aggregate( ~ transectID + Species + Year , data = dat, FUN = 'sum') # Number of individuals of the species in the year
sum_year <- spread(sum_year, Species, Count)

library(tidyr)
xtabs(~Species + Year, dat)
aggregate(Count ~ transectID + Year  , data = dat, FUN = 'sum') # Number of individuals of the species in the year

freq <- as.data.frame(xtabs(~Species + Year, dat))
freq <- spread(freq,Year, Freq)
freq$remove <- 0 # To export data set and select the ones that are present in enough transects

write.csv(freq, "NumberDetect_sp_year.csv")

# Check separatedly farmland and steppe

esp <- read.csv("Tespecies.csv", sep = ";") # All species grouped by community
# Farmland occurrence 
farm <- as.character(esp$codiEspecie[which(esp$Farmland == 1)]) # Vector selecting farmland species
farm <- dat[which(dat$Species %in% farm), ] # Only farmland species
farm$Species <- as.character(farm$Species)
xtabs(~Species, farm) # See species detected more times. Take MECAL as example

#Steppe birds occurrence
step <- as.character(esp$codiEspecie[which(esp$Steppe == 1)]) 
step <- dat[which(dat$Species %in% step), ]
step$Species <- as.character(step$Species)
xtabs(~Species, step) 



# ---- OCCUPIED TRANSECTS PER YEAR ----

# Load data
setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data")
dat <- read.csv("DataDS_ready.csv")
dat$Species <- as.character(dat$Species) # Data species counts
esp <- read.csv("Tespecies.csv", sep = ";") # Classifications all species grouped by community

setwd("C:/Users/Ana/Documents/PhD/GIS Ana/Capes GIS/Carto_general/CAT_30N/Provincies")
cat<- readOGR("C:/Users/Ana/Documents/PhD/GIS Ana/Capes GIS/Carto_general/CAT_30N/Provincies", "Provincies") # Load map study area

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS")
red<- readOGR("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS", "clip_natura2000") # Load rednatura

cen <- readOGR("C:/Users/Ana/Documents/PhD/Second chapter/Farmdindis/Maps/transectes", "Centroide_2017") # Contains transects sampled each year (1/0)

cen_10 <- cen[which(cen@data$FETS2010 == 1), ] # One layer for transects sampled each year only
cen_11 <- cen[which(cen@data$FETS2011 == 1), ]
cen_12 <- cen[which(cen@data$FETS2012 == 1), ]
cen_13 <- cen[which(cen@data$FETS2013 == 1), ]
cen_14 <- cen[which(cen@data$FETS2014 == 1), ]
cen_15 <- cen[which(cen@data$FETS2015 == 1), ]
cen_16 <- cen[which(cen@data$FETS2016 == 1), ]
cen_17 <- cen[which(cen@data$FETS2016 == 1), ]

# Calculate occupancy

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data/Explore_species_occurrence/All")

Year <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
cen <- list(cen_10, cen_11, cen_12, cen_13, cen_14, cen_15, cen_16, cen_17)
sp <- unique(dat$Species)

# Fill 2 datasets:
# 1. Number of transects occupied by the species per year
occupancy_numb <- data.frame(matrix(NA, ncol = length(Year), nrow = length(sp)))
colnames(occupancy_numb) <- Year
rownames(occupancy_numb) <- sp
# 2. Proportion of transects occupied by the species per year
occupancy_prop <- data.frame(matrix(NA, ncol = length(Year), nrow = length(sp)))
colnames(occupancy_prop) <- Year
rownames(occupancy_prop) <- sp

for (j in 1:length(sp)){
  
  dat_sp <- dat[which(dat$Species == sp[j]), ] # Select species
  
    for (i in 1:length(Year)) {
      
      dat_year <- dat_sp[which(dat_sp$Year == Year[i]), ] # Determine year
      
      if (nrow(dat_year) > 0) { # Only the years when was detected
      
      # 1. Suma individuals detected per transect (from dat_sp)
      sum_year <- aggregate(Count ~ transectID + Species, data = dat_year, FUN = 'sum') # Number of individuals of the species in the year
      sum_year <- spread(sum_year, Species, Count) # Wide format
      colnames(sum_year)[which(colnames(sum_year) == "transectID")] <- "Codi" # Same name than spatial layer to join
      #sum_year[is.na(sum_year)] <- 0
      colnames(sum_year)[2] <- "sp" # Make it generic for all species
      
      # 2. Linked to the spatial layer (centroid) to know the overall sampled transects each year
      count_year <- merge(cen[[i]], sum_year, by = "Codi", all.x = TRUE) # Join spatial location of transects to counts
      count_year@data$sp[is.na(count_year@data$sp)] <- 0 
      count_year@data$sp[which(count_year@data$sp > 0)] <- 1
        # Fill number of occupied transects
      number_oc_transects <- sum(count_year@data$sp)
      occupancy_numb[which(rownames(occupancy_numb) == sp[j]), which(colnames(occupancy_numb) == Year[i])] <- number_oc_transects
        # Fill proportion of occupied transects
      prop_oc_transects <- (number_oc_transects/length(count_year@data$sp))*100
      occupancy_prop[which(rownames(occupancy_prop) == sp[j]), which(colnames(occupancy_prop) == Year[i])] <- prop_oc_transects
      } else { # When it was not detected, put 0
        occupancy_numb[which(rownames(occupancy_numb) == sp[j]), which(colnames(occupancy_numb) == Year[i])] <- 0
        occupancy_prop[which(rownames(occupancy_prop) == sp[j]), which(colnames(occupancy_prop) == Year[i])] <- 0
      }}
}

#write.csv(occupancy_numb, "NumberTrans_sp_year.csv")
#write.csv(occupancy_prop, "PropTrans_sp_year.csv")

