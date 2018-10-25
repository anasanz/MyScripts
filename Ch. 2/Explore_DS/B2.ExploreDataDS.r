
rm(list=ls())

# ---- Explore data farmdindis with Distance ----

library(Distance)

setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data")
dat <- read.csv("DataDS_ready.csv")
esp <- read.csv("Tespecies.csv", sep = ";") # All species grouped by community

# Select case species to explore data

# Farmland

farm <- as.character(esp$codiEspecie[which(esp$Farmland == 1)]) # Vector selecting farmland species

farm <- dat[which(dat$Species %in% farm), ] # Only farmland species
xtabs(~Species, farm) # See species detected more times. Take MECAL as example

mec <- farm[which(dat$Species == "MECAL"), ]
mec <- mec[which(mec$Obs_type == "V"), ]
col <- c("blue", "orange", "green", "grey")
hist(mec$distance, main = "Calandra lark", breaks = c(0,25,50,99,200), col = col)


ter <- farm[which(dat$Species == "TERAX"), ]
ter <- ter[which(ter$Obs_type == "V"), ]
hist(ter$distance, main = "Little bustard", breaks = c(0,25,50,99,200), col = col)
plot(ter$Count ~ ter$distance)

mic <- farm[which(dat$Species == "MICAL"), ]
mic <- mic[which(mic$Obs_type == "V"), ]
hist(mic$distance)

pipic <- farm[which(dat$Species == "PIPIC"), ]
pipic <- pipic[which(pipic$Obs_type == "V"), ]
hist(pipic$distance)

padom <- farm[which(dat$Species == "PADOM"), ]
padom <- padom[which(padom$Obs_type == "V"), ]
hist(padom$distance)

gacri <- farm[which(dat$Species == "GACRI"), ]
gacri <- gacri[which(gacri$Obs_type == "V"), ]
hist(gacri$distance)

buoed <- farm[which(dat$Species == "BUOED"), ]
buoed <- buoed[which(buoed$Obs_type == "V"), ]
hist(buoed$distance, main = "Stone curlew", breaks = c(0,25,50,99,200), col = col)

cogar <- farm[which(dat$Species == "COGAR"), ]
cogar <- cogar[which(cogar$Obs_type == "V"), ]
hist(cogar$distance)

par(mfrow = c(1,3))
hist(mec$distance, main = "Calandra lark", breaks = c(0,25,50,99,200), col = col)
hist(ter$distance, main = "Little bustard", breaks = c(0,25,50,99,200), col = col)
hist(buoed$distance, main = "Stone curlew", breaks = c(0,25,50,99,200), col = col)


# Analysis with mec

mec_hn <- ds(mec)
mec_hr <- ds(mec, key = "hr")

mec_hn_count <- ds(mec, formula = ~Count)
mec_hr_count <- ds(mec, key = "hr", formula = ~Count)

summarize_ds_models(mec_hn, mec_hn_count, mec_hr, mec_hr_count)

plot(mec_hr)
plot(mec_hn)

summary(mec_hn)
gof_ds(mec_hn, plot = TRUE)
gof_ds(mec_hr, plot = TRUE)


# Forest species

forest <- as.character(esp$codiEspecie[which(esp$Forest == 1)]) # Vector selecting farmland species

forest <- dat[which(dat$Species %in% forest), ] # Only farmland species
xtabs(~Species, forest) # See species detected more times. Take MECAL as example

piv <- forest[which(forest$Species == "PIVIR"), ]
piv <- piv[which(piv$Obs_type == "V"), ]
hist(piv$distance)


syc <- forest[which(forest$Species == "SYCAN"), ]
syc <- piv[which(piv$Obs_type == "V"), ]
hist(piv$distance)

# ---- Transects duplicated by year ----
# Not really necessary. To check which years the transects where sampled, check GIS layers 2017

# Create variable transectID
for (i in 1:nrow(dat)){
  dat$transectID[i] <- paste(dat$Region.Label[i],dat$Num_transecte[i], sep = "")
}

# Data frame containing when each transect was sampled
unique(dat$transectID)
m <- matrix(nrow = length(unique(dat$transectID)), ncol = length(unique(dat$Year)))
rownames(m) <- unique(dat$transectID)
colnames(m) <- Year
Year <- c(2009,2010,2011,2012,2013,2014,2015,2016,2017) # Create empty matrix

for (i in 1:length(Year)){
  
  tmp <- dat[which(dat$Year == Year[i]), ] # Choose the data of a particular Year
  transects <- as.vector(unique(tmp$transectID)) # Only the transects sampled in that year
  
  for (j in 1:length(transects)){
    m[which(rownames(m) == transects[j]), which(colnames(m) == Year[i])] <- 1 # 1 where the transect was sampled
  }
}

m[which(is.na(m))] <- 0


#####
#####
# ---- SPECIES EVOLUTION (SPATIO-TEMPORAL) ----
# ----- Load data -----
library(rgdal)
library(dplyr)
library(tidyr)
library(sp)
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

cen <- list(cen_10, cen_11, cen_12, cen_13, cen_14, cen_15, cen_16, cen_17)

# For each year, I need a dataframe with
# 1. Suma species detected per transect (from farm)
# 2. Linked to the spatial layer (centroid)

#  ---- PLOTS COUNTS-PRESENCE
#####
#  ---- 1. PLOT OF ABUNDANCE/OCCUPANCY
#  ----- All species ---- 
library(tidyr)
xtabs(~Species + Year, dat)
freq <- as.data.frame(xtabs(~Species + Year, dat))
freq <- spread(freq,Year, Freq)
freq$remove <- 0 # To export data set and select the ones that are present in enough transects

write.csv(freq, "freq_species.csv")
# All species all years PRESENCE + ABUNDANCE

library(devtools)
library(animation)

setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/Explore_species_occurrence/All")

Year <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
sp <- unique(dat$Species)


for (j in 1:length(sp)){
  
  dat_sp <- dat[which(dat$Species == sp[j]), ] # Select species
  
  saveGIF(
    
    for (i in 1:length(Year)) {
      
      dat_year <- dat_sp[which(dat_sp$Year == Year[i]), ] # Determine year
      
      # 1. Suma individuals detected per transect (from dat_sp)
      sum_year <- aggregate(Count ~ transectID + Species, data = dat_year, FUN = 'sum') # Number of individuals of the species in the year
      sum_year <- spread(sum_year, Species, Count) # Wide format
      colnames(sum_year)[which(colnames(sum_year) == "transectID")] <- "Codi" # Same name than spatial layer to join
      #sum_year[is.na(sum_year)] <- 0
      colnames(sum_year)[2] <- "sp" # Make it generic for all species
      
      # 2. Linked to the spatial layer (centroid)
      count_year <- merge(cen[[i]], sum_year, by = "Codi", all.x = TRUE) # Join spatial location of transects to counts
      
      count_year@data$sp[is.na(count_year@data$sp)] <- 0.1 # FOR ABUNDANCE: Set na's to 0.1 to change it to log scale and plot it in different sizes
      count_year@data$log_sp <- log(count_year@data$sp) #FOR ABUNDANCE:Log scale to plot it in different sizes
      presence_year <- count_year[which(count_year@data$sp > 0.1), ] #FOR PRESENCE: Only fields where is present
      
      par(mfrow = c(1,2))
      
      plot(cat, # Use points "presence_year"
           xlim = c(min(red@bbox[1,1]), max(red@bbox[1,2])), ylim = c(min(red@bbox[2,1]), max(red@bbox[2,2])) )
      
      plot(red, col = adjustcolor("lightgrey",alpha.f = 0.3), border = adjustcolor("lightgrey",alpha.f = 0.5), add = TRUE)
      points(presence_year, pch=21, bg = "red", lwd = 0.4, cex = 0.8)
      mtext("Presence", side = 3, line = -2, cex = 1.5, adj = 0.5)
      
      
      plot(cat, # Use points "Count_year"
           xlim = c(min(red@bbox[1,1]), max(red@bbox[1,2])), ylim = c(min(red@bbox[2,1]), max(red@bbox[2,2])) )
      plot(red, col = adjustcolor("lightgrey",alpha.f = 0.3), border = adjustcolor("lightgrey",alpha.f = 0.5), add = TRUE)
      points(count_year, pch=21, bg = adjustcolor("red",alpha.f = 0.5), lwd = 0.4, cex = count_year@data$log_sp)
      mtext("Counts", side = 3, line = -2, cex = 1.5, adj = 0.5)
      
      
      mtext(paste("", sp[j], Year[i], ""), outer = TRUE, side = 1, line = -5, cex = 2, adj = 0.5)
    }, 
    movie.name = paste("",sp[j],".gif"),
    ani.width = 900, ani.heigth = 500,
    interval = 1.5
  ) 
}
#  ----- Only farmland ---- 
farm <- as.character(esp$codiEspecie[which(esp$Farmland == 1)]) # Vector selecting farmland species

farm <- dat[which(dat$Species %in% farm), ] # Only farmland species
xtabs(~Species + Year, farm) # See species detected more times. Take MECAL as example

# All species all years PRESENCE + ABUNDANCE

library(devtools)
library(animation)

setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/Explore_species_occurrence/Farmland2")

Year <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
sp <- unique(farm$Species)


for (j in 1:length(sp)){
  
  farm_sp <- farm[which(farm$Species == sp[j]), ] # Select species
  
  saveGIF(
    
    for (i in 1:length(Year)) {
      
      farm_year <- farm_sp[which(farm_sp$Year == Year[i]), ] # Determine year
      
      # 1. Suma individuals detected per transect (from farm_sp)
      sum_year <- aggregate(Count ~ transectID + Species, data = farm_year, FUN = 'sum') # Number of individuals of the species in the year
      sum_year <- spread(sum_year, Species, Count) # Wide format
      colnames(sum_year)[which(colnames(sum_year) == "transectID")] <- "Codi" # Same name than spatial layer to join
      #sum_year[is.na(sum_year)] <- 0
      colnames(sum_year)[2] <- "sp" # Make it generic for all species
      
      # 2. Linked to the spatial layer (centroid)
      count_year <- merge(cen[[i]], sum_year, by = "Codi", all.x = TRUE) # Join spatial location of transects to counts
      
      count_year@data$sp[is.na(count_year@data$sp)] <- 0.1 # FOR ABUNDANCE: Set na's to 0.1 to change it to log scale and plot it in different sizes
      count_year@data$log_sp <- log(count_year@data$sp) #FOR ABUNDANCE:Log scale to plot it in different sizes
      presence_year <- count_year[which(count_year@data$sp > 0.1), ] #FOR PRESENCE: Only fields where is present
      
      par(mfrow = c(1,2))
      
      plot(cat, # Use points "presence_year"
           xlim = c(min(red@bbox[1,1]), max(red@bbox[1,2])), ylim = c(min(red@bbox[2,1]), max(red@bbox[2,2])) )
      
      plot(red, col = adjustcolor("lightgrey",alpha.f = 0.3), border = adjustcolor("lightgrey",alpha.f = 0.5), add = TRUE)
      points(presence_year, pch=21, bg = "red", lwd = 0.4, cex = 0.8)
      mtext("Presence", side = 3, line = -2, cex = 1.5, adj = 0.5)
      
      
      plot(cat, # Use points "Count_year"
           xlim = c(min(red@bbox[1,1]), max(red@bbox[1,2])), ylim = c(min(red@bbox[2,1]), max(red@bbox[2,2])) )
      plot(red, col = adjustcolor("lightgrey",alpha.f = 0.3), border = adjustcolor("lightgrey",alpha.f = 0.5), add = TRUE)
      points(count_year, pch=21, bg = adjustcolor("red",alpha.f = 0.5), lwd = 0.4, cex = count_year@data$log_sp)
      mtext("Counts", side = 3, line = -2, cex = 1.5, adj = 0.5)
      
      
      mtext(paste("", sp[j], Year[i], ""), outer = TRUE, side = 1, line = -5, cex = 2, adj = 0.5)
    }, 
    movie.name = paste("",sp[j],".gif"),
    ani.width = 900, ani.heigth = 500,
    interval = 1.5
  ) 
}

# There are some (very few) that appear in the data set (sampled) but not in the layer with all the transects
# This ones dont appear in the plot (eg. transect AL04,AL21 MECAL 2010 only in data but not in map)
# To check this, use the code:
prov <- count_year@data
prov_presMECAL <- prov[which(complete.cases(prov$sp)), ] # Only 53! It should be 85
c <- left_join(sum_year,prov_presMECAL)

#  ----- Only steppe ---- 
step <- as.character(esp$codiEspecie[which(esp$Steppe == 1)]) 

step <- dat[which(dat$Species %in% step), ] 
xtabs(~Species, step) 

# All species all years PRESENCE + ABUNDANCE

library(devtools)
library(animation)

setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data/Explore_species_occurrence/Steppe")

Year <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
sp <- unique(step$Species)


for (j in 1:length(sp)){
  
  step_sp <- step[which(step$Species == sp[j]), ] # Select species
  
  saveGIF(
    
    for (i in 1:length(Year)) {
      
      step_year <- step_sp[which(step_sp$Year == Year[i]), ] # Determine year
      
      # 1. Suma individuals detected per transect (from step_sp)
      sum_year <- aggregate(Count ~ transectID + Species, data = step_year, FUN = 'sum') # Number of individuals of the species in the year
      sum_year <- spread(sum_year, Species, Count) # Wide format
      colnames(sum_year)[which(colnames(sum_year) == "transectID")] <- "Codi" # Same name than spatial layer to join
      #sum_year[is.na(sum_year)] <- 0
      colnames(sum_year)[2] <- "sp" # Make it generic for all species
      
      # 2. Linked to the spatial layer (centroid)
      count_year <- merge(cen[[i]], sum_year, by = "Codi", all.x = TRUE) # Join spatial location of transects to counts
      
      count_year@data$sp[is.na(count_year@data$sp)] <- 0.1 # FOR ABUNDANCE: Set na's to 0.1 to change it to log scale and plot it in different sizes
      count_year@data$log_sp <- log(count_year@data$sp) #FOR ABUNDANCE:Log scale to plot it in different sizes
      presence_year <- count_year[which(count_year@data$sp > 0.1), ] #FOR PRESENCE: Only fields where is present
      
      par(mfrow = c(1,2))
      
      plot(cat, # Use points "presence_year"
           xlim = c(min(red@bbox[1,1]), max(red@bbox[1,2])), ylim = c(min(red@bbox[2,1]), max(red@bbox[2,2])) )
      
      plot(red, col = adjustcolor("lightgrey",alpha.f = 0.3), border = adjustcolor("lightgrey",alpha.f = 0.5), add = TRUE)
      points(presence_year, pch=21, bg = "red", lwd = 0.4, cex = 0.8)
      mtext("Presence", side = 3, line = -2, cex = 1.5, adj = 0.5)
      
      
      plot(cat, # Use points "Count_year"
           xlim = c(min(red@bbox[1,1]), max(red@bbox[1,2])), ylim = c(min(red@bbox[2,1]), max(red@bbox[2,2])) )
      plot(red, col = adjustcolor("lightgrey",alpha.f = 0.3), border = adjustcolor("lightgrey",alpha.f = 0.5), add = TRUE)
      points(count_year, pch=21, bg = adjustcolor("red",alpha.f = 0.5), lwd = 0.4, cex = count_year@data$log_sp)
      mtext("Counts", side = 3, line = -2, cex = 1.5, adj = 0.5)
      
      
      mtext(paste("", sp[j], Year[i], ""), outer = TRUE, side = 1, line = -5, cex = 2, adj = 0.5)
    }, 
    movie.name = paste("",sp[j],".gif"),
    ani.width = 900, ani.heigth = 500,
    interval = 1.5
  ) 
}

#####
# ---- 2. TABLE OF TRANSECTS OCCUPIED PER SPECIES
# ----- Number of detections per year ----

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



# ----- Number/proportion of occupied transects per year ----

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

# Round proportion dataset

occupancy_prop <- round(occupancy_prop, 2) 

# Rownames as variable 
occupancy_numb$Species <- rownames(occupancy_numb)
occupancy_numb <- occupancy_numb[ ,c(9,1:8)]

occupancy_prop$Species <- rownames(occupancy_prop)
occupancy_prop <- occupancy_prop[ ,c(9,1:8)]

#Rownumbers
row.names(occupancy_numb) <- seq(1,73,1)
row.names(occupancy_prop) <- seq(1,73,1)

# Get total per species
sum(occupancy_prop[72,c(2:9)])
for (i in 1:nrow(occupancy_prop)){
  occupancy_numb$total[i] <- sum(occupancy_numb[i,c(2:9)])
  occupancy_prop$total[i] <- sum(occupancy_prop[i,c(2:9)])
}

# Sort from more to less common species (based in total)
occupancy_numb <- arrange(occupancy_numb,desc(total))
occupancy_prop <- arrange(occupancy_prop,desc(total))


write.csv(occupancy_numb, "NumberTrans_sp_year.csv")
write.csv(occupancy_prop, "PropTrans_sp_year.csv")

#####


