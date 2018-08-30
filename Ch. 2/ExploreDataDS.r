
rm(list=ls())

# ---- Explore data farmdindis with Distance ----

library(Distance)

setwd("~/Second chapter/Data")
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


# ---- Species evolution (Spatio-temporal) ----

library(rgdal)
library(dplyr)
library(tidyr)
library(sp)

# For each year, I need a dataframe with
# 1. Suma species detected per transect (from farm)
# 2. Linked to the spatial layer (centroid)

# Load data
setwd("~/Second chapter/Data")
dat <- read.csv("DataDS_ready.csv")
dat$Species <- as.character(dat$Species) # Data species counts
esp <- read.csv("Tespecies.csv", sep = ";") # Classifications all species grouped by community

setwd("~/GIS Ana/Capes GIS/Carto_general/CAT_30N/Provincies")
cat<- readOGR("~/GIS Ana/Capes GIS/Carto_general/CAT_30N/Provincies", "Provincies") # Load map study area

setwd("~/Second chapter/Data/GIS")
red<- readOGR("~/Second chapter/Data/GIS", "clip_natura2000") # Load rednatura

cen <- readOGR("~/Second chapter/Farmdindis/Maps/transectes", "Centroide_2017") # Contains transects sampled each year (1/0)

cen_10 <- cen[which(cen@data$FETS2010 == 1), ] # One layer for transects sampled each year only
cen_11 <- cen[which(cen@data$FETS2011 == 1), ]
cen_12 <- cen[which(cen@data$FETS2012 == 1), ]
cen_13 <- cen[which(cen@data$FETS2013 == 1), ]
cen_14 <- cen[which(cen@data$FETS2014 == 1), ]
cen_15 <- cen[which(cen@data$FETS2015 == 1), ]
cen_16 <- cen[which(cen@data$FETS2016 == 1), ]
cen_17 <- cen[which(cen@data$FETS2016 == 1), ]

cen <- list(cen_10, cen_11, cen_12, cen_13, cen_14, cen_15, cen_16, cen_17)


# Create variable transectID, than matches with the code of the GIS layers (i.e., two digits: 09)

#1. Add a 0 before the transect number
for (i in 1:nrow(dat)){ 
  dat$Num_transecte[i] <- paste(0,dat$Num_transecte[i], sep = "")
  }

#2. Keep only the last 2 digits 
library(stringr)
for (i in 1:nrow(dat)){ 
  dat$Num_transecte[i] <- str_sub(dat$Num_transecte[i], start = -2)
}

# Create variable by pasting it
for (i in 1:nrow(dat)){ 
  dat$transectID[i] <- paste(dat$Region.Label[i],dat$Num_transecte[i], sep = "")
}


# Select case species to explore data
# Farmland
farm <- as.character(esp$codiEspecie[which(esp$Farmland == 1)]) # Vector selecting farmland species

farm <- dat[which(dat$Species %in% farm), ] # Only farmland species
xtabs(~Species, farm) # See species detected more times. Take MECAL as example


### 2010_MECAL EXAMPLE
farm_10 <- farm[which(farm$Year == 2010), ]
cen_10 <- readOGR("~/Second chapter/Farmdindis/Maps/transectes", "Centroide_2010")
# 1. Suma species detected per transect (from farm)
sum_10 <- aggregate(Count ~ transectID + Species, data = farm_10, FUN = 'sum')
sum_10 <- spread(sum_10, Species, Count) # Wide format
colnames(sum_10)[which(colnames(sum_10) == "transectID")] <- "Codi" # Same name than spatial layer to join
sum_10[is.na(sum_10)] <- 0
# 2. Linked to the spatial layer (centroid)
count_10 <- merge(cen_10, sum_10, by = "Codi", all.x = TRUE) # Join spatial location of transects to counts
count_10@data[is.na(count_10@data)] <- 0.1 # Set na's and 0 to 0.1 to change it to log scale and plot it in different sizes
count_10@data[count_10@data == 0] <- 0.1
count_10@data$MECAL_log <- log(count_10@data$MECAL) #Log scale to plot it in different sizes
plot(cat, axes = TRUE, xlim = c(min(count_10@coords[,1]), max(count_10@coords[,1])), ylim = c(min(count_10@coords[,2]), max(count_10@coords[,2])) )
points(count_10, pch=21, bg = adjustcolor("lightgrey",alpha.f = 0.1), lwd = 0.4, cex = count_10@data$MECAL_log)
###


# All species all years ABUNDANCE

library(animation)

setwd("~/Second chapter/Data/Explore_species")

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
    count_year@data[is.na(count_year@data)] <- 0.1 # Set na's to 0.1 to change it to log scale and plot it in different sizes
    
    count_year@data$log_sp <- log(count_year@data$sp) #Log scale to plot it in different sizes
    
    plot(cat, axes = TRUE, main = paste("",sp[j], Year[i],""),
         xlim = c(min(count_year@coords[,1]), max(count_year@coords[,1])), ylim = c(min(count_year@coords[,2]), max(count_year@coords[,2])) )
    points(count_year, pch=21, bg = adjustcolor("lightgrey",alpha.f = 0.7), lwd = 0.4, cex = count_year@data$log_sp)
    }, 
    movie.name = paste("",sp[j],".gif")
    ) 
}

# All species all years PRESENCE + ABUNDANCE

library(devtools)
library(animation)

setwd("~/Second chapter/Data/Explore_species_occurrence")

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