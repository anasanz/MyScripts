
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

setwd("~/Second chapter/Data")
dat <- read.csv("DataDS_ready.csv")
dat$Species <- as.character(dat$Species)
esp <- read.csv("Tespecies.csv", sep = ";") # All species grouped by community

# Create variable transectID
for (i in 1:nrow(dat)){
  dat$transectID[i] <- paste(dat$Region.Label[i],dat$Num_transecte[i], sep = "")
}

# Select case species to explore data

# Farmland

farm <- as.character(esp$codiEspecie[which(esp$Farmland == 1)]) # Vector selecting farmland species

farm <- dat[which(dat$Species %in% farm), ] # Only farmland species
xtabs(~Species, farm) # See species detected more times. Take MECAL as example

# For each year, I need a dataframe with
# 1. Suma species detected per transect (from farm)
# 2. Linked to the spatial layer (centroid)


### 2010
farm_10 <- farm[which(farm$Year == 2010), ]
cen_10 <- readOGR("~/Second chapter/Farmdindis/Maps/transectes", "Centroide_2010")

# 1. Suma species detected per transect (from farm)
sum_10 <- aggregate(Count ~ transectID + Species, data = farm_10, FUN = 'sum')
sum_10 <- spread(sum_10, Species, Count)
colnames(sum_10)[which(colnames(sum_10) == "transectID")] <- "Codi"

# 2. Linked to the spatial layer (centroid)
cen_10@data$Codi


