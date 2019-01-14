
rm(list=ls())

library(rjags)
library(jagsUI)
library(rgdal)
library(dplyr)

# Run model 4.1 in MECAL dataset (1 year)
# ---- I ignore counts in each observation (cluster size)

setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data")
d <- read.csv("DataDS_ready.csv")

# To take into account transects with abundance 0
# 1. Select all transects IDs from all species observations
# 2. Join the observations of MECAL (for example) with all transects so that they remain with NA if the
# species was there but it wasnt sampled

d_tr <- d[ ,which(colnames(d) %in% c("Species",  "T_Y"))]
d_tr_all <- data.frame(T_Y = unique(d_tr$T_Y), id = NA)


mec <- d[which(d$Species == "MECAL"), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species"))] # Select species MECAL and all years
mec <- arrange(mec, Year, transectID) #Ordered
mec_detec_transectID <- unique(mec$transectID)


absent <- anti_join(d_tr_all,mec) # Transects with 0 abundance, add to mec.
colnames(absent)[2] <- "Banda" # Format it to add the rows to mec
absent$T_Y <- as.character(absent$T_Y)
absent$Species <- "MECAL"


for (i in 1:nrow(absent)){
  absent$Year[i] <- substr(absent$T_Y[i], 6,9)
  absent$transectID[i] <- substr(absent$T_Y[i], 1,4)
}

all_mec <- rbind(mec,absent) # Include transects with abundance 0
all_mec <- arrange(all_mec, Year, transectID) # Ordered


# ---- Specify data for the model ----

# Information: bins, years, sites
strip.width <- 200 				
dist.breaks <- c(0,25,50,100,200)
int.w <- diff(dist.breaks) # width of distance categories (v)
midpt <- diff(dist.breaks)/2+dist.breaks[-5]
nG <- length(dist.breaks)-1

year <- list("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")
nyrs <- length(year)

# This is nothing
#nind.df <- aggregate(Species ~ Year, FUN = length, data = all_mec)
#nrow(all_mec[which(all_mec$Year == "2010"), ])
#ninds_year <- nind.df[ ,2]
#max.sites <- max(nSites)


# ---- Distance observations ----

nind <- nrow(mec)

# Format

all.sites <- unique(all_mec$transectID)
all.sites <- sort(all.sites,descreasing = TRUE)

m <- matrix(NA, nrow = length(all.sites), ncol = nyrs)
rownames(m) <- all.sites
colnames(m) <- year

# Add counts > 0

count <- aggregate(Species ~ Year + transectID, FUN = length, data = mec)

for (i in 1:nrow(count)){
  m[which(rownames(m) %in% count$transectID[i]), which(colnames(m) %in% count$Year[i])] <- count$Species[i]
}









y <- as.data.frame(array(0, c(nSites, length(dist.breaks)-1)))
rownames(y) <- tr2017

ID <- unique(m$transectID) # order transects = dclass
bin.breaks <- c(0,1,2,3,4)

for (i in 1:length(ID)){
  transect <- m[which(m$transectID == ID[i]), ] # Select all observations from one transect
  counts <- table(cut(transect$Banda, bin.breaks, include.lowest = TRUE)) # Classify them in bins
  y[which(rownames(y) == ID[i]), ] <- counts # Place them where the transectID is
}

y.sum <- apply(y, 1, sum) # Total count per site
nind <- sum(y)
#dclass <- m$Banda
#length(dclass) #394 and it should be 353. There are sampled transects not included in the layer? (41obs)
# PROBLEMS: SI02 (it is transect SI102, the SI02 doesnt exist and wasnt sampled) --> Error mio (4obs)
#           AF27 (doesnt appear en layer, like if it wasnt sampled but I have data) (8obs)
#           BE29 " (8obs)
#           AF02 " (11obs)
#           BE46 " (6obs)
#           SI98 " (1obs)
y[which(rownames(y) == "SI98"),]
tr@data[tr@data$Codi == "AF27", ]
which(m$transectID == "BE29")
length(which(m$transectID == "SI02"))

# Provisionaly: Keep only the ones present in the layer
m2 <- m[which(m$transectID %in% tr2017), ]
dclass <- m2$Banda