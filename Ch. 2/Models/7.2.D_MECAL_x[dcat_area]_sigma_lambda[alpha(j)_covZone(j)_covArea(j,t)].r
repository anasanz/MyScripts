
rm(list=ls())

library(rjags)
library(jagsUI)
library(rgdal)
library(dplyr)

# Run model 4.1 in MECAL dataset (1 year)
# ---- I ignore counts in each observation (cluster size)

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data")

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



absent <- anti_join(d_tr_all,mec) # Transects with 0 abundance, add to mec?



# ---- Specify data for the model ----

# Bin information
strip.width <- 200 				
dist.breaks <- c(0,25,50,100,200)
int.w <- diff(dist.breaks) # width of distance categories (v)
midpt <- diff(dist.breaks)/2+dist.breaks[-5]
nG <- length(dist.breaks)-1	

# Distance observations

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