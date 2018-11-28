
rm(list=ls())

library(rjags)
library(jagsUI)
library(rgdal)


# Run model 4.1 in MECAL dataset (1 year)
  # ---- I ignore counts in each observation (cluster size)

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data")

d <- read.csv("DataDS_ready.csv")
mec17 <- d[which(d$Species == "MECAL" & d$Year == 2017), ] # Select species MECAL and year 2017
m <- mec17[ ,which(colnames(mec17) %in% c("Banda", "transectID"))]

# ---- Specify data for the model ----

# Number of sites obtained from the layer (so that sampled sites with 0 abundance also appear)
tr <- readOGR("C:/Users/Ana/Documents/PhD/Second chapter/Data/GIS", "Transects_2010_2017_EPSG23031") # Contains transects sampled each year (1/0)
tr2017 <- as.character(tr@data[which(tr@data$FETS2017 == 1), which(colnames(tr@data) %in% "Codi")])
nSites <- length(tr2017) 

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
dclass <- m$Banda
length(dclass) #394 and it should be 353. There are sampled transects not included in the layer? (41obs)
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


# ---- Compile data for JAGS model ----

data1 <- list(nsites=nSites, nG=nG, int.w=int.w, strip.width = strip.width, 
              y = y.sum, nind=nind, dclass=dclass, midpt = midpt)
