

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data")

d <- read.csv("DataDS_ready.csv")
mec17 <- d[which(d$Species == "MECAL" & d$Year == 2017), ]

# ---- Specify data for the model ----
nSites <- length(unique(d$transectID)) 
# No. I need to get the transects from the layer, here the 0counts don't appear

# ---- Compile data for JAGS model ----

data1 <- list(nsites=nSites, nG=nG, int.w=int.w, strip.width = strip.width, 
              y = y.sum, nind=nind, dclass=dclass, midpt = midpt)
