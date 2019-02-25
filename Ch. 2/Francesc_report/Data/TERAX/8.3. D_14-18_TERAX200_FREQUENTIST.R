
# Run model 8.2 in TERAX in a frequentist way to compare and see if confidence intervals are different

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data")
d <- read.csv("DataDS_ready_ALL.csv")

d <- d[which(d$Year %in% c(2014, 2015, 2016, 2017, 2018)), ]

# Information: bins, years, sites

strip.width <- 200 				
dist.breaks <- c(0,25,50,100,200)
int.w <- diff(dist.breaks) # width of distance categories (v)
midpt <- diff(dist.breaks)/2+dist.breaks[-5]
nG <- length(dist.breaks)-1

yrs <- c(2014, 2015, 2016, 2017, 2018)
nyrs <- length(yrs)

# To take into account transects with abundance 0
# 1. Select all transects IDs from all species observations
# 2. Join the observations of TERAX (for example) with all transects so that they remain with NA if the
# species was there but it wasnt sampled

d_tr <- d[ ,which(colnames(d) %in% c("Species",  "T_Y", "Observer"))]
d_tr_all <- data.frame(T_Y = unique(d_tr$T_Y), id = NA)


# For observer variable
d_tr$Observer <- as.character(d_tr$Observer)
d_tr_all_obs <- left_join(d_tr_all, d_tr)
d_tr_all_obs <- d_tr_all_obs[ ,c(1,4)]
d_tr_all_obs <- d_tr_all_obs[which(!duplicated(d_tr_all_obs)), ] # Table with all sampled fields and which observer sampled it
d_tr_all_obs$Observer <- as.character(d_tr_all_obs$Observer)
d_tr_all_obs$T_Y <- as.character(d_tr_all_obs$T_Y)


mec <- d[which(d$Species == "TERAX"), which(colnames(d) %in% c("Year", "Banda", "transectID", "T_Y", "Species", "Observer"))] # Select species MECAL and all years
mec <- arrange(mec, Year, transectID) #Ordered
mec_detec_transectID <- unique(mec$transectID)
mec$Observer <- as.character(mec$Observer) 


absent <- anti_join(d_tr_all,mec) # Transects with 0 abundance, add to mec.
colnames(absent)[2] <- "Banda" # Format it to add the rows to mec
absent$T_Y <- as.character(absent$T_Y)
absent$Species <- "TERAX"
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

all_mec <- rbind(mec,absent) # Include transects with abundance 0
all_mec <- arrange(all_mec, Year, transectID) # Ordered

absent$count <- 0


# ---- Distance observations ----

# Format
all.sites <- unique(all_mec$transectID)
all.sites <- sort(all.sites,descreasing = TRUE)
max.sites <- length(all.sites)

m <- matrix(NA, nrow = length(all.sites), ncol = nyrs)
rownames(m) <- all.sites
colnames(m) <- yrs

# Add counts > 0
count <- aggregate(Species ~ Year + transectID, FUN = length, data = mec)

for (i in 1:nrow(count)){
  m[which(rownames(m) %in% count$transectID[i]), which(colnames(m) %in% count$Year[i])] <- count$Species[i]
}

# Add absences (0)
for (i in 1:nrow(absent)){
  m[which(rownames(m) %in% absent$transectID[i]), which(colnames(m) %in% absent$Year[i])] <- absent$count[i]
}

# Only to check: Count of individuals per year

count.year <- colSums(m,na.rm = TRUE)
# ---- Co-variates ----

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data")
#setwd("C:/Users/ana.sanz/OneDrive/PhD/Second chapter/Data")
manag <- read.csv("management_area_200.csv")

manag <- manag[ , c(1,2,11:15)] # Select years 2014 - 2018
#manag$area_sg13 <- 0 # Add 13 as a 0
#manag$area_sg13 <- as.numeric(manag$area_sg13)

#manag <- manag[ ,c(1,2,8,3:7)] # Order years

manag <- manag[which(manag$Codi %in% all.sites), ] # Select transects with census

# Be sure the fields are in the same order
order <- as.data.frame(m)
order_codi <- as.vector(rownames(order))
order$Codi <- order_codi
manag <- left_join(order,manag)


# Area SG
area_sg <- as.matrix(manag[ ,c(8:12)])

sg_mean <- mean(area_sg)
sg_sd <- sd(area_sg)
sg_sc <- (area_sg - sg_mean) / sg_sd

# Zone (Occidental = 0; Oriental = 1)
zone <- order
for (i in 1:nrow(zone)){
  if(substr(zone$Codi[i], 1,2) == "BA"){zone[i,1:5] <- 0}
  if(substr(zone$Codi[i], 1,2) == "BM"){zone[i,1:5] <- 1}
  if(substr(zone$Codi[i], 1,2) == "SI"){zone[i,1:5] <- 1}
  if(substr(zone$Codi[i], 1,2) == "AF"){zone[i,1:5] <- 0}
  if(substr(zone$Codi[i], 1,2) == "BE"){zone[i,1:5] <- 1}
  if(substr(zone$Codi[i], 1,2) == "GR"){zone[i,1:5] <- 0}
}
zone <- zone[,-6]

# Observer 

# Format
obs <- matrix(NA, nrow = max.sites, ncol = nyrs)
rownames(obs) <- all.sites
colnames(obs) <- yrs

# Add observers for fields with counts > 0
for (i in 1:nrow(mec)){
  obs[which(rownames(obs) %in% mec$transectID[i]), which(colnames(obs) %in% mec$Year[i])] <- mec$Observer[i]
}

# Add observers for fields with absences (0)
for (i in 1:nrow(absent)){
  obs[which(rownames(obs) %in% absent$transectID[i]), which(colnames(obs) %in% absent$Year[i])] <- absent$Observer[i]
}

