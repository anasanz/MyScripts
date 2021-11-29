

# Sort data DRYAD

setwd("D:/PhD/Third chapter/Data")
d <- read.csv("DataDS_ch3_15_19_READY_FIXED_LAST_GASSP.csv")
sort(unique(d$Species))
d <- d[-which(d$Species %in% c("GACRI", "GATHE", "PADOM", "STSSP")), ]
sort(unique(d$Species))

# To restrict distribution of PTALC and CABRA: remove observations out of the distr.range (probably a mistake)
unique(d$Region.Label)
d <- d[-which(d$Species == "CABRA" & d$Region.Label %in% c("BA", "SI", "BM", "AL")), ]
d[which(d$Species == "PTALC" & d$Region.Label %in% c("BA", "SI", "BM", "AL", "BE")), ]
d[which(d$Species == "PTORI" & d$Region.Label %in% c("AF","SI", "BM", "AL", "BE")), ]


d <- d[ ,which(colnames(d) %in% c("Year", "Species", "Banda", "Observer", "transectID", "distance"))]

d$transectID2 <- as.integer(as.factor(d$transectID))
d$Observer <- as.integer(as.factor(d$Observer))


sum <- d %>%
  group_by(transectID, transectID2) %>%
  summarise(n())
  
write.csv(d,"Data_HDS_dryad.csv")


# ---- Co-variates ----

setwd("D:/PhD/Third chapter/Data")
aes <- read.csv("AES_15_19.csv")
sg <- read.csv("SG_15_19.csv")
green <- read.csv("GREEN_15_19.csv", sep = ";")
crop_diversity <- read.csv("crop_richness_500.csv")
field_size <- read.csv("av_fieldsize_500.csv")


aes <- aes[which(aes$Codi %in% all.sites), ] # Select transects with census
sg <- sg[which(sg$Codi %in% all.sites), ] 
green <- green[which(green$Codi %in% all.sites), ] 
crop_diversity <- crop_diversity[which(crop_diversity$Codi %in% all.sites), ] 
field_size <- field_size[which(field_size$Codi %in% all.sites), ] 


# Be sure the fields are in the same order
order <- as.data.frame(m)
order_codi <- as.vector(rownames(order))
order$Codi <- order_codi
aes <- left_join(order,aes, by = "Codi")
sg <- left_join(order,sg, by = "Codi")
green <- left_join(order,green, by = "Codi")
crop_diversity <- left_join(order,crop_diversity, by = "Codi")
field_size <- left_join(order,field_size, by = "Codi")

# JOIN WITH ENCRIPTED CODE FOR DATA UPLOAD
colnames(field_size)[which(colnames(field_size) == "Codi")] <- "transectID"
colnames(crop_diversity)[which(colnames(crop_diversity) == "Codi")] <- "transectID"
colnames(aes)[which(colnames(aes) == "Codi")] <- "transectID"
colnames(sg)[which(colnames(sg) == "Codi")] <- "transectID"
colnames(green)[which(colnames(green) == "Codi")] <- "transectID"

field_size <- left_join(field_size, sum, by = "transectID")
crop_diversity <- left_join(crop_diversity, sum, by = "transectID")
aes <- left_join(aes, sum, by = "transectID")
sg <- left_join(sg, sum, by = "transectID")
green <- left_join(green, sum, by = "transectID")


# Need to log-transform the FALLOW variables (AES and SG) because of extreme values (see script Explore_corr.r)

setwd("D:/PhD/Third chapter/Data/Driad")

# Area AES
area_aes_real <- as.matrix(aes[ ,c(8:12)])
area_aes_real <- area_aes_real + 0.001 # Add a constant for log-transform (log(0) = Inf)
area_aes <- log(area_aes_real)

aes_mean <- mean(area_aes) # Also scale to unify with different variables like crop diversity
aes_sd <- sd(area_aes)
aes_sc <- (area_aes - aes_mean) / aes_sd

colnames(aes_sc) <- c("2015", "2016", "2017", "2018", "2019")
aes_sc <- as.data.frame(aes_sc)
aes_sc$transectID <- field_size$transectID2

write.csv(aes_sc, "AES_Dryad.csv")

# Area SG
area_sg_real <- as.matrix(sg[ ,c(8:12)])
area_sg_real <- area_sg_real + 0.001 # Add a constant for log-transform (log(0) = Inf)
area_sg <- log(area_sg_real)


sg_mean <- mean(area_sg)
sg_sd <- sd(area_sg)
sg_sc <- (area_sg - sg_mean) / sg_sd

colnames(sg_sc) <- c("2015", "2016", "2017", "2018", "2019")
sg_sc <- as.data.frame(sg_sc)
sg_sc$transectID <- field_size$transectID2

write.csv(sg_sc, "SG_Dryad.csv")

# Area GREEN
area_green_real <- as.matrix(green[ ,c(8:12)])
area_green_real <- area_green_real + 0.001 # Add a constant for log-transform (log(0) = Inf)
area_green <- log(area_green_real)


green_mean <- mean(area_green)
green_sd <- sd(area_green)
green_sc <- (area_green - green_mean) / green_sd

colnames(green_sc) <- c("2015", "2016", "2017", "2018", "2019")
green_sc <- as.data.frame(green_sc)
green_sc$transectID <- field_size$transectID2

write.csv(green_sc, "Green_Dryad.csv")

# Crop diversity
crop_diversity <- as.matrix(crop_diversity[ ,c(8:12)])

cd_mean <- mean(crop_diversity)
cd_sd <- sd(crop_diversity)
cd_sc <- (crop_diversity - cd_mean) / cd_sd

colnames(cd_sc) <- c("2015", "2016", "2017", "2018", "2019")
cd_sc <- as.data.frame(cd_sc)
cd_sc$transectID <- field_size$transectID2

write.csv(sg_sc, "CD_Dryad.csv")

# Field size
field_size <- as.matrix(field_size[ ,c(8:12)])

fs_mean <- mean(field_size)
fs_sd <- sd(field_size)
fs_sc <- (field_size - fs_mean) / fs_sd

colnames(fs_sc) <- c("2015", "2016", "2017", "2018", "2019")
fs_sc <- as.data.frame(fs_sc)
fs_sc$transectID <- field_size$transectID2

write.csv(fs_sc, "FS_Dryad.csv")


