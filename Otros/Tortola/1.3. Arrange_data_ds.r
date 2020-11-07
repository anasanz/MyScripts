
##################################################################################
############      ARRANGE DATA FOR DS ANALYSIS      ########################
##################################################################################

# Load packages

rm(list=ls())

library(dplyr)


setwd("D:/PhD/Otros/Tórtola/Data")

tor <- read.csv("tortola_ds.csv", sep = ",")
tor <- tor[ ,-1]
tor$count <- 1
tor <- tor[-which(tor$Site > 70000), ]

all_tr <- read.csv("FS_SOCCs_ampliats.csv", sep = ";")
colnames(all_tr)[1] <- "Site"
colnames(all_tr)[2] <- "Year"
all_tr <- all_tr[which(all_tr$Periode == 2), ]
all_tr$site_year <- paste(all_tr$Site, all_tr$Year, sep = "_") # All transects_year sampled
all_tr <- arrange(all_tr, Site, Year)

# ---- Modify bin ----
unique(tor$Bin)
for (i in 1:nrow(tor)){
  if (tor$Bin[i] == "banda1") {tor$Bin[i] = 1}
  else if (tor$Bin[i] == "banda2") {tor$Bin[i] = 2}
  else if (tor$Bin[i] == "banda3") {tor$Bin[i] = 3}
}

# ---- Get absences ---- #

allsites <- data.frame(site_year = all_tr$site_year) # Identify all transects IDs sampled

all_tr_sec <- do.call("rbind", replicate(6, all_tr, simplify = FALSE)) # Add sections
all_tr_sec <- arrange(all_tr_sec, site_year)
all_tr_sec$Section <- rep(c(1:6), times = nrow(all_tr))
all_tr_sec$site_sec <- paste(all_tr_sec$Site, all_tr_sec$Section, sep = "_") # All transects_year sampled
#all_tr_sec <- all_tr_sec[ ,-c(1,2,3,4,5,13)]

d <- left_join(all_tr_sec, tor, by = c("site_year", "site_sec")) # Which count is NA are the transects with 0 detections
absence <- d[which(is.na(d$count)), ]
f <- unique(tor$site_year)

# ---- Join climate data and absences to database ---- #

# Join climate
tor2 <- left_join(tor, all_tr_sec, by = c("site_sec", "site_year")) 

# Sort absence ant tor2 to do rbind
colnames(tor2)[1] <- "Site"
colnames(tor2)[2] <- "Year"
colnames(tor2)[4] <- "Section"
tor2 <- tor2[ ,-c(9:13, 20)]
colnames(tor2)[3] <- "Observer"

absence <- absence[ ,-c(3,4, 15:18)]
colnames(absence)[1] <- "Site"
colnames(absence)[2] <- "Year"
colnames(absence)[3] <- "Observer"
colnames(absence)[11] <- "Section"
absence$count <- 0

colnames_order <- c("Site", "Year", "Section", "site_year", "site_sec", "Observer", "count", "Bin", 
                    "Temperatura", "Vent", "Cel", "Pluja", "Visibilitat", "Tipus")

tor2 <- tor2[,match(colnames_order, colnames(tor2))]
absence <- absence[,match(colnames_order, colnames(absence))]

dat <- rbind(tor2, absence)

# ---- OBSERVER ---- #

obs <- unique(dat$Observer)

# Cuantos transectos ha hecho cada observador

data_obs <- dat %>%
  group_by(Observer) %>%
  summarise(n_transects = n_distinct(site_year))

freq_obs <- arrange(data_obs, n_transects)
freq_obs$Observer <- factor(freq_obs$Observer, levels = freq_obs$Observer)

barplot(freq_obs$n_transects ~ freq_obs$Observer, las = 2)
abline(h = 10)

data_obs <- dat %>%
  group_by(Observer) %>%
  summarise(n_transects = n_distinct(site_year))

d <- dat[which(dat$Observer == "1180"), ]
d <- arrange(d, site_sec)
k <- unique(d$site_year)

# Select observers that did less than 5 census
obs_less5 <- freq_obs[which(freq_obs$n_transects < 5), ]
obs_less5 <- unique(obs_less5$Observer)
dat_less5 <- dat[which(dat$Observer %in% obs_less5), ] #
dat_less5_detect <- dat_less5[which(dat_less5$count == 1), ]  # Perdemos 915 observaciones (pero solo 2015 detecciones), pero para meter observador como variable es necesario
# PROBAMOS CON 4
# Remove
dat <- dat[-which(dat$Observer %in% obs_less5), ] 
length(unique(dat$site_year)) 

dat <- arrange(dat, Site, Year, Section)

# Join forest variable BUFFER 500M

setwd("D:/PhD/Otros/Tórtola/Data")

load("D:/PhD/Otros/Tórtola/Data/data_buff500.rdata")
colnames(data_buff500)[1] <- "Site"
colnames(data_buff500)[2] <- "Section"

# Create site_section
data_buff500$site_sec <- paste(data_buff500$Site, data_buff500$Section, sep = "_")

# Select interesting variables
colnames(data_buff500)
var <- data_buff500[ ,c("site_sec", "Hm_mean", "Hm_max", "FCC_mean", "FCC100%", "Forest%", "richness", "ForestMargin", "ForestMargin%")]
var2 <- var[,which(colnames(var) %in% c("site_sec", "Forest%"))]
dat2 <- left_join(dat, var2)
colnames(dat2)[15] <- "Forest"

setwd("D:/PhD/Otros/Tórtola/Data")
write.csv(dat2, "tortola_ds_ready.csv")


