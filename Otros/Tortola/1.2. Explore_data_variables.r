
rm(list=ls())

library(dplyr)

# Load data
setwd("D:/PhD/Otros/Tórtola/Data")

load("D:/PhD/Otros/Tórtola/Data/data_buff500.rdata")
colnames(data_buff500)[1] <- "Site"
colnames(data_buff500)[2] <- "Section"

# Create site_section
data_buff500$site_sec <- paste(data_buff500$Site, data_buff500$Section, sep = "_")

# Select interesting variables
colnames(data_buff500)
var <- data_buff500[ ,c("Hm_mean", "Hm_max", "FCC_mean", "FCC100%", "Forest%", "richness", "ForestMargin", "ForestMargin%", "site_sec")]

tor <- read.csv("tortola_ds.csv")

# Número de detecctiones por transecto-sección ~ variables
tor_det <- tor %>% 
  group_by(site_sec) %>%
  summarise(detections = n())

tor_det <- tor %>% 
  group_by(site_sec, site_year) %>%
  summarise(detections = n())

hist(tor_det$detections, breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))

dat <- left_join(tor_det, var, by = "site_sec")
dat <- as.data.frame(dat)
variables <- colnames(var)[-9]
class(dat)


par(mfrow = c(2,4))
par(mfrow = c(1,1))
for(i in 1:length(variables)){
  plot(dat$detections ~ dat[,colnames(dat) %in% variables[i]], main = variables[i], pch = 18)
}


par(mfrow = c(2,4))
par(mfrow = c(1,1))
for(i in 1:length(variables)){
  hist(dat[,colnames(dat) %in% variables[i]], main = variables[i])
}


# TEMPERATURE CO-VARIATE
setwd("D:/PhD/Otros/Tórtola/Data")

tor <- read.csv("tortola_ds_ready.csv", sep = ",")
tor[,1] <- "STTUR"

hist(tor$Temperatura)

library(dplyr)
t2 <- tor %>% 
  group_by(site_sec, Year) %>%
  mutate(count_sitesec = sum(count))
  
t3 <- t2[-which(duplicated(t2)), ]  
plot(t3$count_sitesec ~ t3$Temperatura)
