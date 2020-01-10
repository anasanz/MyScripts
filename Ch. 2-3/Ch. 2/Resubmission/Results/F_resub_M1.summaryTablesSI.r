#######################################################################################
#####                 METHODS: SUPPLEMENTARY MATERIAL (RESUBMISSION)        ##############
#######################################################################################

rm(list=ls())

library(dplyr)
library(tidyr)

# 1. Number of transects per year

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Resubmission")
d <- read.csv("DataDS_ready_ALL_revch2.csv")

d_transects <- d[ ,which(colnames(d) %in% c("Year", "T_Y", "transectID"))]
d_transects <- d_transects[which(!duplicated(d_transects$T_Y)), ]

trans <- aggregate(transectID ~ Year, data = d_transects, FUN = length)
colnames(trans)[2] <- "Number of transects"

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Resubmission/Results/Final")
write.csv(trans, "TableSI_TransectsYear.csv")

# See proportion not sampled every year
?spread
d_transects1 <- d_transects[ ,-which(colnames(d_transects) %in% c("T_Y"))] 
d_transects1$transectID2 <- d_transects1$transectID
d_transects2 <- spread(d_transects1, Year, transectID)
nrow(d_transects2)
nrow(d_transects2[complete.cases(d_transects2), ])/nrow(d_transects2) # % of transects sampled every year

nas <- function(x){
  num_na <- sum(is.na(x))
  return(num_na)
}

number_nas <- apply(d_transects2[,c(2:10)],1, function (x) nas(x)) 
length(number_nas[which(number_nas == 0)])/nrow(d_transects2) # % of transects sampled every year

length(number_nas[which(number_nas > 0 & number_nas < 3)])/nrow(d_transects2)
length(number_nas[which(number_nas > 2)])/nrow(d_transects2) # % of transects not sampled more than 2 years
1-0.23# % of transects sampled at least 8 years

# Average number of years surveyed
years_sampled <- 9-number_nas
mean(years_sampled)
sd(years_sampled)

# From ARCGIS (study area map, layer that intersects spa with transects): 133 transects intersect with SPA
133/166


# 2. Number of observers per year

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Resubmission")
d <- read.csv("DataDS_ready_ALL_revch2.csv")

unique(d$Observer) # For acknowledgements

d$Observer <- as.numeric(d$Observer)
year <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")

ob_table <- d %>% group_by(Year) %>% summarise(n_distinct(Observer)) # Check:

ob <- list()

for (i in 1:9){
  d_y <- d[which(d$Year == year[i]),]
  Nob <- unique(d_y$Observer)
  ob[[i]] <- Nob
}

library(plyr)

ob1 <- ldply(ob, rbind)
ob1 <- as.data.frame(t(ob1))
colnames(ob1) <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")
n_obs <- !is.na(ob1)
n_obs <- colSums(n_obs)
t(n_obs)

# 3. Species summary: Proportion of transects occupied by each species yearly

s_good <- c("SYCAN", "CACHL", "LASEN", "ALRUF", "PAMAJ", "ALARV", "CABRA", "CACAR", "COPAL", "PIPIC",
            "COOEN", "HIRUS", "PYRAX", "CAINA", "COMON", "PADOM", "PAMON", "FATIN", "SESER", "TUMER", "GATHE",
            "BUOED", "SYMEL", "UPEPO", "GACRI", "STSSP", "MICAL", "TERAX", "MEAPI", "MECAL") # This has to be in the same order than sp_convergence

s_good <- sort(s_good)
year <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
n_transects <- trans$`Number of transects`

prop_sp <- as.data.frame(matrix(ncol = length(year), nrow = length(s_good)))
rownames(prop_sp) <- s_good
colnames(prop_sp) <- year

for (i in 1:length(s_good)){
  d_sp <- d[which(d$Species == s_good[i]), ]
  for (t in 1:9){
    d_t <- d_sp[which(d_sp$Year == year[t]), ]
    d_prop <- (length(unique(d_t$T_Y))/n_transects[t])*100
    prop_sp[i,t] <- d_prop
  }
}
d_prop <- (length(unique(d_sp$T_Y))/1083)*100

prop_sp <- round(prop_sp,2)

prop_sp$Mean_proportion <- round(apply(prop_sp,1,mean), 2) # average of species occupancy in transects 

# Join with legend for english names

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Data")
leg <- read.csv("leg_species.csv", sep = ";")
leg <- arrange(leg,codiEspecie)
colnames(leg)[1] <- "sp"

prop_sp$sp <- rownames(prop_sp)

prop_sp <- left_join(prop_sp, leg) 
prop_sp <- prop_sp[ ,-c(11,12)]
prop_sp <- arrange(prop_sp,English)
prop_sp <- prop_sp[ ,c(11,1:10)]

setwd("C:/Users/Ana/Documents/PhD/Second chapter/Resubmission/Results/Final")
#write.csv(prop_sp, "TableSI2_sp_prop.csv")

arrange(prop_sp,Mean_proportion)
yearly_avg <- summarise_all(prop_sp, funs(mean))

# 3. Species summary: Proportion of individuals occupying all transects


s_good <- c("ALRUF","BUOED","CACAR","COOEN","COPAL","GACRI","GATHE","MEAPI","MECAL","PAMAJ","SESER","STSSP","SYCAN","SYMEL","TERAX","UPEPO",
            "MICAL","HIRUS","PADOM","PIPIC","PAMON", "COMON") 
s_good <- sort(s_good)
year <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
n_transects <- trans$`Number of transects`

prop_sp <- as.data.frame(matrix(ncol = 1, nrow = length(s_good)))
rownames(prop_sp) <- s_good
colnames(prop_sp) <- "prop"

for (i in 1:length(s_good)){
  d_sp <- d[which(d$Species == s_good[i]), ]
  d_prop <- (length(unique(d_sp$T_Y))/1083)*100
  prop_sp[i,1] <- d_prop}


