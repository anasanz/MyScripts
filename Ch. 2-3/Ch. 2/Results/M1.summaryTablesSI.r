
#######################################################################################
#####                 METHODS: SUPPLEMENTARY MATERIAL                   ##############
#######################################################################################

library(dplyr)

# 1. Number of transects per year

setwd("S:/PhD/Second chapter/Data")
d <- read.csv("DataDS_ready_ALL.csv")
d_transects <- d[ ,which(colnames(d) %in% c("Year", "T_Y", "transectID"))]
d_transects <- d_transects[which(!duplicated(d_transects$T_Y)), ]

trans <- aggregate(transectID ~ Year, data = d_transects, FUN = length)
colnames(trans)[2] <- "Number of transects"

setwd("S:/PhD/Second chapter/Data/Results/Paper")
write.csv(trans, "TableSI_TransectsYear.csv")

# 2. Number of observers per year

setwd("S:/PhD/Second chapter/Data")
d <- read.csv("DataDS_ready_ALL.csv")
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
nrows(ob1[which(complete.cases(ob1)), ])
n_obs <- !is.na(ob1)
n_obs <- colSums(n_obs)
t(n_obs)

