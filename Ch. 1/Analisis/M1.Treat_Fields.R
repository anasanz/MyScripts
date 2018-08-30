
# METHODS: Field distribution years - Treatments

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")
setwd("C:/Users/Ana/Documents/PhD/Datos/Datos barbechos arrendados/Variables")


f <- read.csv("Data_path_manuscript.csv", sep = ",", header=TRUE, fill = TRUE)
f <- f[which(f$EspecieObj == "BUOED"), ]

#Select 
f <- f[ , which(colnames(f) %in% c("Codi_Finca", "Any", "Tractament"))]
f$Tractament <- as.character(f$Tractament)
f$Codi_Finca <- as.character(f$Codi_Finca)
f$Any <- as.character(f$Any)

#Frequency
freq <- xtabs(  ~ Tractament + Any, data = f)




# APPENDIX: Vegetation- Treatments

setwd("C:/Users/ana.sanz/Documents/First chapter/Datos/Datos barbechos arrendados/Variables")

f <- read.csv("Data_path_manuscript.csv", sep = ",", header=TRUE, fill = TRUE)
f <- f[which(f$EspecieObj == "BUOED"), ]

#Select 
f <- f[ , which(colnames(f) %in% c("CF_A", "Tractament", "PromigAltura1Plot", "Recob_plotViu", "Recob_plotMort"))]

#Values mean + se
m <- aggregate(f[ ,c(1:3)], list(f$Tractament), mean)
m_round <- round(m [, c(2:4)], digits = 2)

sd <- aggregate(f[ ,c(1:3)], list(f$Tractament), sd) 
sd_round <- round(sd [, c(2:4)], digits = 2)

tab<-matrix(ncol = dim(m_round)[2], nrow = dim(m_round)[1])
for (i in 1:nrow(m_round)){
  pas<-paste(m_round[i, ], sd_round[i, ], sep=" +/- ")
  tab[i,]<-pas }

tab <- as.data.frame(tab)
colnames(tab) <- c("Cover", "Dead_Cover", "Height")
tab$Treatment <- m$Group.1

setwd("~/First chapter/RESULTS")
write.csv(tab, "Av_height.csv")

# Min and max

min <- aggregate(f[ ,c(1:3)], list(f$Tractament), min)
min_round <- round(min [, c(2:4)], digits = 2)

max <- aggregate(f[ ,c(1:3)], list(f$Tractament), max)
max_round <- round(max [, c(2:4)], digits = 2)

maxmin<-matrix(ncol = dim(min_round)[2], nrow = dim(min_round)[1])
for (i in 1:nrow(min_round)){
  pas<-paste(min_round[i, ], max_round[i, ], sep=" - ")
  maxmin[i,]<-pas }

maxmin <- as.data.frame(maxmin)
colnames(maxmin) <- c("Cover", "Dead_Cover", "Height")
maxmin$Treatment <- m$Group.1

write.csv(maxmin, "Maxmin_height.csv")

# Data for the main text: ha covered
total_area <- sum(f$area)
ha <- total_area/10000 #Total area covered

mean_area_ha <- mean(f$area)/10000 # Average area of FF
sd_area_ha <- sqrt(var(f$area))/10000

# Check FF that were repeated accross years
f$Codi_Finca <- as.character(f$Codi_Finca)
f15 <- f[which(f$Any == 2015), 2] 
f16 <- f[which(f$Any == 2016), 2] 
f17 <- f[which(f$Any == 2017), 2] 

rep <- Reduce(intersect, list(f15, f16, f17))

