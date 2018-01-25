
#CÁLCULO DE INDICES FINCAS F.BIODIVERSIDAD

library(tidyr)
library(dplyr)

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Datos de campo/Dades F.biodiversidad 2017/Lleida")
f <- read.csv("C_Veg_2017.csv", sep = ";", header=TRUE, fill = TRUE, na.strings="")

colnames(f)[2] <- "Codi_Finca"
colnames(f)[3] <- "Any"
f <- f[ , which(colnames(f) %in% c("Codi_Finca", "Any", "Plot", "Especie", "Cobertura"))]

##########################################################################################

#1. Media cobertura plots

f$CF_A<-paste(f$Codi_Finca,f$Any,sep = "-")
ID <- unique(f$CF_A)
f$CF_A <- as.character(f$CF_A)
f<-droplevels(f)
f$Cobertura[is.na(f$Cobertura)]<-0
#Create an empty matrix with the length of the species (wide) and fincas(long)
m <- matrix(0, ncol=length(unique(f$Especie)), nrow=length(unique(f$CF_A)) )
m <-data.frame(m)
colnames(m) <- unique(f$Especie)
rownames(m) <- unique(f$CF_A) 
#Loop
for( i in 1:length(ID)){
  
  tmp <- f[f$CF_A==ID[i], ]
  ag<-aggregate(Cobertura ~ Especie, data = tmp, mean)
  m[i, as.character(ag$Especie)] <- ag$Cobertura
}

m$CF_A<-rownames(m)
m<-separate(data = m, col = CF_A, into = c("Codi_Finca", "Any"), sep = "\\-")

colnames(m) <- sub(" ", ".", colnames(m))
#write.csv(m,file = "Cobertura_media_flora_fdiv.csv")


#################################################################################


#2. Data Index

#COVER
#Check species for which I have traits
setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")
t <- read.csv("Cover_traits.csv", sep = ",", header=TRUE, fill = TRUE, na.strings="")


colnames(m)[1]<-"Capsella.bursa.pastoris"
colnames(m)[44] <- "Bromus.gr..rubens"
colnames(m)[7] <- "Hordeum.murinum.subsp..leporinum"

length(which(colnames(t) %in% colnames(m))) #53 (enough). m = COVER

#HEIGHT
#Dato bibliogrÃ¡fico para cada especie
g<-read.csv("Species_traits_full.csv")
v<-names(m)[which(names(m) %in% g$V1)] #Vector with the columns of the species that have information
data_av<-g[which(g$V1 %in% v), ] #SPECIES WITH DATA AVAILABLE
dat<-data_av[,c(7,4)] #dat = HEIGHT
dat <- dat[complete.cases(dat), ]

alt <- as.data.frame(t(dat))
colnames(alt) <- dat$V1
alt <- alt [-c(1), ]
alt <- alt [ ,which(colnames(alt) %in% colnames(m))]  #Seleccionar datos de altura para los que hay cobertura
#Change to numeric
for(i in c(1:ncol(alt))) {
  alt[,i] <- as.numeric(as.character(alt[,i]))
}

#SAME COLUMNS COVER AND HEIGHT
m <- m[ , which(colnames(m) %in% dat$V1)] #Seleccionar datos cobertura para los que hay altura (bibliográfica)
setcolorder(m, colnames(alt)) 
colnames(alt) == colnames(m)
#Change to numeric
for(i in c(1:ncol(m))) {
  m[,i] <- as.numeric(as.character(m[,i]))
}


#SLA
sla<-data_av[,c(7,3)]
sla <- sla[complete.cases(sla),]
slat <- as.data.frame(t(sla))
colnames(slat) <- sla$V1
slat <- slat [-c(1), ]

for(i in c(1:ncol(slat))) {
  slat[,i] <- as.numeric(as.character(slat[,i]))
}

####################################################################################

 #3. Index calculation

#LEAF AVAILABILITY INDEX
#INDEX = m (cover) x alt(alt) x sla. 

#Cover * alt

v <- unname(unlist(alt[1,]))
v <- as.numeric(as.character(v))

mul <- mapply("*",m,v)
mul <- as.data.frame(mul)

#mul * slat

v <- unname(unlist(slat[1,]))
v <- as.numeric(as.character(v))

y <- mapply("*",mul,v)
y <- as.data.frame(y)

y$LAI<-rowSums(y)


#SEED AVAILABILITY INDEX
#3. SEED
seed<-data_av[,c(7,6,5)]

seed <- seed[complete.cases(seed),]
seedt <- as.data.frame(t(seed))
colnames(seedt) <- seed$V1
seedt <- seedt [-c(1), ]

#_*Seed mass
v1 <- unname(unlist(seedt[1,]))
v1 <- as.numeric(as.character(v1))

u <- mapply("*",mul,v1)
u <- as.data.frame(u)

#Flowering bloom
v2 <- unname(unlist(seedt[2,]))
v2 <- as.numeric(as.character(v2))

x <- mapply("*",u,v2)
x <- as.data.frame(x)

x$SAI<-rowSums(x)
y$SAI <- x$SAI

y$LAI_sd <- scale(y$LAI)
y$SAI_sd <- scale(y$SAI)
rownames(y) <- rownames(m)

write.csv(y, file = "av_index_fdiv.csv")

#Check extreme values

SAI <- y[order(y$SAI_sd), ]

# Comprobar tratamientos valores extremos

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados")
c <- read.csv("TODOS_TRATAMIENTOS.csv",sep = ",",
            header=TRUE,fill = TRUE)

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Datos de campo/Dades F.biodiversidad 2017/Lleida")
y <- read.csv("av_index_fdiv.csv", sep = ",", header=TRUE, fill = TRUE, na.strings="")

y <- y[ , c(1, 52:55)]
SAI <- y[order(y$SAI_sd), ]

write.csv(SAI, file = "av_index_fdiv_print.csv")

#Search treatments
c <- c[c$Any == "2017", ]

#BE
#BAJO
c$Tractament[which(c$Codi_Finca == "BE119A")]
c$Tractament[which(c$Codi_Finca == "BE11D")]
c$Tractament[which(c$Codi_Finca == "BE148A")]
c$Tractament[which(c$Codi_Finca == "BE97A")]
c$Tractament[which(c$Codi_Finca == "BE103A")]
c$Tractament[which(c$Codi_Finca == "BE104A")]
c$Tractament[which(c$Codi_Finca == "BE28A")]
c$Tractament[which(c$Codi_Finca == "BE27B")]
c$Tractament[which(c$Codi_Finca == "BE141A")]


#ALTO
c$Tractament[which(c$Codi_Finca == "BE73A")]
c$Tractament[which(c$Codi_Finca == "BE137A")]
c$Tractament[which(c$Codi_Finca == "BE87A")]

#GR
#BAJO
c$Tractament[which(c$Codi_Finca == "GR20A")]
c$Tractament[which(c$Codi_Finca == "GR2A")]
c$Tractament[which(c$Codi_Finca == "GR14A")]

#ALTO
c$Tractament[which(c$Codi_Finca == "GR31")]
c$Tractament[which(c$Codi_Finca == "GR37")]
c$Tractament[which(c$Codi_Finca == "GR41")]

