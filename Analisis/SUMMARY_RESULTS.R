

# SUMMARY RESULTS

# % Of each species in each treatment (nº species present in the treatment / nº of total presences)

rm(list=ls())
setwd("~/PhD/Datos/Datos barbechos arrendados/Variables")

f <- read.csv("Variables.csv", sep = ",", header=TRUE, fill = TRUE)
colnames(f)[6] <- "EspecieObj"

# ---- SC ----

dataSC <- f[which(f$EspecieObj == "BUOED"), ]# All fields, select SC column

dataSC[dataSC == 99.00] <- NA
dataSC <- dataSC[-which(duplicated(dataSC[ , 2:15])), ]
dataSC <- dataSC[which(complete.cases(dataSC$Contatge)), ]
length(which(dataSC$Contatge > 0))
dataSC <- dataSC[which(dataSC$Tractament %in% c("Picar i herbicidar", "Picar", "Llaurar", "Curronar", "Alfals", "Control")), ]
dataSC$Tractament[which(dataSC$Tractament == "Curronar")] <- "Llaurar"

SC_present <- dataSC[which(dataSC$Contatge > 0), ] # Only SC fields where SC is present


ph_SC <- (length(which(SC_present$Tractament == "Picar i herbicidar"))/nrow(SC_present))*100
p_SC <- (length(which(SC_present$Tractament == "Picar"))/nrow(SC_present))*100

SC_present$Tractament[which(SC_present$Tractament == "Curronar")] <- "Llaurar" 
ll_SC <- (length(which(SC_present$Tractament == "Llaurar"))/nrow(SC_present))*100

a_SC <- (length(which(SC_present$Tractament == "Alfals"))/nrow(SC_present))*100

c_SC <- (length(which(SC_present$Tractament == "Control"))/nrow(SC_present))*100

SC <- c(ph_SC, p_SC, ll_SC, a_SC, c_SC)

# ---- LB ----

dataLB <- f[which(f$EspecieObj == "TERAX_m"), ]# All fields, select LB column

dataLB[dataLB == 99.00] <- NA
dataLB <- dataLB[-which(duplicated(dataLB[ , 2:15])), ]
dataLB <- dataLB[which(complete.cases(dataLB$Contatge)), ]
length(which(dataLB$Contatge > 0))
dataLB <- dataLB[which(dataLB$Tractament %in% c("Picar i herbicidar", "Picar", "Llaurar", "Curronar", "Alfals", "Control")), ]
dataLB$Tractament[which(dataLB$Tractament == "Curronar")] <- "Llaurar"

LB_present <- dataLB[which(dataLB$Contatge > 0), ] # Only LB fields where LB is present


ph_LB <- (length(which(LB_present$Tractament == "Picar i herbicidar"))/nrow(LB_present))*100
p_LB <- (length(which(LB_present$Tractament == "Picar"))/nrow(LB_present))*100

LB_present$Tractament[which(LB_present$Tractament == "Curronar")] <- "Llaurar" 
ll_LB <- (length(which(LB_present$Tractament == "Llaurar"))/nrow(LB_present))*100

a_LB <- (length(which(LB_present$Tractament == "Alfals"))/nrow(LB_present))*100

c_LB <- (length(which(LB_present$Tractament == "Control"))/nrow(LB_present))*100

LB <- c(ph_LB, p_LB, ll_LB, a_LB, c_LB)

# ---- CL ----

dataCL <- f[which(f$EspecieObj == "TERAX_m"), ]# All fields, select CL column

dataCL[dataCL == 99.00] <- NA
dataCL <- dataCL[-which(duplicated(dataCL[ , 2:15])), ]
dataCL <- dataCL[which(complete.cases(dataCL$Contatge)), ]
length(which(dataCL$Contatge > 0))
dataCL <- dataCL[which(dataCL$Tractament %in% c("Picar i herbicidar", "Picar", "Llaurar", "Curronar", "Alfals", "Control")), ]
dataCL$Tractament[which(dataCL$Tractament == "Curronar")] <- "Llaurar"

CL_present <- dataCL[which(dataCL$Contatge > 0), ] # Only CL fields where CL is present


ph_CL <- (length(which(CL_present$Tractament == "Picar i herbicidar"))/nrow(CL_present))*100
p_CL <- (length(which(CL_present$Tractament == "Picar"))/nrow(CL_present))*100

CL_present$Tractament[which(CL_present$Tractament == "Curronar")] <- "Llaurar" 
ll_CL <- (length(which(CL_present$Tractament == "Llaurar"))/nrow(CL_present))*100

a_CL <- (length(which(CL_present$Tractament == "Alfals"))/nrow(CL_present))*100

c_CL <- (length(which(CL_present$Tractament == "Control"))/nrow(CL_present))*100

CL <- c(ph_CL, p_CL, ll_CL, a_CL, c_CL)

# ---- Join treatments ----

treatment <- c("S+H", "S", "T", "A", "C")

summary <- data.frame (treatment, SC, LB, CL)
