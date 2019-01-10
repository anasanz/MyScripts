

# SUMMARY RESULTS

# FOR THE ARTICLE, GOOD ONE: % Of each species in each treatment (weighted by treatment) 
#(presences in the treatment / num of fields of that treatment)


rm(list=ls())
setwd("~/Datos/Datos barbechos arrendados/Variables")

f <- read.csv("Data_path_manuscript.csv", sep = ",", header=TRUE, fill = TRUE)
colnames(f)[4] <- "EspecieObj"


# ---- SC ----

dataSC <- f[which(f$EspecieObj == "BUOED"), ]# All fields, select SC column
SC_present <- dataSC[which(dataSC$Contatge > 0), ] # Only SC fields where SC is present


ph_SC <- (length(which(SC_present$Tractament == "Picar i herbicidar"))/length(which(dataSC$Tractament == "Picar i herbicidar")))*100
p_SC <- (length(which(SC_present$Tractament == "Picar"))/length(which(dataSC$Tractament == "Picar")))*100
ll_SC <- (length(which(SC_present$Tractament == "Llaurar"))/length(which(dataSC$Tractament == "Llaurar")))*100
a_SC <- (length(which(SC_present$Tractament == "Alfals"))/length(which(dataSC$Tractament == "Alfals")))*100
c_SC <- (length(which(SC_present$Tractament == "Control"))/length(which(dataSC$Tractament == "Control")))*100

SC <- c(ph_SC, p_SC, ll_SC, a_SC, c_SC)

# ---- LB ----

dataLB <- f[which(f$EspecieObj == "TERAX_m"), ]# All fields, select LB column

LB_present <- dataLB[which(dataLB$Contatge > 0), ] # Only LB fields where LB is present


ph_LB <- (length(which(LB_present$Tractament == "Picar i herbicidar"))/length(which(dataLB$Tractament == "Picar i herbicidar")))*100
p_LB <- (length(which(LB_present$Tractament == "Picar"))/length(which(dataLB$Tractament == "Picar")))*100
ll_LB <- (length(which(LB_present$Tractament == "Llaurar"))/length(which(dataLB$Tractament == "Llaurar")))*100
a_LB <- (length(which(LB_present$Tractament == "Alfals"))/length(which(dataLB$Tractament == "Alfals")))*100
c_LB <- (length(which(LB_present$Tractament == "Control"))/length(which(dataLB$Tractament == "Control")))*100


LB <- c(ph_LB, p_LB, ll_LB, a_LB, c_LB)

# ---- CL ----

dataCL <- f[which(f$EspecieObj == "MECAL"), ]# All fields, select CL column

CL_present <- dataCL[which(dataCL$Contatge > 0), ] # Only CL fields where CL is present

ph_CL <- (length(which(CL_present$Tractament == "Picar i herbicidar"))/length(which(dataCL$Tractament == "Picar i herbicidar")))*100
p_CL <- (length(which(CL_present$Tractament == "Picar"))/length(which(dataCL$Tractament == "Picar")))*100
ll_CL <- (length(which(CL_present$Tractament == "Llaurar"))/length(which(dataCL$Tractament == "Llaurar")))*100
a_CL <- (length(which(CL_present$Tractament == "Alfals"))/length(which(dataCL$Tractament == "Alfals")))*100
c_CL <- (length(which(CL_present$Tractament == "Control"))/length(which(dataCL$Tractament == "Control")))*100


CL <- c(ph_CL, p_CL, ll_CL, a_CL, c_CL)

# ---- Join treatments ----

treatment <- c("S+H", "S", "T", "A", "C")

summary <- data.frame (treatment, SC, LB, CL)

setwd("~/First chapter/RESULTS")
write.csv(summary, "summary_presence.csv")




# % Of each species in each treatment (n presences in the treatment / n of total presences)
# (Distribution of species within the treatments)

rm(list=ls())
setwd("~/Datos/Datos barbechos arrendados/Variables")

f <- read.csv("Data_path_manuscript.csv", sep = ",", header=TRUE, fill = TRUE)
colnames(f)[4] <- "EspecieObj"


# ---- SC ----

dataSC <- f[which(f$EspecieObj == "BUOED"), ]# All fields, select SC column
SC_present <- dataSC[which(dataSC$Contatge > 0), ] # Only SC fields where SC is present


ph_SC <- (length(which(SC_present$Tractament == "Picar i herbicidar"))/nrow(SC_present))*100
p_SC <- (length(which(SC_present$Tractament == "Picar"))/nrow(SC_present))*100
ll_SC <- (length(which(SC_present$Tractament == "Llaurar"))/nrow(SC_present))*100
a_SC <- (length(which(SC_present$Tractament == "Alfals"))/nrow(SC_present))*100
c_SC <- (length(which(SC_present$Tractament == "Control"))/nrow(SC_present))*100

SC <- c(ph_SC, p_SC, ll_SC, a_SC, c_SC)

# ---- LB ----

dataLB <- f[which(f$EspecieObj == "TERAX_m"), ]# All fields, select LB column

LB_present <- dataLB[which(dataLB$Contatge > 0), ] # Only LB fields where LB is present


ph_LB <- (length(which(LB_present$Tractament == "Picar i herbicidar"))/nrow(LB_present))*100
p_LB <- (length(which(LB_present$Tractament == "Picar"))/nrow(LB_present))*100
ll_LB <- (length(which(LB_present$Tractament == "Llaurar"))/nrow(LB_present))*100
a_LB <- (length(which(LB_present$Tractament == "Alfals"))/nrow(LB_present))*100
c_LB <- (length(which(LB_present$Tractament == "Control"))/nrow(LB_present))*100

LB <- c(ph_LB, p_LB, ll_LB, a_LB, c_LB)

# ---- CL ----

dataCL <- f[which(f$EspecieObj == "MECAL"), ]# All fields, select CL column

CL_present <- dataCL[which(dataCL$Contatge > 0), ] # Only CL fields where CL is present

ph_CL <- (length(which(CL_present$Tractament == "Picar i herbicidar"))/nrow(CL_present))*100
p_CL <- (length(which(CL_present$Tractament == "Picar"))/nrow(CL_present))*100
ll_CL <- (length(which(CL_present$Tractament == "Llaurar"))/nrow(CL_present))*100
a_CL <- (length(which(CL_present$Tractament == "Alfals"))/nrow(CL_present))*100
c_CL <- (length(which(CL_present$Tractament == "Control"))/nrow(CL_present))*100

CL <- c(ph_CL, p_CL, ll_CL, a_CL, c_CL)

# ---- Join treatments ----

treatment <- c("S+H", "S", "T", "A", "C")

summary <- data.frame (treatment, SC, LB, CL)

setwd("~/First chapter/RESULTS")
write.csv(summary, "summary_presence.csv")
