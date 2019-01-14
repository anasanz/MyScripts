

# SUMMARY RESULTS

# FOR THE ARTICLE, GOOD ONE: % Of each species in each treatment (weighted by treatment) 
#(presences in the treatment / num of fields of that treatment)
# Modified with the 7 fields removed when repeating the analysis in the 2nd submission


rm(list=ls())
setwd("C:/Users/ana.sanz/OneDrive/PhD/First chapter/Datos/Datos barbechos arrendados/Variables")

f <- read.csv("Data_path_manuscript2.csv", sep = ",", header=TRUE, fill = TRUE)

# ---- SC ----

dataSC <- f[which(f$Species == "SC"), ]# All fields, select SC column
SC_present <- dataSC[which(dataSC$Presence > 0), ] # Only SC fields where SC is present


ph_SC <- (length(which(SC_present$agri_practice == "S+H"))/length(which(dataSC$agri_practice == "S+H")))*100
p_SC <- (length(which(SC_present$agri_practice == "S"))/length(which(dataSC$agri_practice == "S")))*100
ll_SC <- (length(which(SC_present$agri_practice == "T"))/length(which(dataSC$agri_practice == "T")))*100
a_SC <- (length(which(SC_present$agri_practice == "A"))/length(which(dataSC$agri_practice == "A")))*100
c_SC <- (length(which(SC_present$agri_practice == "C"))/length(which(dataSC$agri_practice == "C")))*100

SC <- c(ph_SC, p_SC, ll_SC, a_SC, c_SC)

# ---- LB ----

dataLB <- f[which(f$Species == "LB"), ]# All fields, select LB column

LB_present <- dataLB[which(dataLB$Presence > 0), ] # Only LB fields where LB is present


ph_LB <- (length(which(LB_present$agri_practice == "S+H"))/length(which(dataLB$agri_practice == "S+H")))*100
p_LB <- (length(which(LB_present$agri_practice == "S"))/length(which(dataLB$agri_practice == "S")))*100
ll_LB <- (length(which(LB_present$agri_practice == "T"))/length(which(dataLB$agri_practice == "T")))*100
a_LB <- (length(which(LB_present$agri_practice == "A"))/length(which(dataLB$agri_practice == "A")))*100
c_LB <- (length(which(LB_present$agri_practice == "C"))/length(which(dataLB$agri_practice == "C")))*100


LB <- c(ph_LB, p_LB, ll_LB, a_LB, c_LB)

# ---- CL ----

dataCL <- f[which(f$Species == "CL"), ]# All fields, select CL column

CL_present <- dataCL[which(dataCL$Presence > 0), ] # Only CL fields where CL is present

ph_CL <- (length(which(CL_present$agri_practice == "S+H"))/length(which(dataCL$agri_practice == "S+H")))*100
p_CL <- (length(which(CL_present$agri_practice == "S"))/length(which(dataCL$agri_practice == "S")))*100
ll_CL <- (length(which(CL_present$agri_practice == "T"))/length(which(dataCL$agri_practice == "T")))*100
a_CL <- (length(which(CL_present$agri_practice == "A"))/length(which(dataCL$agri_practice == "A")))*100
c_CL <- (length(which(CL_present$agri_practice == "C"))/length(which(dataCL$agri_practice == "C")))*100


CL <- c(ph_CL, p_CL, ll_CL, a_CL, c_CL)

# ---- Join treatments ----

treatment <- c("S+H", "S", "T", "A", "C")

summary <- data.frame (treatment, SC, LB, CL)

setwd("C:/Users/ana.sanz/OneDrive/PhD/First chapter/RESULTS")
write.csv(summary, "summary_presence2.csv")



######################################## NOT ARTICLE, NOT GOOD ############################
# % Of each species in each treatment (n presences in the treatment / n of total presences)
# (Distribution of species within the treatments)

rm(list=ls())
setwd("~/Datos/Datos barbechos arrendados/Variables")

f <- read.csv("Data_path_manuscript.csv", sep = ",", header=TRUE, fill = TRUE)
colnames(f)[4] <- "Species"


# ---- SC ----

dataSC <- f[which(f$Species == "SC"), ]# All fields, select SC column
SC_present <- dataSC[which(dataSC$Presence > 0), ] # Only SC fields where SC is present


ph_SC <- (length(which(SC_present$agri_practice == "S+H"))/nrow(SC_present))*100
p_SC <- (length(which(SC_present$agri_practice == "S"))/nrow(SC_present))*100
ll_SC <- (length(which(SC_present$agri_practice == "T"))/nrow(SC_present))*100
a_SC <- (length(which(SC_present$agri_practice == "A"))/nrow(SC_present))*100
c_SC <- (length(which(SC_present$agri_practice == "C"))/nrow(SC_present))*100

SC <- c(ph_SC, p_SC, ll_SC, a_SC, c_SC)

# ---- LB ----

dataLB <- f[which(f$Species == "LB"), ]# All fields, select LB column

LB_present <- dataLB[which(dataLB$Presence > 0), ] # Only LB fields where LB is present


ph_LB <- (length(which(LB_present$agri_practice == "S+H"))/nrow(LB_present))*100
p_LB <- (length(which(LB_present$agri_practice == "S"))/nrow(LB_present))*100
ll_LB <- (length(which(LB_present$agri_practice == "T"))/nrow(LB_present))*100
a_LB <- (length(which(LB_present$agri_practice == "A"))/nrow(LB_present))*100
c_LB <- (length(which(LB_present$agri_practice == "C"))/nrow(LB_present))*100

LB <- c(ph_LB, p_LB, ll_LB, a_LB, c_LB)

# ---- CL ----

dataCL <- f[which(f$Species == "CL"), ]# All fields, select CL column

CL_present <- dataCL[which(dataCL$Presence > 0), ] # Only CL fields where CL is present

ph_CL <- (length(which(CL_present$agri_practice == "S+H"))/nrow(CL_present))*100
p_CL <- (length(which(CL_present$agri_practice == "S"))/nrow(CL_present))*100
ll_CL <- (length(which(CL_present$agri_practice == "T"))/nrow(CL_present))*100
a_CL <- (length(which(CL_present$agri_practice == "A"))/nrow(CL_present))*100
c_CL <- (length(which(CL_present$agri_practice == "C"))/nrow(CL_present))*100

CL <- c(ph_CL, p_CL, ll_CL, a_CL, c_CL)

# ---- Join treatments ----

treatment <- c("S+H", "S", "T", "A", "C")

summary <- data.frame (treatment, SC, LB, CL)

setwd("~/First chapter/RESULTS")
write.csv(summary, "summary_presence.csv")
