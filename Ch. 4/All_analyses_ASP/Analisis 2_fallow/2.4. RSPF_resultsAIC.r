
## -------------------------------------------------
##        ANALISIS 2: RESULTS AIC
## ------------------------------------------------- 

rm(list=ls())

library(dplyr)

temp <- setwd("D:/PhD/Fourth chapter/Results/RSF_Ana/Analisis 2/results_prov_rspf/Analisis_AIC2")
temp <- list.files(pattern = "*.csv")
data <- lapply(temp, read.csv, sep = ",")


## ---- PERIOD 1 ----

file <- grep("_Pre_", temp) 
d <- data[file]
d_names <- temp[file]

st <- c("St1", "St2", "St3", "St4")

# Add column structure

for (i in 1:length(d)){
  d[[i]] <- cbind(d[[i]], St = st[[i]])
}

df <- do.call(rbind,d)
df_p1 <- df %>% 
  arrange(AIC)

df_p1$Period <- "p1"

## ---- PERIOD 2 ----

file <- grep("_PreRep_", temp) # Files with one type of availability sampling
d <- data[file]
d_names <- temp[file]

st <- c("St1", "St2", "St3", "St4")

# Add column structure

for (i in 1:length(d)){
  d[[i]] <- cbind(d[[i]], St = st[[i]])
}

df <- do.call(rbind,d)
df_p2 <- df %>% 
  arrange(AIC)

df_p2$Period <- "p2"

## ---- PERIOD 3 ----

file <- grep("_Rep_", temp) # Files with one type of availability sampling
d <- data[file]
d_names <- temp[file]

st <- c("St1", "St2", "St3", "St4")

# Add column structure

for (i in 1:length(d)){
  d[[i]] <- cbind(d[[i]], St = st[[i]])
}

df <- do.call(rbind,d)
df_p3 <- df %>% 
  arrange(AIC)

df_p3$Period <- "p3"

## -------------------------------------------------
##                    Tabla AIC
## ------------------------------------------------- 

df <- rbind(df_p1, df_p2, df_p3)

summary_AIC <- data.frame(df %>% group_by(Period, St) %>% summarise(AIC = mean(AIC)))
summary_AIC <- data.frame(summary_AIC %>% group_by(Period) %>% arrange(Period, AIC))

library(plyr)

summary_AIC <-  ddply(summary_AIC, .(Period), transform, deltaAIC = round(AIC-AIC[1], 2))

setwd("D:/PhD/Fourth chapter/Results/RSF_Ana/Analisis 2/results_prov_rspf/Analisis_AIC2")
write.csv(summary_AIC, "summary_AIC.csv")
write.csv(df, "coef_AIC.csv")


               