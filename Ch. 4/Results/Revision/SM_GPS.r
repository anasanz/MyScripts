

rm(list = ls())


## -------------------------------------------------
##                  SupMaterial
## ------------------------------------------------- 

library(tidyverse)

## -------------------------------------------------
##            GPS DATA - All (Si vale, con flying y NA)
## ------------------------------------------------- 

datos <- read.table("D:/PhD/Fourth chapter/GPS Cataluña/Ganga/Updated_24-2-2020/FINAL_ALLpos_no_regadio_ETRS89.txt", header = T, dec = ",",
                  sep = "\t")

periods <- c("Pre", "PreRep", "Rep")

locations_ID <- data.frame(matrix(0, ncol = length(periods), nrow = length(unique(datos$Logger_ID))))
colnames(locations_ID) <- periods
rownames(locations_ID) <- unique(datos$Logger_ID)

loc <- list()

for (i in 1:length(periods)) {
  
  datos.rspf <- subset(datos, period%in%periods[i])
  
  n.id <- table(list(datos.rspf[ , "Logger_ID"]))
  
  locations_ID[rownames(locations_ID) %in% names(n.id), colnames(locations_ID) %in% periods[i]] <- n.id
}

# Check start and end dates
##rev: convert to date to order it

datos$fecha <- as.Date(paste(datos$Day,datos$Month,datos$Year,sep = "/"), format = "%d/%m/%Y")
datos$hora <- as.POSIXlt(paste(datos$Hour,datos$Minute,datos$Second,sep = ":"), format = "%H:%M:%OS")
datos$hora <- format(datos$hora,"%H:%M:%OS")

datos <- arrange(datos, fecha, hora)

d <- datos[which(datos$Logger_ID == "PIC17"), ]
d[1, 2:5]
d[nrow(d), 2:5]


setwd("D:/PhD/Fourth chapter/Results")
write.csv(locations_ID, file = "SM_GPS_info_ALL.csv")
sum(colSums(locations_ID))

## -------------------------------------------------
##        GPS DATA - Only used in RSPF (no vale)
## ------------------------------------------------- 

datos <- read.table("D:/PhD/Fourth chapter/Data/2_matrix_RSPF.txt", header = T, dec = ",")

periods <- c("Pre", "PreRep", "Rep")

locations_ID <- data.frame(matrix(0, ncol = length(periods), nrow = length(unique(datos$Logger_ID))))
colnames(locations_ID) <- periods
rownames(locations_ID) <- unique(datos$Logger_ID)

loc <- list()

for (i in 1:length(periods)) {
  
  datos.rspf <- subset(datos, periodo%in%periods[i])
  
  n.id <- table(list(datos.rspf[datos.rspf$STATUS == 1, "Logger_ID"]))
  
  locations_ID[rownames(locations_ID) %in% names(n.id), colnames(locations_ID) %in% periods[i]] <- n.id
}

setwd("D:/PhD/Fourth chapter/Results")
write.csv(locations_ID, file = "SM_GPS_info.csv")
sum(colSums(locations_ID))

# Check start and end dates

d <- datos[which(datos$Logger_ID == "GUE05" & datos$STATUS == 1), ]
d[1, 2:5]
d[nrow(d), 2:5]

d <- datos[which(datos$Logger_ID == "PIC17"& datos$STATUS == 1), ]
d[1, 2:5]
d[nrow(d), 2:5]


## -------------------------------------------------
##            HOME RANGE SIZES
## ------------------------------------------------- 

library(tidyr)
library(dplyr)

mcp.hab <- read.table("D:/PhD/Fourth chapter/Results/Analisis3_bottleneck_effect/MCP_indiv_hab_avai.txt", header = T, dec = ",",
                      sep = "\t")
mcp.hab$MCP.area_ha <- mcp.hab$MCP.area/10000

mcp_ID <- mcp.hab %>%
  group_by(Period, Bird.ID) %>%
  summarise(
    mean_ID = mean(MCP.area_ha, na.rm = TRUE))
mcp_ID <- as.data.frame(mcp_ID)

# Table
mcp_ID_wide <- spread(mcp_ID,Period, mean_ID)
mcp_ID_wide <- cbind(mcp_ID_wide$Bird.ID ,round(mcp_ID_wide[,c(2:4)],2))
colnames(mcp_ID_wide) <- c("Logger ID", "PreBottleneck", "Bottleneck", "PostBottleneck")
mcp_ID_wide$Total_mean <- apply(mcp_ID_wide[,2:4], 1, mean, na.rm = TRUE)

mean_periods <- mcp_ID_wide %>%
  summarise(PreBot = mean(PreBottleneck, na.rm = TRUE),
            Bot = mean(Bottleneck, na.rm = TRUE),
            PostBot = mean(PostBottleneck, na.rm = TRUE),
            Total = mean(Total, na.rm = TRUE))

sd_periods <- mcp_ID_wide %>%
  summarise(PreBot = sd(PreBottleneck, na.rm = TRUE),
            Bot = sd(Bottleneck, na.rm = TRUE),
            PostBot = sd(PostBottleneck, na.rm = TRUE),
            Total = sd(Total, na.rm = TRUE))

mcp_ID_wide[nrow(mcp_ID_wide) + 1,] <- c("Mean(sd)",paste(round(mean_periods,2),"(",round(sd_periods,2), ")", sep = ""))
mcp_ID_wide$sd <- c(apply(mcp_ID_wide[,2:4], 1, sd, na.rm = TRUE),NA)

mcp_ID_wide$Total_sd <- apply(mcp_ID_wide[,2:4], 1, sd, na.rm = TRUE)
mcp_ID_wide$Total <- paste(round(as.numeric(mcp_ID_wide$Total_mean),2),"(",round(as.numeric(mcp_ID_wide$Total_sd),2), ")", sep = "")
mcp_ID_wide <- mcp_ID_wide[,-c(5,6)]
                                          

setwd("D:/PhD/Fourth chapter/Results")
write.csv(mcp_ID_wide, file = "SM_HomeRange_info.csv")

## -------------------------------------------------
##                    Core area KDE sizes
## ------------------------------------------------- 


dat <- read.table("D:/PhD/Fourth chapter/Data/kernel_homerange_analyisis3/1_kernels_periodo_reddata.txt", header = T, dec = ",")

id <- data.frame(do.call('rbind', strsplit(as.character(dat$IND),'.',fixed=TRUE)))
kde <- cbind(dat,id)
colnames(kde)[c(8:10)] <- c("ID", "Period", "Year")
kde$Period <- as.factor(kde$Period)

# Transform to same units (The ones in ha to km2)

kde_units <- kde

kde_units[which(kde_units$unidades == "area (hectares)"), c(1,2,3)] <- kde_units[which(kde_units$unidades == "area (hectares)"), c(1,2,3)]/100
kde_units$unidades <- "area (square kilometers)"

kde50 <- kde_units[which(kde_units$vol == 0.50), ] # Area en km2

kde50$low <- kde50$low*100 # Transform to ha
kde50$est <- kde50$est*100
kde50$high <- kde50$high*100

kde_ID <- kde50 %>%
  group_by(Period, ID) %>%
  summarise(
    mean_ID = mean(est, na.rm = TRUE))

kde_ID <- as.data.frame(kde_ID)

# Table
kde_ID_wide <- spread(kde_ID,Period, mean_ID)
kde_ID_wide <- cbind(kde_ID_wide$ID ,round(kde_ID_wide[,c(2:4)],2))
colnames(kde_ID_wide) <- c("Logger ID", "PreBottleneck", "Bottleneck", "PostBottleneck")
kde_ID_wide$Total_mean <- apply(kde_ID_wide[,2:4], 1, mean, na.rm = TRUE)

mean_periods <- kde_ID_wide %>%
  summarise(PreBot = mean(PreBottleneck, na.rm = TRUE),
            Bot = mean(Bottleneck, na.rm = TRUE),
            PostBot = mean(PostBottleneck, na.rm = TRUE),
            Total = mean(Total_mean, na.rm = TRUE))

sd_periods <- kde_ID_wide %>%
  summarise(PreBot = sd(PreBottleneck, na.rm = TRUE),
            Bot = sd(Bottleneck, na.rm = TRUE),
            PostBot = sd(PostBottleneck, na.rm = TRUE),
            Total = sd(Total_mean, na.rm = TRUE))

kde_ID_wide[nrow(kde_ID_wide) + 1,] <- c("Mean(sd)",paste(round(mean_periods,2),"(",round(sd_periods,2), ")", sep = ""))

kde_ID_wide$Total_sd <- apply(kde_ID_wide[,2:4], 1, sd, na.rm = TRUE)
kde_ID_wide$Total <- paste(round(as.numeric(kde_ID_wide$Total_mean),2),"(",round(as.numeric(kde_ID_wide$Total_sd),2), ")", sep = "")
kde_ID_wide <- kde_ID_wide[,-c(5,6)]


setwd("D:/PhD/Fourth chapter/Results")
write.csv(kde_ID_wide, file = "SM_coreAreas_info.csv")
