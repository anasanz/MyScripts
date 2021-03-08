#-----------------------------------------------------------------------------------------
#           CALCULAR % DE HABITAT USADO/EVITADO EN MCP DE CADA INDIVIDUO 
#                             POR PERIODO Y A?O
#                               ROCIO TARJUELO
#-----------------------------------------------------------------------------------------

# LIBRARIES-------------------------------------------------------------------------------

# FUNCTIONS-------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------
#         START ANALYSIS
#-----------------------------------------------------------------------------------------

library(dplyr)

# Load data-------------------------------------------------------------------------------
mcp.hab <- read.table("D:/PhD/Fourth chapter/Results/Analisis3_bottleneck_effect/MCP_indiv_hab_avai.txt", header = T, dec = ",",
                           sep = "\t")

all_data <- as.data.frame(matrix(nrow = length(unique(mcp.hab$Bird.ID)), ncol = 6))
rownames(all_data) <- unique(mcp.hab$Bird.ID)
colnames(all_data) <- c("PreBot_used", "PreBot_avoided", "Bot_used", "Bot_avoided", "PostBot_used", "PostBot_avoided")

# Periodo pre
# Habitat seleccionado son fallow, cereal, natveg (column 5, 6, 12)
# H?bitat evitado son olive, fruiirri, herbirri, forest (column 7, 8, 9, 11)
mcp.hab.pre <- subset(mcp.hab, Period%in%"Pre")
mcp.hab.pre$usado <- apply(mcp.hab.pre[, c(5, 6, 12)], 1, sum, na.rm = T)
mean.hab.usado <- tapply(mcp.hab.pre$usado, mcp.hab.pre$Year, mean, na.rm = T)
sd.hab.usado <- tapply(mcp.hab.pre$usado, mcp.hab.pre$Year, sd, na.rm = T)

mcp.hab.pre$evitado <- apply(mcp.hab.pre[, c(7:9, 11)], 1, sum, na.rm = T)
mean.hab.evitado.pre <- tapply(mcp.hab.pre$evitado, mcp.hab.pre$Year, mean, na.rm = T)
sd.hab.evitado.pre <- tapply(mcp.hab.pre$evitado, mcp.hab.pre$Year, sd, na.rm = T)

# ASP: Un valor por individuo (media distintos años)
pre <- mcp.hab.pre %>%
  group_by(Bird.ID) %>%
  summarise(
    used = mean(usado, na.rm = TRUE),
    avoided = mean(evitado))
pre <- as.data.frame(pre)

# Fill in overall data frame
pre_id <- unique(pre$Bird.ID)
for (i in 1:length(pre_id)){
  all_data[rownames(all_data) %in% pre_id[i], c(1:2)] <-  pre[pre$Bird.ID %in% pre_id[i], 2:3]
}

# Periodo prerep
# Habitat seleccionado son fallow, natveg (column 5, 12)
# H?bitat evitado son cereal, olive, fruiirri, (column 6, 7, 9)
mcp.hab.prerep <- subset(mcp.hab, Period%in%"PreRep")
mcp.hab.prerep$usado <- apply(mcp.hab.prerep[, c(5, 12)], 1, sum, na.rm = T)
mean.hab.usado.prerep <- tapply(mcp.hab.prerep$usado, mcp.hab.prerep$Year, mean, na.rm = T)
sd.hab.usado.prerep <- tapply(mcp.hab.prerep$usado, mcp.hab.prerep$Year, sd, na.rm = T)

mcp.hab.prerep$evitado <- apply(mcp.hab.prerep[, c(6, 7, 9)], 1, sum, na.rm = T)
mean.hab.evitado.prerep <- tapply(mcp.hab.prerep$evitado, mcp.hab.prerep$Year, mean, na.rm = T)
sd.hab.evitado.prerep <- tapply(mcp.hab.prerep$evitado, mcp.hab.prerep$Year, sd, na.rm = T)

# ASP: Un valor por individuo (media distintos años)
prerep <- mcp.hab.prerep %>%
  group_by(Bird.ID) %>%
  summarise(
    used = mean(usado, na.rm = TRUE),
    avoided = mean(evitado))
prerep <- as.data.frame(prerep)

# Fill in overall data frame
prerep_id <- unique(prerep$Bird.ID)
for (i in 1:length(prerep_id)){
  all_data[rownames(all_data) %in% prerep_id[i], c(3:4)] <-  prerep[prerep$Bird.ID %in% prerep_id[i], 2:3]
}


# Periodo Rep
# Habitat seleccionado son fallow, olive, almond, natveg (column 5, 12)
# H?bitat evitado ninguno
mcp.hab.rep <- subset(mcp.hab, Period%in%"Rep")
mcp.hab.rep$usado <- apply(mcp.hab.rep[, c(4, 5, 9, 12)], 1, sum, na.rm = T)
mean.hab.usado.rep <- tapply(mcp.hab.rep$usado, mcp.hab.rep$Year, mean, na.rm = T)
sd.hab.usado.rep <- tapply(mcp.hab.rep$usado, mcp.hab.rep$Year, sd, na.rm = T)

# ASP: Un valor por individuo (media distintos años)
rep <- mcp.hab.rep %>%
  group_by(Bird.ID) %>%
  summarise(
    used = mean(usado, na.rm = TRUE))
rep <- as.data.frame(rep)
rep$avoided <- 0

# Fill in overall data frame
rep_id <- unique(rep$Bird.ID)
for (i in 1:length(rep_id)){
  all_data[rownames(all_data) %in% prerep_id[i], c(5:6)] <-  rep[rep$Bird.ID %in% rep_id[i], 2:3]
}

## ---- Add Mean and SD ----
all_data <- data.frame(round(as.matrix(all_data),2))

all_data[nrow(all_data) + 1, ] <- round(apply(all_data,2,mean,na.rm = TRUE),2)
rownames(all_data)[13] <- "mean"

all_data[nrow(all_data) + 1, ] <- round(apply(all_data,2,sd,na.rm = TRUE),2)
rownames(all_data)[14] <- "sd"

all_data[is.na(all_data)] <- "-"

all_data[nrow(all_data) + 1, ] <- paste(all_data[13, ],"(",all_data[14, ],")", sep = "")
rownames(all_data)[15] <- "Mean(SD)"


setwd("D:/PhD/Fourth chapter/Results/Figures_Tables")
write.csv(all_data, file = "MCP_selected_avoided.csv")


## ---- Table to add to this table ----

# With % of each use in each period

mcp.hab <- read.table("D:/PhD/Fourth chapter/Results/Analisis3_bottleneck_effect/MCP_indiv_hab_avai.txt", header = T, dec = ",",
                      sep = "\t")


# Periodo pre
# Habitat seleccionado son fallow, cereal, natveg (column 5, 6, 12)
# H?bitat evitado son olive, fruiirri, herbirri, forest (column 7, 8, 9, 11)

all_data_pre <- as.data.frame(matrix(nrow = length(unique(mcp.hab$Bird.ID)), ncol = 11))
rownames(all_data_pre) <- unique(mcp.hab$Bird.ID)
colnames(all_data_pre) <- c("Almond", "Fallow", "Cereal", "Fuit.irri", "Herb.irri", "Olive", "Herb.sec", "Forest","NatVeg","usado","evitado")

mcp.hab.pre <- subset(mcp.hab, Period%in%"Pre")
mcp.hab.pre$usado <- apply(mcp.hab.pre[, c(5, 6, 12)], 1, sum, na.rm = T)
mean.hab.usado <- tapply(mcp.hab.pre$usado, mcp.hab.pre$Year, mean, na.rm = T)
sd.hab.usado <- tapply(mcp.hab.pre$usado, mcp.hab.pre$Year, sd, na.rm = T)

mcp.hab.pre$evitado <- apply(mcp.hab.pre[, c(7:9, 11)], 1, sum, na.rm = T)
mean.hab.evitado.pre <- tapply(mcp.hab.pre$evitado, mcp.hab.pre$Year, mean, na.rm = T)
sd.hab.evitado.pre <- tapply(mcp.hab.pre$evitado, mcp.hab.pre$Year, sd, na.rm = T)

pre_each_use <- mcp.hab.pre %>%
  group_by(Bird.ID) %>%
  summarise(
    Almond = mean(ALMENDRO, na.rm = TRUE),
    Fallow = mean(BARBECHO, na.rm = TRUE),
    Cereal = mean(CEREAL, na.rm = TRUE),
    Fuit.irri = mean(FRUTALES.DE.REGADIO, na.rm = TRUE),
    Herb.irri = mean(HERBACEOS.DE.REGADIO, na.rm = TRUE),
    Olive = mean(OLIVO, na.rm = TRUE),
    Herb.sec = mean(OTROS.HERBACEOS.DE.SECANO, na.rm = TRUE),
    Forest = mean(FORESTAL, na.rm = TRUE),
    NatVeg = mean(PASTOS, na.rm = TRUE),    
    used = mean(usado, na.rm = TRUE),
    avoided = mean(evitado, na.rm = TRUE)
    )
pre_each_use <- as.data.frame(pre_each_use)

# Fill in overall data frame
pre_id <- unique(pre_each_use$Bird.ID)
for (i in 1:length(pre_id)){
  all_data_pre[rownames(all_data_pre) %in% pre_id[i], ] <-  pre_each_use[pre_each_use$Bird.ID %in% pre_id[i], 2:12]
}

all_data_pre <- round(all_data_pre,2)

# Add mean + sd per use

all_data_pre[nrow(all_data_pre) + 1, ] <- round(apply(all_data_pre,2,mean,na.rm = TRUE),2)
rownames(all_data_pre)[13] <- "mean"

all_data_pre[nrow(all_data_pre) + 1, ] <- round(apply(all_data_pre,2,sd,na.rm = TRUE),2)
rownames(all_data_pre)[14] <- "sd"

all_data_pre[is.na(all_data_pre)] <- "-"

all_data_pre[nrow(all_data_pre) + 1, ] <- paste(all_data_pre[13, ],"(",all_data_pre[14, ],")", sep = "")
rownames(all_data_pre)[15] <- "Mean(SD)"

###############################################################

# Periodo prerep
# Habitat seleccionado son fallow, natveg (column 5, 12)
# H?bitat evitado son cereal, olive, fruiirri, (column 6, 7, 9)

all_data_prerep <- as.data.frame(matrix(nrow = length(unique(mcp.hab$Bird.ID)), ncol = 11))
rownames(all_data_prerep) <- unique(mcp.hab$Bird.ID)
colnames(all_data_prerep) <- c("Almond", "Fallow", "Cereal", "Fuit.irri", "Herb.irri", "Olive", "Herb.sec", "Forest","NatVeg","usado","evitado")

mcp.hab.prerep <- subset(mcp.hab, Period%in%"PreRep")
mcp.hab.prerep$usado <- apply(mcp.hab.prerep[, c(5, 12)], 1, sum, na.rm = T)
mean.hab.usado.prerep <- tapply(mcp.hab.prerep$usado, mcp.hab.prerep$Year, mean, na.rm = T)
sd.hab.usado.prerep <- tapply(mcp.hab.prerep$usado, mcp.hab.prerep$Year, sd, na.rm = T)

mcp.hab.prerep$evitado <- apply(mcp.hab.prerep[, c(6, 7, 9)], 1, sum, na.rm = T)
mean.hab.evitado.prerep <- tapply(mcp.hab.prerep$evitado, mcp.hab.prerep$Year, mean, na.rm = T)
sd.hab.evitado.prerep <- tapply(mcp.hab.prerep$evitado, mcp.hab.prerep$Year, sd, na.rm = T)

prerep_each_use <- mcp.hab.prerep %>%
  group_by(Bird.ID) %>%
  summarise(
    Almond = mean(ALMENDRO, na.rm = TRUE),
    Fallow = mean(BARBECHO, na.rm = TRUE),
    Cereal = mean(CEREAL, na.rm = TRUE),
    Fuit.irri = mean(FRUTALES.DE.REGADIO, na.rm = TRUE),
    Herb.irri = mean(HERBACEOS.DE.REGADIO, na.rm = TRUE),
    Olive = mean(OLIVO, na.rm = TRUE),
    Herb.sec = mean(OTROS.HERBACEOS.DE.SECANO, na.rm = TRUE),
    Forest = mean(FORESTAL, na.rm = TRUE),
    NatVeg = mean(PASTOS, na.rm = TRUE),    
    used = mean(usado, na.rm = TRUE),
    avoided = mean(evitado, na.rm = TRUE)
  )
prerep_each_use <- as.data.frame(prerep_each_use)

# Fill in overall data frame
prerep_id <- unique(prerep_each_use$Bird.ID)
for (i in 1:length(prerep_id)){
  all_data_prerep[rownames(all_data_prerep) %in% prerep_id[i], ] <-  prerep_each_use[prerep_each_use$Bird.ID %in% prerep_id[i], 2:12]
}

all_data_prerep <- round(all_data_prerep,2)

# Add mean + sd per use

all_data_prerep[nrow(all_data_prerep) + 1, ] <- round(apply(all_data_prerep,2,mean,na.rm = TRUE),2)
rownames(all_data_prerep)[13] <- "mean"

all_data_prerep[nrow(all_data_prerep) + 1, ] <- round(apply(all_data_prerep,2,sd,na.rm = TRUE),2)
rownames(all_data_prerep)[14] <- "sd"

all_data_prerep[is.na(all_data_prerep)] <- "-"

all_data_prerep[nrow(all_data_prerep) + 1, ] <- paste(all_data_prerep[13, ],"(",all_data_prerep[14, ],")", sep = "")
rownames(all_data_prerep)[15] <- "Mean(SD)"

############################################################

# Periodo Rep
# Habitat seleccionado son fallow, olive, almond, natveg (column 5, 12)
# H?bitat evitado ninguno

all_data_rep <- as.data.frame(matrix(nrow = length(unique(mcp.hab$Bird.ID)), ncol = 11))
rownames(all_data_rep) <- unique(mcp.hab$Bird.ID)
colnames(all_data_rep) <- c("Almond", "Fallow", "Cereal", "Fuit.irri", "Herb.irri", "Olive", "Herb.sec", "Forest","NatVeg","usado","evitado")

mcp.hab.rep <- subset(mcp.hab, Period%in%"Rep")
mcp.hab.rep$usado <- apply(mcp.hab.rep[, c(4, 5, 9, 12)], 1, sum, na.rm = T)
mean.hab.usado.rep <- tapply(mcp.hab.rep$usado, mcp.hab.rep$Year, mean, na.rm = T)
sd.hab.usado.rep <- tapply(mcp.hab.rep$usado, mcp.hab.rep$Year, sd, na.rm = T)

rep_each_use <- mcp.hab.rep %>%
  group_by(Bird.ID) %>%
  summarise(
    Almond = mean(ALMENDRO, na.rm = TRUE),
    Fallow = mean(BARBECHO, na.rm = TRUE),
    Cereal = mean(CEREAL, na.rm = TRUE),
    Fuit.irri = mean(FRUTALES.DE.REGADIO, na.rm = TRUE),
    Herb.irri = mean(HERBACEOS.DE.REGADIO, na.rm = TRUE),
    Olive = mean(OLIVO, na.rm = TRUE),
    Herb.sec = mean(OTROS.HERBACEOS.DE.SECANO, na.rm = TRUE),
    Forest = mean(FORESTAL, na.rm = TRUE),
    NatVeg = mean(PASTOS, na.rm = TRUE),    
    used = mean(usado, na.rm = TRUE)
  )
rep_each_use <- as.data.frame(rep_each_use)
rep_each_use$avoided <- 0

# Fill in overall data frame
rep_id <- unique(rep_each_use$Bird.ID)
for (i in 1:length(rep_id)){
  all_data_rep[rownames(all_data_rep) %in% rep_id[i], ] <-  rep_each_use[rep_each_use$Bird.ID %in% rep_id[i], 2:12]
}

all_data_rep <- round(all_data_rep,2)

# Add mean + sd per use

all_data_rep[nrow(all_data_rep) + 1, ] <- round(apply(all_data_rep,2,mean,na.rm = TRUE),2)
rownames(all_data_rep)[13] <- "mean"

all_data_rep[nrow(all_data_rep) + 1, ] <- round(apply(all_data_rep,2,sd,na.rm = TRUE),2)
rownames(all_data_rep)[14] <- "sd"

all_data_rep[is.na(all_data_rep)] <- "-"

all_data_rep[nrow(all_data_rep) + 1, ] <- paste(all_data_rep[13, ],"(",all_data_rep[14, ],")", sep = "")
rownames(all_data_rep)[15] <- "Mean(SD)"


##################################################################
# JOIN ALL

all <- cbind(all_data_pre, all_data_prerep,all_data_rep)

setwd("D:/PhD/Fourth chapter/Results/Figures_Tables")
write.csv(all, file = "MCP_selected_avoided_all_uses.csv")

