rm(list = ls())

library(lme4)
library(MuMIn)
library(ggeffects)
library(ggplot2)

# Run GLMM

setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/Data")
data <- read.csv ("covariates.csv", header = TRUE)

# Check correlation

data <- data[complete.cases(data), ] # I don't know but I delete because I don't have time

correlation <- cor(data[ ,c(8:21)])
correlation[which(correlation > 0.5 & correlation < 1)] # Correlated: Dist carreteras - Dist nucleos urbanos (0.6)
#             Frut secano - Olivo y almendro 
names(data) # Quitar: Dist nucleos urbanos, olivo y almendro
data <- data[ , -c(10, 19, 20)]

#### Period 1 ####

ID_p <- unique(data$ID_p)
data_p1 <- data[which(data$ID_p %in% ID_p[grep("p1", ID_p)]), ]

# Scale variables

data_p1$dist_caminos_sc <- as.numeric(scale(data_p1$dist_caminos, center = TRUE, scale = TRUE))
data_p1$dist_carreteras_sc <- as.numeric(scale(data_p1$dist_carreteras, center = TRUE, scale = TRUE))
data_p1$pendiente_sc <- as.numeric(scale(data_p1$pendiente, center = TRUE, scale = TRUE))
data_p1$pastos_sc <- as.numeric(scale(data_p1$pastos, center = TRUE, scale = TRUE))
data_p1$forestal_sc <- as.numeric(scale(data_p1$forestal, center = TRUE, scale = TRUE))
data_p1$cereal_sc <- as.numeric(scale(data_p1$cereal, center = TRUE, scale = TRUE))
data_p1$barbecho_sc <- as.numeric(scale(data_p1$barbecho, center = TRUE, scale = TRUE))
data_p1$herb_secano_sc <- as.numeric(scale(data_p1$herb_secano, center = TRUE, scale = TRUE))
data_p1$herb_regadio_sc <- as.numeric(scale(data_p1$herb_regadio, center = TRUE, scale = TRUE))
data_p1$frut_regadio_sc <- as.numeric(scale(data_p1$frut_regadio, center = TRUE, scale = TRUE))
data_p1$frut_secano_sc <- as.numeric(scale(data_p1$frut_secano, center = TRUE, scale = TRUE))

# Full model:

p1 <- glm(used ~ dist_caminos_sc + dist_carreteras_sc + pendiente_sc +      # Without forestal, herb_secano, frut regadio, ID: It doesn't converge
                pastos_sc + 
                #forestal + 
                cereal_sc + barbecho_sc + 
                #herb_secano + 
                herb_regadio_sc 
                #frut_regadio + 
                #frut_secano_sc 
              #+ (1|Logger_ID), 
              ,family = binomial (link = "logit"),
              data = data_p1)

summary(p1)

options(na.action = na.fail)
models_p1 <- dredge(p1, beta = "none", evaluate = TRUE ) # 2 Top models: MODEL AVERAGING

#setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/Data/Results")
#save(models_p1, file = "2.dredge_p1.RData")

topmodels_p1 <- get.models(models_p1,subset = delta < 2)
avg_p1 <- model.avg(topmodels_p1)

# ---- Predict ----

pred <- predict(avg_p1, type = "response", se.fit = TRUE )

# 1. Dist_caminos

data_p1$dist_caminos_m <- exp(data_p1$dist_caminos)

p1_distcam <- ggplot(data_p1, aes(x = dist_caminos_m, y = pred$fit)) +
  #geom_point() +
  #geom_line() +
  geom_smooth(data = data_p1, aes(x = dist_caminos_m, y = used),
              method = "glm", method.args = list(family = "binomial")) + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = " Distance to trails (m) ", y = "Prob. selection") +
  ylim(0,1) + 
  theme( 
    #panel.background = element_blank(),
         #axis.line = element_line(linetype = 1 ),
         axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))

# 2. Dist_carreteras

data_p1$dist_carreteras_m <- exp(data_p1$dist_carreteras)

p1_distasp <- ggplot(data_p1, aes(x = dist_carreteras_m, y = pred$fit)) +
  #geom_point() +
  #geom_line() +
  geom_smooth(data = data_p1, aes(x = dist_carreteras_m, y = used),
              method = "glm", method.args = list(family = "binomial")) + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = " Distance to roads (m) ", y = "Prob. selection") +
  ylim(0,1) + 
  theme( 
    #panel.background = element_blank(),
    #axis.line = element_line(linetype = 1 ),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))

# 3. Pendiente

data_p1$dist_carreteras_m <- exp(data_p1$dist_carreteras)

p1_pend <- ggplot(data_p1, aes(x = pendiente, y = pred$fit)) +
  #geom_point() +
  #geom_line() +
  geom_smooth(data = data_p1, aes(x = pendiente, y = used),
              method = "glm", method.args = list(family = "binomial")) + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = " Pendiente (%) ", y = "Prob. selection") +
  ylim(0,1) + 
  xlim(0,10) + 
  theme( 
    #panel.background = element_blank(),
    #axis.line = element_line(linetype = 1 ),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))

# 1. BARBECHO



ggplot(data_p1, aes(x = barbecho, y = pred$fit)) +
  #geom_point() +
  #geom_line() +
  geom_smooth(data = data_p1, aes(x = barbecho, y = used),
              method = "glm", method.args = list(family = "binomial")) + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = "Barbecho", y = "Prob. selection") +
  ylim(0,1) + 
  theme( panel.background = element_blank(),
         axis.line = element_line(linetype = 1 ))


###################################### THEORY #################################################################
# Create new data frame to predict values. I will predict over the real values (not scaled)
# I don't want to unscale the coefficients because I want to be able to compare between them
# Remove the extra variables, forestal, cereal (because it is what I want to predict), herb secano, frut regadio and scaled variables
# Remove barbecho instead of pendiente (14)
newdata_bar <- as.data.frame(lapply(lapply(data_p1[, -c(1:7,12,14,15,17,19:29)], mean), rep, 100)) 
newdata_bar$barbecho <- seq(min(data_p1$barbecho), max(data_p1$barbecho), length.out = 100) 
newdata_bar <- newdata_bar[ ,c(1:5,8,6:7)]

colnames(newdata_bar) <- paste(colnames(newdata_bar), "_sc", sep = "")

# Predict response (probability of selection)

pred <- predict(p1_3_1, newdata = newdata_bar, type = "response", se.fit = TRUE )

lcl <- pred$fit - 1.96*pred$se.fit
lch <- pred$fit + 1.96*pred$se.fit

plot(pred$fit ~ newdata_bar$barbecho_sc, ylim = c(-1, 1), main = "p1", type = "l", xlab = "% Barbecho", ylab = "Probabilidad de selección")
polygon( x = c(newdata_bar$barbecho_sc, rev(newdata_bar$barbecho_sc)),
         y = c(lcl, rev(lch)), 
         col = adjustcolor(c("grey"),alpha.f = 0.6),
         border = NA)
newx <- seq(min(newdata_bar$barbecho_sc),max(newdata_bar$barbecho_sc), length.out = 100)
lines(newx, lcl, col = "red")
lines(newx, lch, col = "red")

# Op1
ggplot(newdata_bar, aes(x = newdata_bar$barbecho_sc, y = pred$fit)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial")) + # Aqui la linea es la del glm sobre los valores predichos (no el modelo original)
  #facet_wrap(~Sector) +
  labs(x = "Barbecho", y = "Prob. selection") +
  ylim(0,1)
#Op2
ggplot(newdata_bar, aes(x = newdata_bar$barbecho_sc, y = pred$fit)) +
  geom_point() +
  #geom_line() +
  geom_smooth(data = data_p1, aes(x = barbecho, y = used),
              method = "glm", method.args = list(family = "binomial"))  + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = "Barbecho", y = "Prob. selection") +
  ylim(0,1)

# Por qué estas dos salen distintas??? Porque al crear una variable con valores del 0 al 1, la pendiente sube muy progresivamente
# En cambio, al usar los datos metidos, solo hay 0 o 1, y esto hace que suba rápidamente
# Esto se puede ver en la opción 2, donde los puntos son los valores predichos con una variable respuesta que sube progresivamente,
# y la linea azul es la predicción del modelo (en el cual sólo hay 0 y 1)


# Plot without new data (Aquí la gráfica sale más razonable)

pred <- predict(p1_3_1, type = "response", se.fit = TRUE )

plot(pred$fit ~ data_p1$barbecho, ylim = c(0, 2), main = "p1", xlab = "Pendiente", ylab = "Probabilidad de selección")

m <- ggpredict(p1_3_1, "barbecho_sc", type = "re")
plot(m)

ggplot(data_p1, aes(x = barbecho, y = pred$fit)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial")) + # Aqui la linea es la del glm sobre los valores predichos (no el modelo original)
  #facet_wrap(~Sector) +
  labs(x = "Barbecho", y = "Prob. selection")

ggplot(data_p1, aes(x = barbecho, y = pred$fit)) +
  geom_point() +
  #geom_line() +
  geom_smooth(data = data_p1, aes(x = barbecho, y = used),
              method = "glm", method.args = list(family = "binomial")) + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = "Barbecho", y = "Prob. selection")

# En estos dos casos la gráfica es igual, porque es con los datos del modelo (que solo tiene 0 y 1), por lo que en ninguna sube progresivamente
# LA MEJOR ES ESTA ÚLTIMA
##############################################



# 2. CEREAL
# Create new data frame to predict values. I will predict over the real values (not scaled)
# I don't want to unscale the coefficients because I want to be able to compare between them
# Remove the extra variables, forestal, cereal (because it is what I want to predict), herb secano, frut regadio and scaled variables
newdata <- as.data.frame(lapply(lapply(data_p1[, -c(1:7,12,13,15,17,19:29)], mean), rep, 100)) 
newdata$cereal <- seq(min(data_p1$cereal), max(data_p1$cereal), length.out = 100)
newdata <- newdata[ ,c(1:4,8,5:7)]

colnames(newdata) <- paste(colnames(newdata), "_sc", sep = "") # Change the column names so that it fits

# Predict response (probability of selection)

pred <- predict(p1_3_1, newdata = newdata, type = "response", se.fit = TRUE )

lcl <- pred$fit - 1.96*pred$se.fit
lch <- pred$fit + 1.96*pred$se.fit

plot(pred$fit ~ newdata$cereal_sc, ylim = c(-3, 3), main = "p1", type = "l")

polygon( x = c(newdata$cereal_sc, rev(newdata$cereal_sc)),
         y = c(lcl, rev(lch)), 
         col = adjustcolor(c("grey"),alpha.f = 0.6),
         border = NA)
newx <- seq(min(newdata$cereal_sc),max(newdata$cereal_sc), length.out = 100)
lines(newx, lcl, col = "red")
lines(newx, lch, col = "red")

m <- ggpredict(p1_3_1, "cereal_sc", type = "re")
plot(m)

# Without new data

pred <- predict(p1_3_1, type = "response", se.fit = TRUE )

lcl <- pred$fit - 1.96*pred$se.fit
lch <- pred$fit + 1.96*pred$se.fit

plot(pred$fit ~ data_p1$cereal, ylim = c(-3, 3), main = "p1")

#lines(newx, lcl, col = "red")
#lines(newx, lch, col = "red")

m <- ggpredict(p1_3_1, "cereal_sc", type = "re")
plot(m)

ggplot(data_p1, aes(x = cereal, y = pred$fit)) +
  geom_point() +
  #geom_line() +
  geom_smooth(data = data_p1, aes(x = cereal, y = used),
              method = "glm", method.args = list(family = "binomial")) + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = "Barbecho", y = "Prob. selection")


# 2. PENDIENTE
# Remove pendiente instead of cereal (10)
newdata_pend <- as.data.frame(lapply(lapply(data_p1[, -c(1:7,12,10,15,17,19:29)], mean), rep, 100)) 
newdata_pend$pendiente <- seq(min(data_p1$pendiente), 5, length.out = 100) # Cojo 5 de máximo para mejorar la gráfica
newdata_pend <- newdata_pend[ ,c(1:2,8,3:7)]

colnames(newdata_pend) <- paste(colnames(newdata_pend), "_sc", sep = "")

# Predict response (probability of selection)

pred <- predict(p1_3_1, newdata = newdata_pend, type = "response", se.fit = TRUE )

lcl <- pred$fit - 1.96*pred$se.fit
lch <- pred$fit + 1.96*pred$se.fit

plot(pred$fit ~ newdata_pend$pendiente_sc, ylim = c(-1, 2), main = "p1", type = "l", xlab = "Pendiente", ylab = "Probabilidad de selección")

polygon( x = c(newdata_pend$pendiente_sc, rev(newdata_pend$pendiente_sc)),
         y = c(lcl, rev(lch)), 
         col = adjustcolor(c("grey"),alpha.f = 0.6),
         border = NA)


newx <- seq(min(newdata_pend$pendiente_sc),5, length.out = 100)
lines(newx, lcl, col = "red")
lines(newx, lch, col = "red")

ggplot(newdata_pend, aes(x = newdata_pend$pendiente_sc, y = pred$fit)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial")) +
  #facet_wrap(~Sector) +
  labs(x = "Pendiente", y = "Prob. selection") 

ggplot(newdata_pend, aes(x = newdata_pend$pendiente_sc, y = pred$fit)) + # Los puntos son los valores predichos de y con los nuevos datos
  geom_point() +
  #geom_line() +
  geom_smooth(data = data_p1, aes(x = pendiente_sc, y = used),
                method = "glm", method.args = list(family = "binomial")) +
  #facet_wrap(~Sector) +
  labs(x = "Pendiente", y = "Prob. selection") 

# Plot without new data

pred <- predict(p1_3_1, type = "response", se.fit = TRUE )

plot(pred$fit ~ data_p1$pendiente, ylim = c(0, 2), main = "p1", xlab = "Pendiente", ylab = "Probabilidad de selección")

m <- ggpredict(p1_3_1, "pendiente_sc [all]", type = "fe")
plot(m)

ggplot(data_p1, aes(x = pendiente, y = pred$fit)) + # Los puntos son los valores predichos de y con la pendiente (los valores metidos en el modelo)
  geom_point() +
  #geom_line() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial")) + 
  #facet_wrap(~Sector) +
  labs(x = "Pendiente", y = "Prob. selection")

ggplot(data_p1, aes(x = pendiente, y = pred$fit)) + # Los puntos son los valores predichos de y con la pendiente (los valores metidos en el modelo)
  geom_point() +
  #geom_line() +
  geom_smooth(data = data_p1, aes(x = pendiente_sc, y = used),
              method = "glm", method.args = list(family = "binomial")) + # Linea con los valores de pendiente y la respuesta del modelo?
  #facet_wrap(~Sector) +
  labs(x = "Pendiente", y = "Prob. selection")


#### Period 2 ####

ID_p <- unique(data$ID_p)
data_p2 <- data[which(data$ID_p %in% ID_p[grep("p2", ID_p)]), ]


# Scale variables

data_p2$dist_caminos_sc <- as.numeric(scale(data_p2$dist_caminos, center = TRUE, scale = TRUE))
data_p2$dist_carreteras_sc <- as.numeric(scale(data_p2$dist_carreteras, center = TRUE, scale = TRUE))
data_p2$pendiente_sc <- as.numeric(scale(data_p2$pendiente, center = TRUE, scale = TRUE))
data_p2$pastos_sc <- as.numeric(scale(data_p2$pastos, center = TRUE, scale = TRUE))
data_p2$forestal_sc <- as.numeric(scale(data_p2$forestal, center = TRUE, scale = TRUE))
data_p2$cereal_sc <- as.numeric(scale(data_p2$cereal, center = TRUE, scale = TRUE))
data_p2$barbecho_sc <- as.numeric(scale(data_p2$barbecho, center = TRUE, scale = TRUE))
data_p2$herb_secano_sc <- as.numeric(scale(data_p2$herb_secano, center = TRUE, scale = TRUE))
data_p2$herb_regadio_sc <- as.numeric(scale(data_p2$herb_regadio, center = TRUE, scale = TRUE))
data_p2$frut_regadio_sc <- as.numeric(scale(data_p2$frut_regadio, center = TRUE, scale = TRUE))
data_p2$frut_secano_sc <- as.numeric(scale(data_p2$frut_secano, center = TRUE, scale = TRUE))

# Full model:

p2 <- glmer(used ~ dist_caminos_sc + dist_carreteras_sc + pendiente_sc +      # Without forestal, frut regadio and herb_secano: It CONVERGES
                pastos_sc + 
                #forestal + 
                cereal_sc + barbecho_sc + 
                #herb_secano_sc + 
                herb_regadio_sc + 
                #frut_regadio + 
                #frut_secano_sc + 
                (1|Logger_ID), 
              family = binomial (link = "logit"),
              data = data_p2)

summary(p2)

options(na.action = na.fail)
models_p2 <- dredge(p2, beta = "none", evaluate = TRUE )

#setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/Data/Results")
#save(models_p2, file = "2.dredge_p2.RData")

# ---- Predict ----

# 1. BARBECHO

pred <- predict(p2_3, type = "response" )

plot(pred$fit ~ data_p1$barbecho, ylim = c(0, 2), main = "p1", xlab = "Pendiente", ylab = "Probabilidad de selección")

m <- ggpredict(p1_3_1, "barbecho_sc", type = "re")
plot(m)

ggplot(data_p1, aes(x = barbecho, y = pred$fit)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial")) + # Aqui la linea es la del glm sobre los valores predichos (no el modelo original)
  #facet_wrap(~Sector) +
  labs(x = "Barbecho", y = "Prob. selection")

ggplot(data_p2, aes(x = barbecho, y = pred)) +
  geom_point() +
  #geom_line() +
  geom_smooth(data = data_p2, aes(x = barbecho, y = used),
              method = "glm", method.args = list(family = "binomial")) + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = "Barbecho", y = "Prob. selection")


#### Period 3 ####

data_p3 <- data[which(data$ID_p %in% ID_p[grep("p3", ID_p)]), ]

data_p3$dist_caminos_sc <- as.numeric(scale(data_p3$dist_caminos, center = TRUE, scale = TRUE))
data_p3$dist_carreteras_sc <- as.numeric(scale(data_p3$dist_carreteras, center = TRUE, scale = TRUE))
data_p3$pendiente_sc <- as.numeric(scale(data_p3$pendiente, center = TRUE, scale = TRUE))
data_p3$pastos_sc <- as.numeric(scale(data_p3$pastos, center = TRUE, scale = TRUE))
data_p3$forestal_sc <- as.numeric(scale(data_p3$forestal, center = TRUE, scale = TRUE))
data_p3$cereal_sc <- as.numeric(scale(data_p3$cereal, center = TRUE, scale = TRUE))
data_p3$barbecho_sc <- as.numeric(scale(data_p3$barbecho, center = TRUE, scale = TRUE))
data_p3$herb_secano_sc <- as.numeric(scale(data_p3$herb_secano, center = TRUE, scale = TRUE))
data_p3$herb_regadio_sc <- as.numeric(scale(data_p3$herb_regadio, center = TRUE, scale = TRUE))
data_p3$frut_regadio_sc <- as.numeric(scale(data_p3$frut_regadio, center = TRUE, scale = TRUE))
data_p3$frut_secano_sc <- as.numeric(scale(data_p3$frut_secano, center = TRUE, scale = TRUE))

p3 <- glmer(used ~ dist_caminos_sc + dist_carreteras_sc + pendiente_sc +      # Without forestal, frut regadio, frut secano and herb_secano
                pastos_sc + 
                #forestal + 
                cereal_sc + barbecho_sc + 
                #herb_secano + 
                herb_regadio_sc + 
                #frut_regadio + 
                #frut_secano_sc + 
                (1|Logger_ID), 
              family = binomial (link = "logit"),
              data = data_p3)

summary(p3)

options(na.action = na.fail)
models_p3 <- dredge(p3, beta = "none", evaluate = TRUE ) # Best model is the FULL model (NO model averaging)

#setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/Data/Results")
#save(models_p3, file = "2.dredge_p3.RData")

# ---- Predict ---- 





