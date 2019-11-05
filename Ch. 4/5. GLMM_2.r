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
  labs(x = " Distance to trails (m) ", y = "Prob. selección") +
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
  labs(x = " Distance to roads (m) ", y = "Prob. selección") +
  ylim(0,1) + 
  theme( 
    #panel.background = element_blank(),
    #axis.line = element_line(linetype = 1 ),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))

# 3. Pendiente

p1_pend <- ggplot(data_p1, aes(x = pendiente, y = pred$fit)) +
  #geom_point() +
  #geom_line() +
  geom_smooth(data = data_p1, aes(x = pendiente, y = used),
              method = "glm", method.args = list(family = "binomial")) + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = " Pendiente (%) ", y = "Prob. selección") +
  ylim(0,1) + 
  xlim(0,10) + 
  theme( 
    #panel.background = element_blank(),
    #axis.line = element_line(linetype = 1 ),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))

# 4. Pastos

p1_past <- ggplot(data_p1, aes(x = pastos, y = pred$fit)) +
  #geom_point() +
  #geom_line() +
  geom_smooth(data = data_p1, aes(x = pastos, y = used),
              method = "glm", method.args = list(family = "binomial")) + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = " Pastos ", y = "Prob. selección") +
  ylim(0,1) + 
  #xlim(0,10) + 
  theme( 
    #panel.background = element_blank(),
    #axis.line = element_line(linetype = 1 ),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))

# 5. Cereal

p1_cereal <- ggplot(data_p1, aes(x = cereal, y = pred$fit)) +
  #geom_point() +
  #geom_line() +
  geom_smooth(data = data_p1, aes(x = cereal, y = used),
              method = "glm", method.args = list(family = "binomial")) + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = " Cereal ", y = "Prob. selección") +
  ylim(0,1) + 
  #xlim(0,10) + 
  theme( 
    #panel.background = element_blank(),
    #axis.line = element_line(linetype = 1 ),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))

# 6. Barbecho

p1_barbecho <- ggplot(data_p1, aes(x = barbecho, y = pred$fit)) +
  #geom_point() +
  #geom_line() +
  geom_smooth(data = data_p1, aes(x = barbecho, y = used),
              method = "glm", method.args = list(family = "binomial")) + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = " Barbecho ", y = "Prob. selección") +
  ylim(0,1) + 
  #xlim(0,10) + 
  theme( 
    #panel.background = element_blank(),
    #axis.line = element_line(linetype = 1 ),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))


###################################### THEORY: BARBECHO #################################################################
# Create new data frame to predict values. I will predict over the real values (not scaled)
# I don't want to unscale the coefficients because I want to be able to compare between them
# Remove the extra variables, forestal, cereal (because it is what I want to predict), herb secano, frut regadio and scaled variables
# Remove barbecho instead of pendiente (14)
newdata_bar <- as.data.frame(lapply(lapply(data_p1[, -c(1:7,12,14,15,17,19:29)], mean), rep, 100)) 
newdata_bar$barbecho <- seq(min(data_p1$barbecho), max(data_p1$barbecho), length.out = 100) 
newdata_bar <- newdata_bar[ ,c(1:5,8,6:7)]

colnames(newdata_bar) <- paste(colnames(newdata_bar), "_sc", sep = "")

# Predict response (probability of selección)

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
  labs(x = "Barbecho", y = "Prob. selección") +
  ylim(0,1)
#Op2
ggplot(newdata_bar, aes(x = newdata_bar$barbecho_sc, y = pred$fit)) +
  geom_point() +
  #geom_line() +
  geom_smooth(data = data_p1, aes(x = barbecho, y = used),
              method = "glm", method.args = list(family = "binomial"))  + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = "Barbecho", y = "Prob. selección") +
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
  labs(x = "Barbecho", y = "Prob. selección")

ggplot(data_p1, aes(x = barbecho, y = pred$fit)) +
  geom_point() +
  #geom_line() +
  geom_smooth(data = data_p1, aes(x = barbecho, y = used),
              method = "glm", method.args = list(family = "binomial")) + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = "Barbecho", y = "Prob. selección")

# En estos dos casos la gráfica es igual, porque es con los datos del modelo (que solo tiene 0 y 1), por lo que en ninguna sube progresivamente
# LA MEJOR ES ESTA ÚLTIMA
##############################################


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
models_p2 <- dredge(p2, beta = "none", evaluate = TRUE ) # Solo hay un top model (full model), escoger este y no hacer model avg.

#setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/Data/Results")
#save(models_p2, file = "2.dredge_p2.RData")

setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/Data/Results")
load("2.dredge_p2.RData")


# ---- Predict ----

pred <- predict(p2, type = "response" )

# 1. Dist_caminos

data_p2$dist_caminos_m <- exp(data_p2$dist_caminos)

p2_distcam <- ggplot(data_p2, aes(x = dist_caminos_m, y = pred$fit)) +
  #geom_point() +
  #geom_line() +
  geom_smooth(data = data_p2, aes(x = dist_caminos_m, y = used),
              method = "glm", method.args = list(family = "binomial")) + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = " Distance to trails (m) ", y = "Prob. selección") +
  ylim(0,1) + 
  theme( 
    #panel.background = element_blank(),
    #axis.line = element_line(linetype = 1 ),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))

# 2. Dist_carreteras

data_p2$dist_carreteras_m <- exp(data_p2$dist_carreteras)

p2_distasp <- ggplot(data_p2, aes(x = dist_carreteras_m, y = pred$fit)) +
  #geom_point() +
  #geom_line() +
  geom_smooth(data = data_p2, aes(x = dist_carreteras_m, y = used),
              method = "glm", method.args = list(family = "binomial")) + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = " Distance to roads (m) ", y = "Prob. selección") +
  ylim(0,1) + 
  theme( 
    #panel.background = element_blank(),
    #axis.line = element_line(linetype = 1 ),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))

# 3. Pendiente

p2_pend <- ggplot(data_p2, aes(x = pendiente, y = pred$fit)) +
  #geom_point() +
  #geom_line() +
  geom_smooth(data = data_p2, aes(x = pendiente, y = used),
              method = "glm", method.args = list(family = "binomial")) + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = " Pendiente (%) ", y = "Prob. selección") +
  ylim(0,1) + 
  xlim(0,10) + 
  theme( 
    #panel.background = element_blank(),
    #axis.line = element_line(linetype = 1 ),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))

# 4. Pastos

p2_past <- ggplot(data_p2, aes(x = pastos, y = pred$fit)) +
  #geom_point() +
  #geom_line() +
  geom_smooth(data = data_p2, aes(x = pastos, y = used),
              method = "glm", method.args = list(family = "binomial")) + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = " Pastos ", y = "Prob. selección") +
  ylim(0,1) + 
  #xlim(0,10) + 
  theme( 
    #panel.background = element_blank(),
    #axis.line = element_line(linetype = 1 ),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))

# 5. Cereal

p2_cereal <- ggplot(data_p2, aes(x = cereal, y = pred$fit)) +
  #geom_point() +
  #geom_line() +
  geom_smooth(data = data_p2, aes(x = cereal, y = used),
              method = "glm", method.args = list(family = "binomial")) + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = " Cereal ", y = "Prob. selección") +
  ylim(0,1) + 
  #xlim(0,10) + 
  theme( 
    #panel.background = element_blank(),
    #axis.line = element_line(linetype = 1 ),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))

# 6. Barbecho

p2_barbecho <- ggplot(data_p2, aes(x = barbecho, y = pred$fit)) +
  #geom_point() +
  #geom_line() +
  geom_smooth(data = data_p2, aes(x = barbecho, y = used),
              method = "glm", method.args = list(family = "binomial")) + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = " Barbecho ", y = "Prob. selección") +
  ylim(0,1) + 
  #xlim(0,10) + 
  theme( 
    #panel.background = element_blank(),
    #axis.line = element_line(linetype = 1 ),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))


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
models_p3 <- dredge(p3, beta = "none", evaluate = TRUE ) # Solo hay un top model (full model), escoger este y no hacer model avg.

#setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/Data/Results")
#save(models_p3, file = "2.dredge_p3.RData")

setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/Data/Results")
load("2.dredge_p3.RData")

# ---- Predict ----

pred <- predict(p3, type = "response" )

# 1. Dist_caminos

data_p3$dist_caminos_m <- exp(data_p3$dist_caminos)

p3_distcam <- ggplot(data_p3, aes(x = dist_caminos_m, y = pred$fit)) +
  #geom_point() +
  #geom_line() +
  geom_smooth(data = data_p3, aes(x = dist_caminos_m, y = used),
              method = "glm", method.args = list(family = "binomial")) + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = " Distance to trails (m) ", y = "Prob. selección") +
  ylim(0,1) + 
  theme( 
    #panel.background = element_blank(),
    #axis.line = element_line(linetype = 1 ),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))

# 2. Dist_carreteras

data_p3$dist_carreteras_m <- exp(data_p3$dist_carreteras)

p3_distasp <- ggplot(data_p3, aes(x = dist_carreteras_m, y = pred$fit)) +
  #geom_point() +
  #geom_line() +
  geom_smooth(data = data_p3, aes(x = dist_carreteras_m, y = used),
              method = "glm", method.args = list(family = "binomial")) + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = " Distance to roads (m) ", y = "Prob. selección") +
  ylim(0,1) + 
  theme( 
    #panel.background = element_blank(),
    #axis.line = element_line(linetype = 1 ),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))

# 3. Pendiente

p3_pend <- ggplot(data_p3, aes(x = pendiente, y = pred$fit)) +
  #geom_point() +
  #geom_line() +
  geom_smooth(data = data_p3, aes(x = pendiente, y = used),
              method = "glm", method.args = list(family = "binomial")) + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = " Pendiente (%) ", y = "Prob. selección") +
  ylim(0,1) + 
  xlim(0,10) + 
  theme( 
    #panel.background = element_blank(),
    #axis.line = element_line(linetype = 1 ),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))

# 4. Pastos

p3_past <- ggplot(data_p3, aes(x = pastos, y = pred$fit)) +
  #geom_point() +
  #geom_line() +
  geom_smooth(data = data_p3, aes(x = pastos, y = used),
              method = "glm", method.args = list(family = "binomial")) + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = " Pastos ", y = "Prob. selección") +
  ylim(0,1) + 
  #xlim(0,10) + 
  theme( 
    #panel.background = element_blank(),
    #axis.line = element_line(linetype = 1 ),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))

# 5. Cereal

p3_cereal <- ggplot(data_p3, aes(x = cereal, y = pred$fit)) +
  #geom_point() +
  #geom_line() +
  geom_smooth(data = data_p3, aes(x = cereal, y = used),
              method = "glm", method.args = list(family = "binomial")) + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = " Cereal ", y = "Prob. selección") +
  ylim(0,1) + 
  #xlim(0,10) + 
  theme( 
    #panel.background = element_blank(),
    #axis.line = element_line(linetype = 1 ),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))

# 6. Barbecho

p3_barbecho <- ggplot(data_p3, aes(x = barbecho, y = pred$fit)) +
  #geom_point() +
  #geom_line() +
  geom_smooth(data = data_p3, aes(x = barbecho, y = used),
              method = "glm", method.args = list(family = "binomial")) + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = " Barbecho ", y = "Prob. selección") +
  ylim(0,1) + 
  #xlim(0,10) + 
  theme( 
    #panel.background = element_blank(),
    #axis.line = element_line(linetype = 1 ),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))





