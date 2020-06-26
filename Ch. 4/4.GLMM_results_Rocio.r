
####### SCRIPT PARA ENSEÑAR PROBLEMAS CON LAS GRÁFICAS A ROCÍO #########
# Pongo como ejemplo
# el modelo del periodo 1, que es un glm porque sólo tenemos datos de dos individuos
# y no puedo meter el individuo como random intercept, y el modelo del periodo 2 
# que es un glmm (al tener 5 individuos si puede meterse el random effect)
# Al final del script te pongo una prueba que hago con otro paquete


rm(list = ls())

library(lme4)
library(MuMIn)
library(ggeffects)
library(ggplot2)

# Run models and plot predictions

setwd("D:/PhD/Fourth chapter/Congreso SEO/Data_SEO")
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

sum_p1 <- summary(p1)

options(na.action = na.fail)
models_p1 <- dredge(p1, beta = "none", evaluate = TRUE ) # 2 Top models: MODEL AVERAGING

#setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/Data/Results")
#save(models_p1, file = "2.dredge_p1.RData")

topmodels_p1 <- get.models(models_p1,subset = delta < 2)
avg_p1 <- model.avg(topmodels_p1)


# ---- Predict CON GGPLOT2 (Ejemplos de variables Distancia a caminos y pendiente) ----

pred <- predict(avg_p1, type = "response", se.fit = TRUE )

# Dist_caminos

data_p1$dist_caminos_m <- exp(data_p1$dist_caminos)

ggplot(data_p1, aes(x = dist_caminos_m, y = pred$fit)) +
  geom_point() +
  #geom_line() +
  geom_smooth(data = data_p1, aes(x = dist_caminos_m, y = used),
              method = "glm", method.args = list(family = "binomial"),
              color = "darkgreen", formula = y ~ x) + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = " Distancia a caminos (m) ", y = "Prob. selección") +
  ylim(0,1) + 
  theme( 
    #panel.background = element_blank(),
    #axis.line = element_line(linetype = 1 ),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size = 15),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), size = 15),
    axis.text = element_text(size = 15))

# Pendiente

ggplot(data_p1, aes(x = pendiente, y = pred$fit)) +
  geom_point() +
  #geom_line() +
  geom_smooth(data = data_p1, aes(x = pendiente, y = used),
              method = "glm", method.args = list(family = "binomial"), formula = y ~ x,
              color = "darkgreen") + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = " Pendiente ", y = "Prob. selección") +
  ylim(0,1) + 
  xlim(0,15) +
theme( 
  #panel.background = element_blank(),
  #axis.line = element_line(linetype = 1 ),
  axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size = 15),
  axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), size = 15),
  axis.text = element_text(size = 15))

# ---- Predict MANUALMENTE CON R (Ejemplo variable pendiente) ----
# El problema con los CI es el mismo para todos los modelos.
# Aquí los CI son muy amplios (así ves el problema que tengo), y también sirve para ver que la linea de 
# predicción no es lineal

newdata_pend<- as.data.frame(lapply(lapply(data_p1[, -c(1:18,21, 23, 26, 28, 29)], mean), rep, 25))
newdata_pend$pendiente_sc <- seq(min(data_p1$pendiente), max(data_p1$pendiente), length.out = 25)

pred <- predict(avg_p1, newdata = newdata_pend, type = "response", se.fit = TRUE )

lcl <- pred$fit - 1.96*pred$se.fit
lch <- pred$fit + 1.96*pred$se.fit

plot(pred$fit ~ newdata_pend$pendiente, ylim = c(0, 0.05), main = "p1", type = "l")
polygon( x = c(newdata_pend$pendiente, rev(newdata_pend$pendiente)),
         y = c(lcl, rev(lch)), 
         col = adjustcolor(c("grey"),alpha.f = 0.6),
         border = NA)

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

sum_p2 <- summary(p2)

#options(na.action = na.fail)
#models_p2 <- dredge(p2, beta = "none", evaluate = TRUE ) # Solo hay un top model (full model), escoger este y no hacer model avg.

# ---- Predict ----

pred <- predict(p2, type = "response" ) # Con random effects, no se pueden predecir los SE

# ---- Predict CON GGPLOT2 (Ejemplos de variables Distancia a caminos y pendiente) ----

# Dist caminos

data_p2$dist_caminos_m <- exp(data_p2$dist_caminos)

ggplot(data_p2, aes(x = dist_caminos_m, y = pred$fit)) +
  #geom_point() +
  #geom_line() +
  geom_smooth(data = data_p2, aes(x = dist_caminos_m, y = used),
              method = "glm", method.args = list(family = "binomial"),
              color = "darkgreen") + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = " Distancia a caminos (m) ", y = "Prob. selección") +
  ylim(0,1) + 
  theme( 
    #panel.background = element_blank(),
    #axis.line = element_line(linetype = 1 ),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size = 15),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), size = 15),
    axis.text = element_text(size = 15))

# Pendiente

ggplot(data_p2, aes(x = pendiente, y = pred$fit)) +
  #geom_point() +
  #geom_line() +
  geom_smooth(data = data_p2, aes(x = pendiente, y = used),
              method = "glm", method.args = list(family = "binomial"),
              color = "darkgreen") + # Aqui la linea es la del glm sobre la respuesta (y)
  #facet_wrap(~Sector) +
  labs(x = " Pendiente ", y = "Prob. selección") +
  ylim(0,1) + 
  xlim(0,15) +
theme( 
  #panel.background = element_blank(),
  #axis.line = element_line(linetype = 1 ),
  axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size = 15),
  axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), size = 15),
  axis.text = element_text(size = 15))

# ---- Predict MANUALMENTE CON R (Ejemplo variable pendiente) ----

newdata_pend<- as.data.frame(lapply(lapply(data_p2[, -c(1:18,21, 23, 26, 28, 29)], mean), rep, 25))
newdata_pend$pendiente_sc <- seq(min(data_p2$pendiente), max(data_p2$pendiente), length.out = 25)

pred <- predict(p2, newdata = newdata_pend, type = "response")

#lcl <- pred$fit - 1.96*pred$se.fit #No se puede con glmm
#lch <- pred$fit + 1.96*pred$se.fit

plot(pred$fit ~ newdata_pend$pendiente, ylim = c(0, 0.05), main = "p2", type = "l")
#polygon( x = c(newdata_pend$pendiente, rev(newdata_pend$pendiente)),
#         y = c(lcl, rev(lch)), 
#         col = adjustcolor(c("grey"),alpha.f = 0.6),
#         border = NA)

#### PRUEBA CON OTRO PAQUETE (que permite plotear random effects) ####
# Con otro paquete tengo los mismos problemas (grandes CI, y la linea no es recta)
library(ggeffects)
m1 <- ggpredict(p1, "pendiente_sc[all]", type = "fe")
plot(m1)

m2 <- ggpredict(p2, "pendiente_sc[all]", type = "fe")
plot(m2)

m2_cam <- ggpredict(p2, "dist_caminos_sc[all]", type = "fe")
plot(m2_cam)
