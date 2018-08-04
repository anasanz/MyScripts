
#FIELD SCALE_PTEROCLIDOS

library(ggplot2)
library(pscl) 
library(MuMIn)

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")
f<-read.csv("Variables.csv",sep = ",",header=TRUE,fill = TRUE)
f<-f[,which(colnames(f) %in% c("IDfinca","Codi_Finca","Any","CF_A","Subzone","Zone","EspecieObj","Cont_pt",
                               "Recob_plotViu","Recob_plotMort","PromigAltura1Plot","Cluster",
                               "Simpson","lev_ind","age","area","shape","Tractament"))]


g<-f[which(f$EspecieObj == "PTALC"),]
g<-g[complete.cases(g),]
g<-g[-which(duplicated(g[,2:15])),]
which(duplicated(g$CF_A))

#Change treatment control so that it is the intercept
g$Tractament<-as.character(g$Tractament)
g$Tractament[which(g$Tractament == "Control")]<-"Acontrol"

################################################################################
#1. DATA EXPLORATION

#Relation response - Predictors

ggplot(g,aes(Recob_plotViu,Cont_pt))+geom_point()+geom_smooth()+theme_classic()
ggplot(g,aes(Recob_plotMort,Cont_pt))+geom_point()+geom_smooth()+theme_classic() 
ggplot(g,aes(PromigAltura1Plot,Cont_pt))+geom_point()+geom_smooth()+theme_classic() 
ggplot(g,aes(Simpson,Cont_pt))+geom_point()+geom_smooth()+theme_classic()
ggplot(g,aes(lev_ind,Cont_pt))+geom_point()+geom_smooth()+theme_classic() #quadratic?
ggplot(g,aes(area,Cont_pt))+geom_point()+geom_smooth()+theme_classic()
ggplot(g,aes(shape,Cont_pt))+geom_point()+geom_smooth()+theme_classic()
ggplot(g,aes(age,Cont_pt))+geom_point()+geom_smooth()+theme_classic()#Continuous. categorical?

ggplot(g,aes(as.factor(Cluster),Cont_pt))+geom_boxplot()+theme_classic()
ggplot(g,aes(as.factor(age),Cont_pt))+geom_boxplot()+theme_classic() #categorical

ggplot(g,aes(as.factor(Tractament),Cont_pt))+geom_boxplot()+theme_classic()

ggplot(g,aes(as.factor(Subzone),Cont_pt))+geom_boxplot()+theme_classic() 
ggplot(g,aes(as.factor(Zone),Cont_pt))+geom_boxplot()+theme_classic()#Possible effect of zone
ggplot(g,aes(as.factor(Any),Cont_pt))+geom_boxplot()+theme_classic() #No year effect

#CORRELATION BETWEEN VARIABLES (GENÉRICO)
cor(g$Recob_plotViu,g$PromigAltura1Plot)
cor(g$Simpson,g$lev_ind)
cor(g$age,g$area)
#Es posible que haya alta correlación entre las variables de vegetación y el tratamiento 
ggplot(g,aes(as.factor(Tractament),Recob_plotViu))+geom_boxplot()+theme_classic() #Correlation (más alfalfa y control)
ggplot(g,aes(as.factor(Tractament),Recob_plotMort))+geom_boxplot()+theme_classic() #Correlation (más picar)
ggplot(g,aes(as.factor(Tractament),PromigAltura1Plot))+geom_boxplot()+theme_classic() #Some correlation (más alfalfa y control),not much
ggplot(g,aes(as.factor(Tractament),Simpson))+geom_boxplot()+theme_classic() 

#RESPONSE

hist(g$Cont_pt) #Only presence/absence!!!!467/483; only 16 en AF (OC)
length(which(g$Cont_pt == 0))/174*100 #95% OF 0
xtabs(~ Cont_pt + Subzone,g) #Es posible que haya cuentas de 16,19,20,22?
xtabs(~ Cont_pt + Zone,g)


#CHECK ASSUMPTIONS, QUADRATIC EFFECTS, OVERDISPERSION
#NO HAY EN LOS ORIENTALES. USAR SÓLO LOS OCCIDENTALES
g<-g[which(g$Zone == "OCCIDENTAL"),]
g<-g[,c(3,6:18)]
#I start by doing a glm poisson model with all covariates and checking
#assumptions:
mp1<-glm(Cont_pt ~ Recob_plotViu + PromigAltura1Plot + Recob_plotMort + Simpson + lev_ind + area + shape + as.factor(Cluster)
         + as.factor(age) + as.factor(Tractament),
         data = g,
         family = poisson)

mp2<-glm(Cont_pt ~ Recob_plotViu + PromigAltura1Plot + Recob_plotMort + I(PromigAltura1Plot^2) + Simpson + lev_ind + 
           area + shape + as.factor(Cluster) + as.factor(age) + as.factor(Tractament),
         data = g,
         family = poisson)

mp3<-glm(Cont_pt ~ Recob_plotViu + PromigAltura1Plot + Recob_plotMort + Simpson + lev_ind + 
           I(lev_ind^2) + area + shape + as.factor(Cluster) + as.factor(age) + as.factor(Tractament),
         data = g,
         family = poisson)

mp4<-glm(Cont_pt ~ Recob_plotViu + Recob_plotMort + PromigAltura1Plot + I(PromigAltura1Plot^2) + Simpson + lev_ind + 
           I(lev_ind^2) + area + shape + as.factor(Cluster) + as.factor(age) + as.factor(Tractament),
         data = g,
         family = poisson)

AIC(mp1,mp2,mp3,mp4) #AIC better with quadratic effects of heterogeneity and height (mp4)
summary(mp4) # Dev/df = 2.02; Overdispersion
#In poisson dist: variance = mean
m<-mean(g$Cont_pt)
v<-var(g$Cont_pt)
ratio<-v/m
ratio    # r=12.59 so we have overdispersion (>1) (CHECK NB)

par(mfrow=c(2,2))
plot(mp3) #Not normal, not linear, not homogeneous
influencePlot(mp3) #Not very influencing obs

#Residuals against predictors to check homogeneity
ggplot(g,aes(Recob_plotViu,residuals(mp3)))+geom_point()+geom_smooth() #Homo?
ggplot(g,aes(PromigAltura1Plot,residuals(mp3)))+geom_point()+geom_smooth() #Not homo
ggplot(g,aes(area,residuals(mp3)))+geom_point()+geom_smooth() #Not homo
ggplot(g,aes(Subzone,rstandard(mp3)))+geom_boxplot()+theme_classic() #Mean around 0...no effect?
#Dont see the effect of zone so maybe I dont need a glmm

############################################################################################
#2. CHOOSE A MODEL STRUCTURE
#There is a lot of 0 AND overdispersion: use zero inflated

#Scale variables 
g$sviv<-scale(g$Recob_plotViu)
g$smuer<-scale(g$Recob_plotMort)
g$salt<-scale(g$PromigAltura1Plot)
g$salt_q<-scale(I(g$PromigAltura1Plot^2))
g$ssim<-scale(g$Simpson)
g$shet<-scale(g$lev_ind)
g$shet_q<-scale(I(g$lev_ind^2))
g$sshap<-scale(g$shape)
g$sar<-scale(g$area)
g$sage<-scale(g$age)
g$sclus<-scale(as.numeric(g$Cluster))
g$Tractament<-factor(g$Tractament) #.First as factor, take into account when unscaling!
g$strat<-scale(as.numeric(g$Tractament))
#Check from here
#Model mp4 to check the zero part of the model (cuadratic heterogeneity)

zip1<-zeroinfl(Cont_pt ~ sviv + smuer + salt + salt_q + ssim + shet + shet_q + sshap + sar + sage
               + sclus + strat, 
               dist="poisson", link = "logit",data = g)
summary(zip1) #In zero part, significant treatment, area

#Choose 0 part

zip2<-zeroinfl(Cont_pt ~ sviv + smuer + salt + salt_q + ssim + shet + shet_q + sshap + sar + sage
               + sclus + strat |
                 sar + sviv + strat + sshap,
               dist="poisson", link = "logit",data = g)

zip3<-zeroinfl(Cont_pt ~ sviv + smuer + salt + salt_q + ssim + shet + shet_q + sshap + sar + sage
               + sclus + strat |
                 sar + sviv + strat,
               dist="poisson", link = "logit",data = g)

zip4<-zeroinfl(Cont_pt ~ sviv + smuer + salt + salt_q + ssim + shet + shet_q + sshap + sar + sage
               + sclus + strat |
                 sar + strat,
               dist="poisson", link = "logit",data = g)

zip5<-zeroinfl(Cont_pt ~ sviv + smuer + salt + salt_q + ssim + shet + shet_q + sshap + sar + sage
               + sclus + strat |
                 sar,
               dist="poisson", link = "logit",data = g)

AIC(zip2, zip3, zip4, zip5) #Best zip2 con todas las significativas
summary(zip2)
#Compare with poisson alone 
AIC(mp3,zip2) #Much better zero inflated!

#Use vuong test to check if ZIP or poisson is better
mp<-glm(Cont_pt ~ sviv + smuer + salt + salt_q + ssim + shet + shet_q + sshap + sar + sage
        + sclus + strat,
        family = "poisson", data = g)
vuong(mp,zip2) # En todos test, model 2 da mejor fit que poisson (ZIP MEJOR QUE POISSON)

##################################################################################
#3. MODEL SELECTION

library(MuMIn)
options(na.action=na.fail)
m_pter<-dredge(zip2, beta = "none", evaluate = TRUE )

save(m_pter,file = "pteroclidos_dred.RData")

load("pteroclidos_dred.RData")

p<-get.models(m_pter,subset = delta < 2)
avg_pter<-model.avg(p)
save(avg_pter,file = "avg_pter.RData")

#Model with year as normal covariates. Only occidentales, es donde están la mayoría

g$Any<-as.factor(g$Any)
g$Tractament<-as.factor(g$Tractament)
g$Cluster<-as.factor(g$Cluster)

zip<-zeroinfl(Cont_pt ~ sviv + smuer + salt + salt_q + ssim + shet + shet_q + sshap + sar + sage
               + sclus + strat + Any |
                 sar + sviv + strat + sshap,
               dist="poisson", link = "logit",data = g)

g$Cluster<-as.factor(g$Cluster)
zip_notrat<-zeroinfl(Cont_pt ~ sviv + smuer + salt + salt_q + ssim + shet + shet_q + sshap + sage + Any |
                sviv + sshap,
              dist="poisson", link = "logit",data = g, offset = log(area))

summary(zip)
summary(zip_notrat)

m_ptero_anyzone<-dredge(zip, beta = "none", evaluate = TRUE)
save(m_ptero_anyzone,file = "ptero_dred_any.RData")

m_ptero_notrat<-dredge(zip_notrat, beta = "none", evaluate = TRUE)
save(m_ptero_notrat,file = "ptero_notrat.RData")

load("ptero_dred_anyzone.RData")

m<-get.models(m_ptero_anyzone,subset = delta < 2)
avg_ptero_zero<-model.avg(m, fit = TRUE)
summary(avg_ptero_zero)

m<-get.models(m_ptero_notrat,subset = delta < 2)
avg_ptero_zero<-model.avg(m, fit = TRUE)
summary(avg_ptero_zero) #No trat significativo: age(-),alt(+),het + het2,muert


#Significativo: 
#Count: Age, Muerta, Shape, Div, Viva
#Zero: Area, Shape, tratamiento

pred <- predict( avg_ptero_zero, type="response" ) #It doest return se!!!

library(ggplot2)

ggplot(g, aes(x = age, y = pred)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "loess", formula = y ~ x , size = 1) +
  labs(x = "Age", y = "Pteroclidos")

ggplot(g, aes(x = Recob_plotMort, y = pred)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "lm", formula = y ~ x , size = 1) +
  labs(x = "Recubrimiento vegetación muerta", y = "Pteroclidos")

ggplot(g, aes(x = shape, y = pred)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x , size = 1) +
  labs(x = "Shape", y = "Pteroclidos")

ggplot(g, aes(x = Simpson, y = pred)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "lm", formula = y ~ x , size = 1) +
  labs(x = "Diversidad de plantas", y = "Pteroclidos")

ggplot(g, aes(x = Recob_plotViu, y = pred)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "lm", formula = y ~ x , size = 1) +
  labs(x = "Recubrimiento vegetación viva", y = "Pteroclidos")

ggplot(g, aes(x = Tractament, y = pred)) +
  geom_point() +
  #geom_line() +
  geom_boxplot() +
  labs(x = "Traramiento", y = "Pteroclidos")

ggplot(g, aes(x = strat, y = pred)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "lm", formula = y ~ x , size = 1) +
  labs(x = "Recubrimiento vegetación viva", y = "Pteroclidos")

#####################################################################################
#NADA FUNCIONA. CREAR MODEL AVG GLM POISSON NORMAL CON RANDOM EFFECTS. 
library(lme4)

#Categorical as factor
g$Zone<-as.factor(g$Zone)
g$Any<-as.factor(g$Any)
g$Tractament<-as.factor(g$Tractament)
g$Cluster<-as.factor(g$Cluster)
g$age<-as.factor(g$age)
#Scaled variables
g$sviv<-scale(g$Recob_plotViu)
g$smuer<-scale(g$Recob_plotMort)
g$salt<-scale(g$PromigAltura1Plot)
g$salt_q<-scale(I(g$PromigAltura1Plot^2))
g$ssim<-scale(g$Simpson)
g$shet<-scale(g$lev_ind)
g$shet_q<-scale(I(g$lev_ind^2))
g$sshap<-scale(g$shape)
g$sar<-scale(g$area)
g$sage<-scale(g$age)
g$sage<-scale(g$age)
g$sclus<-scale(as.numeric(g$Cluster))
g$Tractament<-factor(g$Tractament) #.First as factor, take into account when unscaling!
g$strat<-scale(as.numeric(g$Tractament))


mp<-glmer(Cont_pt ~ sviv + smuer + salt + salt_q + ssim + shet + shet_q + sshap + sar + sage
          + sclus + Tractament + (1| Any),
          data = g, family = poisson)

ptero_glm<-dredge(mp, beta = "none", evaluate = TRUE)
save(ptero_glm,file = "ptero_glm.RData")

load("ptero_glm.RData")
m<-get.models(ptero_glm,subset = delta < 2)
avg_ptero<-model.avg(m)
summary(avg_ptero)

#Significant: Age, area, heterogeneity (2), div, muerta, shape, viva, tratam (picar+herb, picar)

pred2 <- predict( avg_ptero, type="response" ) 

ggplot(g, aes(x = age, y = pred2)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "loess", formula = y ~ x , size = 1) +
  labs(x = "Age", y = "Pteroclidos")

ggplot(g, aes(x = area, y = pred2)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "loess", formula = y ~ x , size = 1) +
  labs(x = "Area", y = "Pteroclidos")

ggplot(g, aes(x = lev_ind, y = pred2)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "loess", formula = y ~ x , size = 1) +
  labs(x = "Heterogeneidad", y = "Pteroclidos")

ggplot(g, aes(x = Recob_plotMort, y = pred2)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "loess", formula = y ~ x , size = 1) +
  labs(x = "Recubrimiento vegetación muerta", y = "Pteroclidos")

ggplot(g, aes(x = shape, y = pred2)) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x , size = 1) +
  labs(x = "Shape", y = "Pteroclidos")

ggplot(g, aes(x = Simpson, y = pred2)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "loess", formula = y ~ x , size = 1) +
  labs(x = "Diversidad de plantas", y = "Pteroclidos")

ggplot(g, aes(x = Recob_plotViu, y = pred2)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "lm", formula = y ~ x , size = 1) +
  labs(x = "Recubrimiento vegetación viva", y = "Pteroclidos")

ggplot(g, aes(x = Tractament, y = pred2)) +
  geom_point() +
  #geom_line() +
  geom_boxplot() +
  labs(x = "Traramiento", y = "Pteroclidos")



#Simple glm
g$Tractament<-as.factor(g$Tractament)
pteroc<-glm(Cont_pt ~ Tractament,family = "poisson",data=g)
summary(pteroc)
pred <- predict(pteroc,newdata = data.frame(Tractament=sort(unique(g$Tractament))),type="response",se.fit =TRUE )

lcl <- pred$fit - 1.96*pred$se.fit
lch <- pred$fit + 1.96*pred$se.fit

plot(pred$fit~sort(unique(g$Tractament)), ylim=c(0,2), main = "PTEROCLIDOS")
segments(x0=c(1:6),
         x1=c(1:6),
         y0=lcl,
         y1=lch)
unique(b$Tractament)

#####################################################################

#Try to fit it with a binomial distribution to see if it works
r<-g
r$Cont_pt[r$Cont_pt > 1]<-1

r$Zone<-as.factor(r$Zone)
r$Any<-as.factor(r$Any)
r$Tractament<-as.factor(r$Tractament)
r$Cluster<-as.factor(r$Cluster)
r$sage<-scale(as.numeric(r$age))

mbin<-glm(Cont_pt ~ sviv + smuer + salt + salt_q + ssim + shet + shet_q + sshap + sar + sage
           + sclus + Tractament + Any,
          data = r, family = binomial)

mbin_notrat<-glm(Cont_pt ~ sviv + smuer + salt + salt_q + ssim + shet + shet_q + sshap + sage
          + Cluster + Any,
          data = r, family = binomial,offset = log(r$area))

ptero_glm_bin<-dredge(mbin, beta = "none", evaluate = TRUE)
save(ptero_glm_bin,file = "ptero_glm_bin.RData")

ptero_glmbin_notrat<-dredge(mbin_notrat, beta = "none", evaluate = TRUE)
save(ptero_glmbin_notrat,file = "ptero_glm_bin_notrat.RData")

load("ptero_glm_bin.RData")
m<-get.models(ptero_glm_bin,subset = delta < 2)
avg_ptero_bin<-model.avg(m)
summary(avg_ptero_bin)

m<-get.models(ptero_glmbin_notrat,subset = delta < 2)
avg_ptero_bin<-model.avg(m)
summary(avg_ptero_bin) #Sign: NADA (heterogeneity?)

#Sign: Area,Muerta, Viva, Picar y herbicidar

pred3 <- predict( avg_ptero_bin, type="response" ) 

ggplot(r, aes(x = area, y = pred3)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "lm", formula = y ~ x , size = 1) +
  labs(x = "Area", y = "Presencia pteróclidos")

ggplot(r, aes(x = Recob_plotViu, y = pred3)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "lm", formula = y ~ x , size = 1) +
  labs(x = "Viva", y = "Pteroclidos")

ggplot(r, aes(x = Recob_plotMort, y = pred3)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "lm", formula = y ~ x , size = 1) +
  labs(x = "Muerta", y = "Pteroclidos")

r$Tractament<-as.character(r$Tractament)

r$Tractament[which(r$Tractament == "Acontrol")]<-"Control"
r$Tractament[which(r$Tractament == "Alfals")]<-"Alfalfa"
r$Tractament[which(r$Tractament == "Curronar")]<-"Compactar"
r$Tractament[which(r$Tractament == "Llaurar")]<-"Labrar"
r$Tractament[which(r$Tractament == "Picar i herbicidar")]<-"Pic + Herb"

ggplot(r, aes(x = Tractament, y = pred3)) +
  geom_point() +
  #geom_line() +
  geom_boxplot() +
  theme_bw() +
  theme(text = element_text(size=23)) +
  labs(x = "\nTratamiento", y = "Presencia pteroclidos\n")
  

#GLM sin trat
mbin2<-glmer(Cont_pt ~ sviv + smuer + salt + salt_q + ssim + shet + shet_q + sshap + sar + sage
          + sclus + (1| Any),
          data = r, family = binomial)
summary(mbin2) #SOLO SALE EL AREA
