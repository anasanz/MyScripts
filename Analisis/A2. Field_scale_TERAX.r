
#FIELD SCALE_TERAX(SISON)

setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")
f<-read.csv("Variables.csv",sep = ",",header=TRUE,fill = TRUE)
f<-f[,which(colnames(f) %in% c("IDfinca","Codi_Finca","Any","CF_A","Subzone","Zone","EspecieObj","Contatge",
                               "Recob_plotViu","Recob_plotMort","PromigAltura1Plot","Cluster",
                               "Simpson","lev_ind","age","area","shape","Tractament"))]

i<-f[which(f$EspecieObj == "TERAX_M"),]
i<-i[complete.cases(i),]
i<-i[-which(duplicated(i[,2:15])),]
#Change treatment control so that it is the intercept
i$Tractament<-as.character(i$Tractament)
i$Tractament[which(i$Tractament == "Control")]<-"Acontrol"

################################################################################
#1. DATA EXPLORATION

#Relation response - Predictors

ggplot(i,aes(Recob_plotViu,Contatge))+geom_point()+geom_smooth()+theme_classic()
ggplot(i,aes(Recob_plotMort,Contatge))+geom_point()+geom_smooth()+theme_classic() 
ggplot(i,aes(PromigAltura1Plot,Contatge))+geom_point()+geom_smooth()+theme_classic() #Quadratic?
ggplot(i,aes(Simpson,Contatge))+geom_point()+geom_smooth()+theme_classic()
ggplot(i,aes(lev_ind,Contatge))+geom_point()+geom_smooth()+theme_classic() 
ggplot(i,aes(area,Contatge))+geom_point()+geom_smooth()+theme_classic()
ggplot(i,aes(shape,Contatge))+geom_point()+geom_smooth()+theme_classic()
ggplot(i,aes(age,Contatge))+geom_point()+geom_smooth()+theme_classic()#Continuous. categorical?

ggplot(i,aes(as.factor(Cluster),Contatge))+geom_boxplot()+theme_classic()
ggplot(i,aes(as.factor(age),Contatge))+geom_boxplot()+theme_classic() #categorical

ggplot(i,aes(as.factor(Tractament),Contatge))+geom_boxplot()+theme_classic()
ggplot(i,aes(as.factor(Subzone),Contatge))+geom_boxplot()+theme_classic() 
ggplot(i,aes(as.factor(Zone),Contatge))+geom_boxplot()+theme_classic()#Possible effect of zone
ggplot(i,aes(as.factor(Any),Contatge))+geom_boxplot()+theme_classic() #No year effect

#CORRELATION BETWEEN VARIABLES (generic)

cor(i$Recob_plotViu,i$PromigAltura1Plot)
cor(i$Simpson,i$lev_ind)
#Es posible que haya alta correlación entre las variables de vegetación y el tratamiento
ggplot(i,aes(as.factor(Tractament),Recob_plotViu))+geom_boxplot()+theme_classic() #Correlation (más alfalfa y control)
ggplot(i,aes(as.factor(Tractament),Recob_plotMort))+geom_boxplot()+theme_classic() #Correlation (más picar)
ggplot(i,aes(as.factor(Tractament),PromigAltura1Plot))+geom_boxplot()+theme_classic() #Some correlation (más alfalfa y control),not much
ggplot(i,aes(as.factor(Tractament),Simpson))+geom_boxplot()+theme_classic() 

par(mfrow=c(1,1))

#RESPONSE

hist(i$Contatge) # 373/483 ; 110 presences. MÃ¡s o menos distribuida
length(which(i$Contatge == 0))/451*100 #77% of 0
xtabs(~ Contatge + Subzone,i) 
xtabs(~ Contatge + Zone,i) 

#CHECK ASSUMPTIONS, QUADRATIC EFFECTS, OVERDISPERSION

#I start by doing a glm poisson model with all covariates 
i<-i[,c(3,6:18)]

mp1<-glm(Contatge ~ Recob_plotViu + PromigAltura1Plot + Simpson + lev_ind + area + shape + as.factor(Cluster)
         + as.factor(age) + as.factor(Tractament),
         data = i,
         family = poisson)

mp2<-glm(Contatge ~ Recob_plotViu + PromigAltura1Plot + I(PromigAltura1Plot^2) + Simpson + lev_ind + 
           area + shape + as.factor(Cluster) + as.factor(age) + as.factor(Tractament),
         data = i,
         family = poisson)

mp3<-glm(Contatge ~ Recob_plotViu + PromigAltura1Plot + Simpson + lev_ind + 
           I(lev_ind^2) + area + shape + as.factor(Cluster) + as.factor(age) + as.factor(Tractament),
         data = i,
         family = poisson)

mp4<-glm(Contatge ~ Recob_plotViu + PromigAltura1Plot + I(PromigAltura1Plot^2) + Simpson + lev_ind + 
           I(lev_ind^2) + area + shape + as.factor(Cluster) + as.factor(age) + as.factor(Tractament),
         data = i,
         family = poisson)

AIC(mp1,mp2,mp3,mp4) #AIC better with quadratic effects of heterogeneity ONLY (mp3)


summary(mp3) # Dev/df = 0.89; UNDERDISPERSION????
#In poisson dist: variance = mean
m<-mean(i$Contatge)
v<-var(i$Contatge)
ratio<-v/m
ratio    # r=1.47 so we have overdispersion (>1) (but underdispersion in d/df?)

par(mfrow=c(2,2))
plot(mp3) #Not normal, not linear, not homogeneous

#Residuals against predictors to check homogeneity
ggplot(i,aes(Recob_plotViu,residuals(mp3)))+geom_point()+geom_smooth() #Homo?
ggplot(i,aes(PromigAltura1Plot,residuals(mp3)))+geom_point()+geom_smooth() #Not homo
ggplot(i,aes(area,residuals(mp3)))+geom_point()+geom_smooth() #Not homo
ggplot(i,aes(Subzone,rstandard(mp3)))+geom_boxplot()+theme_classic() #Mean around 0...no effect?
#Dont see the effect of zone so maybe I dont need a glmm

############################################################################################
#2. CHOOSE A MODEL STRUCTURE

#There is a lot of 0 BUT not overdispersion (o si?): use zero inflated????
#Scale variables to make them equal
library(pscl)   
i$sviv<-scale(i$Recob_plotViu)
i$smuer<-scale(i$Recob_plotMort)
i$salt<-scale(i$PromigAltura1Plot)
i$salt_q<-scale(I(i$PromigAltura1Plot^2))
i$ssim<-scale(i$Simpson)
i$shet<-scale(i$lev_ind)
i$shet_q<-scale(I(i$lev_ind^2))
i$sshap<-scale(i$shape)
i$sar<-scale(i$area)
i$sage<-scale(i$age)
i$sclus<-scale(as.numeric(i$Cluster))

#Model with only quadratic effects of heterogeneity to check the 0 part of the model
#Cant include cluster as categorical. Include as continuous scaled (sclus)

zip1<-zeroinfl(Contatge ~ sviv + smuer + salt + ssim + shet + shet_q + sshap + sar + sage
               + sclus + as.factor(Tractament), 
               dist="poisson", link = "logit",data = i)
summary(zip1)

#In the zero inflated part: sclust, salt y sviv significant. Only keep these
zip2<-zeroinfl(Contatge ~ sviv + smuer + salt + ssim + shet + shet_q + sshap + sar + sage
               + sclus + as.factor(Tractament) | sclus + sviv + salt, 
               dist="poisson", link = "logit",data = i)

zip3<-zeroinfl(Contatge ~ sviv + smuer + salt + ssim + shet + shet_q + sshap + sar + sage
               + sclus + as.factor(Tractament) | sclus, 
               dist="poisson", link = "logit",data = i)

zip4<-zeroinfl(Contatge ~ sviv + smuer + salt + ssim + shet + shet_q + sshap + sar + sage
               + sclus + as.factor(Tractament) | sclus + sviv, 
               dist="poisson", link = "logit",data = i)

AIC(zip1,zip2,zip3,zip4)#Better with all terms included but to take the most parsimonious I take zip2 (low difference)
summary(zip2) 

#Compare with poisson alone 
AIC(zip2,mp3) #Much better zero inflated!

#Use vuong test to check if ZIP or poisson is better
mp<-glm(Contatge ~ sviv + smuer + salt + ssim + shet + shet_q + sshap + sar + sage
        + sclus + as.factor(Tractament),
        family = "poisson", data = i)
vuong(mp,zip2) # En 2/3 casos, model 2 da mejor fit que poisson (ZIP MEJOR QUE POISSON)

##################################################################################
#3. MODEL SELECTION

library(MuMIn)
#options(na.action=na.fail)
#m_ter<-dredge(zip2, beta = "none", evaluate = TRUE )
#
#save(m_ter,file = "terax_dred.RData")

load("terax_dred.RData")

t<-get.models(m_ter,subset = delta < 2)
avg_ter<-model.avg(t)
#save(avg_ter,file = "avg_ter.RData")

#Model with year as normal covariates. Only ORIENTALES, es donde están la mayoría
i<-i[which(i$Zone == "ORIENTAL"),]
i$Any<-as.factor(i$Any)
i$Tractament<-as.factor(i$Tractament)
i$Cluster<-as.factor(i$Cluster)

zip<-zeroinfl(Contatge ~ sviv + smuer + salt + ssim + shet + shet_q + sshap + sar + sage
               + sclus + Tractament + Any| sclus + sviv + salt, 
               dist="poisson", link = "logit",data = i)
zip_notrat<-zeroinfl(Contatge ~ sviv + smuer + salt + ssim + shet + shet_q + sshap + sage + Any| sviv + salt, 
              dist="poisson", link = "logit",data = i, offset = log(i$area))

summary(zip)
summary(zip_notrat)

m_ter_anyzone<-dredge(zip, beta = "none", evaluate = TRUE)
#save(m_ter_anyzone,file = "terax_dred_anyzone.RData")

m_ter_anyzone_notrat<-dredge(zip_notrat, beta = "none", evaluate = TRUE)
save(m_ter_anyzone_notrat,file = "terax_dred_anyzone_notrat.RData")

load("terax_dred_anyzone.RData")
m<-get.models(m_ter_anyzone,subset = delta < 2)
avg_ter_zero<-model.avg(m)

m<-get.models(m_ter_anyzone_notrat,subset = delta < 2)
avg_ter_zero<-model.avg(m)
summary(avg_ter_zero) #Significativo no trat: Age, het2,div(-), alt

#Significativo: 
#Count: Area, Diversity
#Zero: Alt

#Predict:
#pred <- predict( avg_ter,newdata = data.frame(sviv = i$sviv, smuer = i$smuer, salt = i$salt, salt_q = ssim = i$ssim,
#                                              shet = i$shet, shet_q = i$shet_q, sshap= i$sshap, sar = i$sar, 
#                                              sage = i$sage, sclus = i$sclus, Tractament = i$Tractament, 
#                                              Any = i$Any), type="response", se.fit = TRUE )

pred <- predict( avg_ter_zero, type="response" ) #It doest return se!!!

library(ggplot2)

ggplot(i, aes(x = PromigAltura1Plot, y = pred)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "auto", formula = y ~ x , size = 1) +
  theme_bw() +
  theme(text = element_text(size=23)) +
  labs(x = "\nAltura (m)", y = "Abundancia sisón\n")
  

ggplot(i, aes(x = Simpson, y = pred)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "lm", formula = y ~ x , size = 1) +
  theme_bw() +
  theme(text = element_text(size=23)) +
  labs(x = "\nDiversidad florística (Sim.index)", y = "Abundancia sisón\n")

ggplot(i, aes(x = area, y = pred)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "lm", formula = y ~ x , size = 1) +
  labs(x = "Area", y = "Abundancia sisón")



#####################################################################################
#NADA FUNCIONA. CREAR MODEL AVG GLM POISSON NORMAL CON RANDOM EFFECTS. 
library(lme4)

#Categorical as factor
i$Zone<-as.factor(i$Zone)
i$Any<-as.factor(i$Any)
i$Tractament<-as.factor(i$Tractament)
i$Cluster<-as.factor(i$Cluster)
i$age<-as.factor(i$age)
#Scaled variables
i$sviv<-scale(i$Recob_plotViu)
i$smuer<-scale(i$Recob_plotMort)
i$salt<-scale(i$PromigAltura1Plot)
i$salt_q<-scale(I(i$PromigAltura1Plot^2))
i$ssim<-scale(i$Simpson)
i$shet<-scale(i$lev_ind)
i$shet_q<-scale(I(i$lev_ind^2))
i$sshap<-scale(i$shape)
i$sar<-scale(i$area)
i$sage<-scale(i$age)


mp<-glmer(Contatge ~ sviv + smuer + salt + ssim + shet + shet_q + sshap + sar + sage
          + sclus + Tractament + (1| Any),
          data = i, family = poisson)

ter_glm<-dredge(mp, beta = "none", evaluate = TRUE)
#save(ter_glm,file = "ter_glm.RData")
load("ter_glm.RData")
m<-get.models(ter_glm,subset = delta < 2)
avg_ter<-model.avg(m)
summary(avg_ter)
pred2 <- predict( avg_ter, type="response" ) 


ggplot(i, aes(x = PromigAltura1Plot, y = pred2)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "auto", formula = y ~ x , size = 1) +
  labs(x = "Altura", y = "Sisón")

ggplot(i, aes(x = Simpson, y = pred2)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "lm", formula = y ~ x , size = 1) +
  labs(x = "Diversidad plantas", y = "Sisón")

ggplot(i, aes(x = area, y = pred2)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "lm", formula = y ~ x , size = 1) +
  labs(x = "area", y = "Sisón")


###########################################################
#EXAMPLE TRY NORMAL MODEL AVERAGING WITH GLM
mp3<-glm(Contatge ~ Recob_plotViu + PromigAltura1Plot + Simpson + lev_ind + 
           I(lev_ind^2) + area,
         data = i,
         family = poisson)

DRED<-dredge(mp3, beta = "none", evaluate = TRUE)
m<-get.models(DRED,subset = delta < 2)
avg_DRED<-model.avg(m, fit = TRUE)
summary(avg_DRED) #In normal model averaging it returns the SE. So the problem is the zero inflated
#Predict:
pred2 <- predict( avg_DRED, type="response", se.fit = TRUE ) 


###################################################################
#PRUEBA: Con zero inflated no devuelve se
CACA<-zeroinfl(Contatge ~ sviv + smuer + salt, dist="poisson", link = "logit",data = i)
summary(CACA)
c<-dredge(CACA, beta = "none", evaluate = TRUE)
m<-get.models(c,subset = delta < 2)
avg_caca<-model.avg(m, fit = TRUE)
summary(avg_caca)
#Predict:
pred_caca <- predict( avg_caca,newdata = data.frame(sviv = i$sviv, smuer = i$smuer, salt = i$salt), type="response", se.fit = TRUE )
predict(CACA, newdata, se.fit = TRUE, conf = 0.95,
        MC = 1000, type = c("response"),na.action = na.pass)
pred_caca <- predict( CACA,newdata = data.frame(sviv = i$sviv, smuer = i$smuer, salt = i$salt), type="response", se = TRUE )
lcl <- pred$fit - 1.96*pred$se.fit
lch <- pred$fit + 1.96*pred$se.fit
plot(pred~i$sar, ylim=c(0,1.5),main = "TERAX")



#Simple glm
i$Tractament<-as.factor(i$Tractament)
terax<-glm(Contatge ~ Tractament,family = "poisson",data=i)
summary(terax)
pred <- predict(terax,newdata = data.frame(Tractament=sort(unique(i$Tractament))),type="response",se.fit =TRUE )

lcl <- pred$fit - 1.96*pred$se.fit
lch <- pred$fit + 1.96*pred$se.fit

plot(pred$fit~sort(unique(i$Tractament)), ylim=c(0,1.5),main = "TERAX")
segments(x0=c(1:6),
         x1=c(1:6),
         y0=lcl,
         y1=lch)

#Alfals significativo. Puede ser porque hay muchas alfalfas en los orientales, que es donde están mayoritariamente.
#Subset y probar sólo con los orientales
o<-i[which(i$Zone == "ORIENTAL"),]
o$Tractament<-as.factor(o$Tractament)
terax<-glm(Contatge ~ Tractament,family = "poisson",data=o)
summary(terax)
pred <- predict(terax,newdata = data.frame(Tractament=sort(unique(o$Tractament))),type="response",se.fit =TRUE )

lcl <- pred$fit - 1.96*pred$se.fit
lch <- pred$fit + 1.96*pred$se.fit

plot(pred$fit~sort(unique(o$Tractament)), ylim=c(0,1.5),main = "TERAX ORIENTALES")
segments(x0=c(1:6),
         x1=c(1:6),
         y0=lcl,
         y1=lch)

#Alfalfa sigue siendo significativo
