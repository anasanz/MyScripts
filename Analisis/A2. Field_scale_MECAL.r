
#FIELD SCALE_MECAL(CALANDRIA)

library(MuMIn)
library(pscl) 
library(ggplot2)


setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")
f<-read.csv("Variables.csv",sep = ",",header=TRUE,fill = TRUE)
f<-f[,which(colnames(f) %in% c("IDfinca","Codi_Finca","Any","CF_A","Subzone","Zone","EspecieObj","Contatge",
                               "Recob_plotViu","Recob_plotMort","PromigAltura1Plot","Cluster",
                               "Simpson","lev_ind","age","area","shape"))]

e<-f[which(f$EspecieObj == "MECAL"),]
e<-e[complete.cases(e),]
e<-e[-which(duplicated(e[,2:15])),]
which(duplicated(e$CF_A)) 
e<-e[-which(duplicated(e$CF_A)),] 


################################################################################
#1. DATA EXPLORATION

#Relation response - Predictors

ggplot(e,aes(Recob_plotViu,Contatge))+geom_point()+geom_smooth()+theme_classic()
ggplot(e,aes(Recob_plotMort,Contatge))+geom_point()+geom_smooth()+theme_classic() #More pattern than viva
ggplot(e,aes(PromigAltura1Plot,Contatge))+geom_point()+geom_smooth()+theme_classic() #Quadratic?
ggplot(e,aes(Simpson,Contatge))+geom_point()+geom_smooth()+theme_classic()
ggplot(e,aes(lev_ind,Contatge))+geom_point()+geom_smooth()+theme_classic() #quadratic? 
ggplot(e,aes(area,Contatge))+geom_point()+geom_smooth()+theme_classic()
ggplot(e,aes(shape,Contatge))+geom_point()+geom_smooth()+theme_classic()
ggplot(e,aes(age,Contatge))+geom_point()+geom_smooth()+theme_classic()#Continuous. categorical?

ggplot(e,aes(as.factor(Cluster),Contatge))+geom_boxplot()+theme_classic()
ggplot(e,aes(as.factor(age),Contatge))+geom_boxplot()+theme_classic() #categorical

ggplot(e,aes(as.factor(Tractament),Contatge))+geom_boxplot()+theme_classic()

ggplot(e,aes(as.factor(Subzone),Contatge))+geom_boxplot()+theme_classic() 
ggplot(e,aes(as.factor(Zone),Contatge))+geom_boxplot()+theme_classic()#Possible effect of zone
ggplot(e,aes(as.factor(Any),Contatge))+geom_boxplot()+theme_classic() #No year effect

#CORRELATION BETWEEN VARIABLES (generic)
cor(e$Recob_plotMort,e$PromigAltura1Plot)
cor(e$Simpson,e$lev_ind)
#Es posible que haya alta correlación entre las variables de vegetación y el tratamiento
ggplot(e,aes(as.factor(Tractament),Recob_plotViu))+geom_boxplot()+theme_classic() #Correlation (más alfalfa y control)
ggplot(e,aes(as.factor(Tractament),Recob_plotMort))+geom_boxplot()+theme_classic() #Correlation (más picar)
ggplot(e,aes(as.factor(Tractament),PromigAltura1Plot))+geom_boxplot()+theme_classic() #Some correlation (más alfalfa y control),not much
ggplot(e,aes(as.factor(Tractament),Simpson))+geom_boxplot()+theme_classic() 

#RESPONSE

hist(e$Contatge) #324/483; 159 presences; mÃ¡s o menos bien distribuidas
length(which(e$Contatge == 0))/451*100 #65??? de 0
xtabs(~ Contatge + Subzone,e) 

#CHECK ASSUMPTIONS, QUADRATIC EFFECTS, OVERDISPERSION

e<-e[,c(3,6:17)]

#I start by doing a glm poisson model with all covariates and checking
#assumptions:
mp1<-glm(Contatge ~ Recob_plotViu + Recob_plotMort + PromigAltura1Plot + Simpson + lev_ind + area + shape + as.factor(Cluster)
         + as.factor(age) + as.factor(Tractament),
         data = e,
         family = poisson)

mp2<-glm(Contatge ~ Recob_plotViu + Recob_plotMort + PromigAltura1Plot + I(PromigAltura1Plot^2) + Simpson + lev_ind + 
           area + shape + as.factor(Cluster) + as.factor(age) + as.factor(Tractament),
         data = e,
         family = poisson)

mp3<-glm(Contatge ~ Recob_plotViu + Recob_plotMort + PromigAltura1Plot + I(PromigAltura1Plot^2) + Simpson + lev_ind + 
           I(lev_ind^2) + area + shape + as.factor(Cluster) + as.factor(age) + as.factor(Tractament),
         data = e,
         family = poisson)

AIC(mp1,mp2,mp3) #Same AIC. No quadractic effects
summary(mp1) # Dev/df = 1.744731; Overdispersion
#In poisson dist: variance = mean
m<-mean(e$Contatge)
v<-var(e$Contatge)
ratio<-v/m
ratio    # r=2.66 so we have overdispersion (>1)

par(mfrow=c(2,2))
plot(mp1) #Not normal, not linear, not homogeneous

#Residuals against predictors to check homogeneity

ggplot(e,aes(Recob_plotViu,residuals(mp1)))+geom_point()+geom_smooth() #Homo?
ggplot(e,aes(PromigAltura1Plot,residuals(mp1)))+geom_point()+geom_smooth() #Not homo
ggplot(e,aes(area,residuals(mp1)))+geom_point()+geom_smooth() #Not homo
ggplot(e,aes(Subzone,rstandard(mp3)))+geom_boxplot()+theme_classic() #Mean around 0...no effect?
#Dont see the effect of zone so maybe I dont need a glmm

############################################################################################
#2. CHOOSE A MODEL STRUCTURE
#There is a lot of 0 AND overdispersion: use zero inflated
#Scale variables 
e$sviv<-scale(e$Recob_plotViu)
e$smuer<-scale(e$Recob_plotMort)
e$salt<-scale(e$PromigAltura1Plot)
e$salt_q<-scale(I(e$PromigAltura1Plot^2))
e$ssim<-scale(e$Simpson)
e$shet<-scale(e$lev_ind)
e$shet_q<-scale(I(e$lev_ind^2))
e$sshap<-scale(e$shape)
e$sar<-scale(e$area)
e$sage<-scale(e$age)

#Model mp1 to check the zero part of the model

zip1<-zeroinfl(Contatge ~ sviv + smuer + salt + ssim + shet + sshap + sar + sage
               + as.factor(Cluster) + as.factor(Tractament), 
               dist="poisson", link = "logit",data = e)

summary(zip2) #In zero part, significant smuert, shape, area, Treatment

#Choose the 0 structure
zip2<-zeroinfl(Contatge ~ sviv + smuer + salt + ssim + shet + sshap + sar + sage
               + as.factor(Cluster) + as.factor(Tractament) | smuer + sar + sshap + as.factor(Tractament), 
               dist="poisson", link = "logit",data = e)

zip3<-zeroinfl(Contatge ~ sviv + smuer + salt + ssim + shet + sshap + sar + sage
               + as.factor(Cluster) + as.factor(Tractament) | smuer + sar + as.factor(Tractament), 
               dist="poisson", link = "logit",data = e)
zip4<-zeroinfl(Contatge ~ sviv + smuer + salt + ssim + shet + sshap + sar + sage
               + as.factor(Cluster) + as.factor(Tractament) | smuer + sar, 
               dist="poisson", link = "logit",data = e)

zip5<-zeroinfl(Contatge ~ sviv + smuer + salt + ssim + shet + sshap + sar + sage
               + as.factor(Cluster) + as.factor(Tractament) | sar, 
               dist="poisson", link = "logit",data = e)

AIC(zip2,zip3,zip4,zip5) #Best 0 inflated: zip2, keeping all

#Compare with poisson alone 
AIC(mp1,zip2) #Much better zero inflated!

#Use vuong test to check if ZIP or poisson is better
mp<-glm(Contatge ~ sviv + smuer + salt + ssim + shet + sshap + sar + sage
        + as.factor(Cluster) + as.factor(Tractament),
        family = "poisson", data = e)
vuong(mp,zip2) # En todos test, model 2 da mejor fit que poisson (ZIP MEJOR QUE POISSON)

##################################################################################
#3. MODEL SELECTION


options(na.action=na.fail)
m_mec<-dredge(zip2, beta = "none", evaluate = TRUE )

#save(m_mec,file = "mecal_dred.RData")

load("mecal_dred.RData")

m<-get.models(m_mec,subset = delta < 2)
avg_mec<-model.avg(m)
save(avg_mec,file = "avg_mec.RData")

#####################################################################
#Adding year and zone as normal variables

e$Tractament<-as.factor(e$Tractament)
e$Cluster<-as.factor(e$Cluster)
e$Tractament<-as.factor(e$Tractament)
e$Any<-as.factor(e$Any)

zip<-zeroinfl(Contatge ~ sviv + smuer + salt + ssim + shet + sshap + sage +
               Cluster + Any + Zone | smuer + sar + sshap, 
              dist="poisson", link = "logit", data = e, offset = log(area))

m_mec_anyzone<-dredge(zip, beta = "none", evaluate = TRUE)

save(m_mec_anyzone,file = "mecal_dred_anyzone_notrat.RData") #Sin trat, sale significativo altura, cover viva, div(-) y forma

load("mecal_dred_anyzone.RData")
m<-get.models(m_mec_anyzone,subset = delta < 2)
avg_mec_zero<-model.avg(m)
summary(avg_mec_zero)

#Significativo: 
#Count: Muert (En ambos, contrapuesta asi que no)
#Zero: Area, Muerta, labrar picar, picar+herbicidar

#Predict:
#pred <- predict( avg_ter,newdata = data.frame(sviv = i$sviv, smuer = i$smuer, salt = i$salt, salt_q = ssim = i$ssim,
#                                              shet = i$shet, shet_q = i$shet_q, sshap= i$sshap, sar = i$sar, 
#                                              sage = i$sage, sclus = i$sclus, Tractament = i$Tractament, 
#                                              Any = i$Any), type="response", se.fit = TRUE )

pred <- predict( avg_mec_zero, type="response" ) #It doest return se!!!

library(ggplot2)


ggplot(e, aes(x = area, y = pred)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "lm", formula = y ~ x , size = 1) +
  labs(x = "Area", y = "Abundancia calandria")

e$Tractament<-as.character(e$Tractament)

e$Tractament[which(e$Tractament == "Acontrol")]<-"Control"
e$Tractament[which(e$Tractament == "Alfals")]<-"Alfalfa"
e$Tractament[which(e$Tractament == "Curronar")]<-"Compactar"
e$Tractament[which(e$Tractament == "Llaurar")]<-"Labrar"
e$Tractament[which(e$Tractament == "Picar i herbicidar")]<-"Pic + Herb"

ggplot(e, aes(x = Tractament, y = pred)) +
  geom_point() +
  geom_boxplot() +
  theme_bw() +
  theme(text = element_text(size=23)) +
  labs(x = "\nTratamiento", y = "Abundancia calandria\n")

#NADA FUNCIONA. CREAR MODEL AVG GLM POISSON NORMAL CON RANDOM EFFECTS.
library(lme4)

#Categorical as factor
e$Zone<-as.factor(e$Zone)
e$Any<-as.factor(e$Any)
e$Tractament<-as.factor(e$Tractament)
e$Cluster<-as.factor(e$Cluster)
e$age<-as.factor(e$age)
#Scaled variables
e$sviv<-scale(e$Recob_plotViu)
e$smuer<-scale(e$Recob_plotMort)
e$salt<-scale(e$PromigAltura1Plot)
e$salt_q<-scale(I(e$PromigAltura1Plot^2))
e$ssim<-scale(e$Simpson)
e$shet<-scale(e$lev_ind)
e$shet_q<-scale(I(e$lev_ind^2))
e$sshap<-scale(e$shape)
e$sar<-scale(e$area)
e$sage<-scale(e$age)


mp<-glmer(Contatge ~ sviv + smuer + salt + ssim + shet + sshap + sar + sage
          + Cluster + Tractament + (1 | Zone) + (1| Any),
          data = e, family = poisson)

mec_glm<-dredge(mp, beta = "none", evaluate = TRUE)
save(mec_glm,file = "mec_glm.RData")

load("mec_glm.RData")
m<-get.models(mec_glm,subset = delta < 2)
avg_mec<-model.avg(m)
summary(avg_mec)

#Significant: Age, area, shape, tractament(picar+herb,picar,llaurar)
pred2 <- predict( avg_mec, type="response") 


ggplot(e, aes(x = area, y = pred2)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "loess", formula = y ~ x , size = 1) +
  labs(x = "Area", y = "Calandria")

ggplot(e, aes(x = age, y = pred2)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "loess", formula = y ~ x , size = 1) +
  labs(x = "Age", y = "Calandria")

ggplot(e, aes(x = shape, y = pred2)) +
  geom_point() +
  #geom_line() +
  geom_smooth(method = "loess", formula = y ~ x , size = 1) +
  labs(x = "Shape", y = "Calandria")

ggplot(e, aes(x = Tractament, y = pred2)) +
  geom_point() +
  geom_boxplot() +
  labs(x = "Tratamiento", y = "Calandria")



################################################################
#Simple glm
e$Tractament<-as.factor(e$Tractament)
mecal<-glm(Contatge ~ Tractament,family = "poisson",data=e)
summary(mecal)
pred <- predict(mecal,newdata = data.frame(Tractament=sort(unique(e$Tractament))),type="response",se.fit =TRUE )

lcl <- pred$fit - 1.96*pred$se.fit
lch <- pred$fit + 1.96*pred$se.fit

plot(pred$fit~sort(unique(e$Tractament)), ylim=c(0,2), main = "MECAL")
segments(x0=c(1:6),
         x1=c(1:6),
         y0=lcl,
         y1=lch)
unique(b$Tractament)

################################################################
#################################################################################
#Try mixed models!!!Random: Zone and Year (BUOED, MECAL, ALRUF)

library(glmmADMB)

e$Zone<-as.factor(e$Zone)
e$Any<-as.factor(e$Any)
e$Tractament<-as.factor(e$Tractament)
e$Cluster<-as.factor(e$Cluster)

g<-glmmadmb(Contatge ~ sviv + smuer + salt + ssim + shet + sshap + sar + sage +
            Cluster + Tractament  + (1 | Zone) + (1| Any), data = e, family = "poisson",
            zeroInflation = TRUE)

summary(g)

#Model averaging
m_mec_ran<-dredge(g, beta = "none", evaluate = TRUE)
save(m_mec_ran,file = "mec_dred_ran.RData")

load("mec_dred_ran.RData")
mec<-get.models(m_mec_ran,subset = delta < 2)
avg_mec<-model.avg(mec)
summary(avg_mec)
predm <- predict( avg_mec, type="response", se.fit = TRUE ) #It doest return se!!!!!!!!!!!!!



