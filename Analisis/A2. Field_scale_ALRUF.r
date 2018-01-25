
#FIELD SCALE_PERDIZ (ALRUF)


setwd("C:/Users/ana.sanz/Documents/Datos/Datos barbechos arrendados/Variables")
f<-read.csv("Variables.csv",sep = ",",header=TRUE,fill = TRUE)
f<-f[,which(colnames(f) %in% c("IDfinca","Codi_Finca","Any","CF_A","Subzone","Zone","EspecieObj","Contatge",
                               "Recob_plotViu","Recob_plotMort","PromigAltura1Plot","Cluster",
                               "Simpson","lev_ind","age","area","shape","Tractament"))]


a<-f[which(f$EspecieObj == "ALRUF"),]
a<-a[complete.cases(a),]
a<-a[-which(duplicated(a[,2:15])),]
which(duplicated(a$CF_A)) 

#Change treatment control so that it is the intercept
a$Tractament<-as.character(a$Tractament)
a$Tractament[which(a$Tractament == "Control")]<-"Acontrol"

################################################################################
#1. DATA EXPLORATION

#Relation response - Predictors
ggplot(a,aes(Recob_plotViu,Contatge))+geom_point()+geom_smooth()+theme_classic()
ggplot(a,aes(Recob_plotMort,Contatge))+geom_point()+geom_smooth()+theme_classic() 
ggplot(a,aes(PromigAltura1Plot,Contatge))+geom_point()+geom_smooth()+theme_classic() #???quadratic?
ggplot(a,aes(Simpson,Contatge))+geom_point()+geom_smooth()+theme_classic()
ggplot(a,aes(lev_ind,Contatge))+geom_point()+geom_smooth()+theme_classic() #quadratic?
ggplot(a,aes(area,Contatge))+geom_point()+geom_smooth()+theme_classic()
ggplot(a,aes(shape,Contatge))+geom_point()+geom_smooth()+theme_classic()
ggplot(a,aes(age,Contatge))+geom_point()+geom_smooth()+theme_classic()#Continuous. categorical?

ggplot(a,aes(as.factor(Cluster),Contatge))+geom_boxplot()+theme_classic()
ggplot(a,aes(as.factor(age),Contatge))+geom_boxplot()+theme_classic() #categorical

ggplot(a,aes(as.factor(Tractament),Contatge))+geom_boxplot()+theme_classic()

ggplot(a,aes(as.factor(Subzone),Contatge))+geom_boxplot()+theme_classic() 
ggplot(a,aes(as.factor(Zone),Contatge))+geom_boxplot()+theme_classic()#Possible effect of zone
ggplot(a,aes(as.factor(Any),Contatge))+geom_boxplot()+theme_classic() #No year effect

#CORRELATION BETWEEN VARIABLES (GENÉRICO)
cor(a$Recob_plotViu,a$PromigAltura1Plot)
cor(a$Simpson,a$lev_ind)
cor(a$age,g$area)
#Es posible que haya alta correlación entre las variables de vegetación y el tratamiento 
ggplot(a,aes(as.factor(Tractament),Recob_plotViu))+geom_boxplot()+theme_classic() #Correlation (más alfalfa y control)
ggplot(a,aes(as.factor(Tractament),Recob_plotMort))+geom_boxplot()+theme_classic() #Correlation (más picar)
ggplot(a,aes(as.factor(Tractament),PromigAltura1Plot))+geom_boxplot()+theme_classic() #Some correlation (más alfalfa y control),not much
ggplot(a,aes(as.factor(Tractament),Simpson))+geom_boxplot()+theme_classic() 

#RESPONSE

hist(a$Contatge)#Many 0: 445/483; 38 presences; MayorÃ­a en SI (OR)
length(which(a$Contatge == 0))/451*100 #92.23
xtabs(~ Contatge + Subzone,a)

#CHECK ASSUMPTIONS, QUADRATIC EFFECTS, OVERDISPERSION

a<-a[,c(3,6:18)]

#I start by doing a glm poisson model with all covariates and checking
#assumptions:
mp1<-glm(Contatge ~ Recob_plotViu + PromigAltura1Plot + Recob_plotMort + Simpson + lev_ind + area + shape + as.factor(Cluster)
         + as.factor(age) + as.factor(Tractament),
         data = a,
         family = poisson)

mp2<-glm(Contatge ~ Recob_plotViu + PromigAltura1Plot + Recob_plotMort + I(PromigAltura1Plot^2) + Simpson + lev_ind + 
           area + shape + as.factor(Cluster) + as.factor(age) + as.factor(Tractament),
         data = a,
         family = poisson)

mp3<-glm(Contatge ~ Recob_plotViu + PromigAltura1Plot + Recob_plotMort + Simpson + lev_ind + 
           I(lev_ind^2) + area + shape + as.factor(Cluster) + as.factor(age) + as.factor(Tractament),
         data = a,
         family = poisson)

mp4<-glm(Contatge ~ Recob_plotViu + Recob_plotMort + PromigAltura1Plot + I(PromigAltura1Plot^2) + Simpson + lev_ind + 
           I(lev_ind^2) + area + shape + as.factor(Cluster) + as.factor(age) + as.factor(Tractament),
         data = a,
         family = poisson)
AIC(mp1,mp2,mp3,mp4) #AIC better without quadratic effects
summary(mp1) # Dev/df = 0.4; UNDERDISPERSION????
#In poisson dist: variance = mean
m<-mean(a$Contatge)
v<-var(a$Contatge)
ratio<-v/m
ratio    # r=1.29 so we have overdispersion (>1) (but underdispersion in d/df?)

par(mfrow=c(2,2))
plot(mp1) #Not normal, not linear, not homogeneous

#Residuals against predictors to check homogeneity

ggplot(a,aes(Recob_plotViu,residuals(mp3)))+geom_point()+geom_smooth() #Homo?
ggplot(a,aes(PromigAltura1Plot,residuals(mp3)))+geom_point()+geom_smooth() #Not homo
ggplot(a,aes(area,residuals(mp3)))+geom_point()+geom_smooth() #Not homo
ggplot(a,aes(Subzone,rstandard(mp3)))+geom_boxplot()+theme_classic() #Mean around 0...nbut more effect of zone
                                                                      #than for other species?
############################################################################################
#2. CHOOSE A MODEL STRUCTURE

#There is a lot of 0 BUT not overdispersion: use zero inflated

library(pscl) 
#Scale variables 
a$sviv<-scale(a$Recob_plotViu)
a$smuer<-scale(a$Recob_plotMort)
a$salt<-scale(a$PromigAltura1Plot)
a$salt_q<-scale(I(a$PromigAltura1Plot^2))
a$ssim<-scale(a$Simpson)
a$shet<-scale(a$lev_ind)
a$shet_q<-scale(I(a$lev_ind^2))
a$sshap<-scale(a$shape)
a$sar<-scale(a$area)
a$sage<-scale(a$age)
a$sclus<-scale(as.numeric(a$Cluster))
a$Tractament<-factor(a$Tractament) #.First as factor, take into account when unscaling!
a$strat<-scale(as.numeric(a$Tractament))
#Check from here
#Model mp1 to check the zero part of the model (cuadratic heterogeneity)

zip1<-zeroinfl(Contatge ~ sviv + smuer + salt + ssim + shet + sshap + sar + sage + 
                 sclus + strat, 
               dist="poisson", link = "logit",data = a)

summary(zip1) #In zero part, significant area, het, altura, sviv(?), age(?)

#Choose 0 part

zip2<-zeroinfl(Contatge ~ sviv + smuer + salt + ssim + shet + sshap + sar + sage + 
                 sclus + strat|
                 sar + shet + salt + sviv + sage,
               dist="poisson", link = "logit",data = a)

zip3<-zeroinfl(Contatge ~ sviv + smuer + salt + ssim + shet + sshap + sar + sage + 
                 sclus + strat|
                 sar + shet + salt + sage,
               dist="poisson", link = "logit",data = a)

zip4<-zeroinfl(Contatge ~ sviv + smuer + salt + ssim + shet + sshap + sar + sage + 
                 sclus + strat|
                 sar + shet + salt,
               dist="poisson", link = "logit",data = a)

zip5<-zeroinfl(Contatge ~ sviv + smuer + salt + ssim + shet + sshap + sar + sage + 
                 sclus + strat|
                 sar + shet,
               dist="poisson", link = "logit",data = a)

AIC(zip2,zip3,zip4,zip5) # Mejor con todas dentro (zip2) y sin sviva y age (zip4)
                          #Cojo mas parsimonious zip4

summary(zip4)
#Compare with poisson alone 
AIC(mp1,zip4) #Much better zero inflated!

#Use vuong test to check if ZIP or poisson is better
mp<-glm(Contatge ~ sviv + smuer + salt + ssim + shet + sshap + sar + sage + 
          sclus + strat,
        family = "poisson", data = a)
vuong(mp,zip4) # En 2/3 test, model 2 da mejor fit que poisson (ZIP MEJOR QUE POISSON)

##################################################################################
#3. MODEL SELECTION

library(MuMIn)
options(na.action=na.fail)

a$Cluster<-as.factor(a$Cluster)
zip4_notrat<-zeroinfl(Contatge ~ sviv + smuer + salt + ssim + shet + sshap + sage + 
                 Cluster|
                 shet + salt,
               dist="poisson", link = "logit",data = a, offset = log(a$area)) #Notrat and areaoffset

m_al<-dredge(zip4, beta = "none", evaluate = TRUE )
#save(m_al,file = "al_dred.RData")
load("al_dred.RData")
a<-get.models(m_al,subset = delta < 2)
avg_al<-model.avg(a)
save(avg_al,file = "avg_al.RData")

m_al_notrat<-dredge(zip4_notrat, beta = "none", evaluate = TRUE )
#save(m_al_notrat,file = "al_dred_notrat.RData")
load("m_al_notrat.RData")
a<-get.models(m_al_notrat, subset = delta < 2)
avg_al<-model.avg(a) #No funciona????
save(avg_al, file = "avg_al.RData")

#Simple glm
a$Tractament<-as.factor(a$Tractament)
alruf<-glm(Contatge ~ Tractament,family = "poisson",data=a)
summary(alruf)
pred <- predict(alruf,newdata = data.frame(Tractament=sort(unique(a$Tractament))),type="response",se.fit =TRUE )

lcl <- pred$fit - 1.96*pred$se.fit
lch <- pred$fit + 1.96*pred$se.fit

plot(pred$fit~sort(unique(a$Tractament)), ylim=c(0,1.5),main = "ALRUF")
segments(x0=c(1:6),
         x1=c(1:6),
         y0=lcl,
         y1=lch)

#Con binomial
a$Contatge[a$Contatge == 2]<-1
a$Contatge[a$Contatge == 3]<-1
alruf<-glm(Contatge ~ Tractament,family = "binomial",data=a)
summary(alruf)
pred <- predict(alruf,newdata = data.frame(Tractament=sort(unique(a$Tractament))),type="response",se.fit =TRUE )

lcl <- pred$fit - 1.96*pred$se.fit
lch <- pred$fit + 1.96*pred$se.fit

plot(pred$fit~sort(unique(a$Tractament)),ylim = c(0,0.5),main = "ALRUF")
segments(x0=c(1:6),
         x1=c(1:6),
         y0=lcl,
         y1=lch)

#####################################################################################
#NADA FUNCIONA. CREAR MODEL AVG GLM POISSON NORMAL CON RANDOM EFFECTS. 
library(lme4)

#Categorical as factor
a$Zone<-as.factor(a$Zone)
a$Any<-as.factor(a$Any)
a$Tractament<-as.factor(a$Tractament)
a$Cluster<-as.factor(a$Cluster)
a$age<-as.factor(a$age)
#Scaled variables
a$sviv<-scale(a$Recob_plotViu)
a$smuer<-scale(a$Recob_plotMort)
a$salt<-scale(a$PromigAltura1Plot)
a$salt_q<-scale(I(a$PromigAltura1Plot^2))
a$ssim<-scale(a$Simpson)
a$shet<-scale(a$lev_ind)
a$shet_q<-scale(I(a$lev_ind^2))
a$sshap<-scale(a$shape)
a$sar<-scale(a$area)
a$sage<-scale(as.numeric(a$age))

a$sclus<-scale(as.numeric(a$Cluster))
a$Tractament<-factor(a$Tractament) #.First as factor, take into account when unscaling!
a$strat<-scale(as.numeric(a$Tractament))
#Check from here


mp<-glmer(Contatge ~ sviv + smuer + salt + ssim + shet + sshap + sar + sage + 
            sclus + strat + (1| Any) + (1| Zone),
          data = a, family = poisson)

mp_notrat<-glmer(Contatge ~ sviv + smuer + salt + ssim + shet + sshap + sage + 
            (1| Any) + (1| Zone),
          data = a, family = poisson, offset = log(a$area)) #notrat

al_glm<-dredge(mp, beta = "none", evaluate = TRUE)
save(al_glm,file = "l_glm.RData")
#load("buoed_dred_ran.RData")
m<-get.models(al_glm,subset = delta < 2)
avg_al<-model.avg(m)
summary(avg_al)
pred2 <- predict( avg_al, type="response", se.fit = TRUE )

al_glm<-dredge(mp_notrat, beta = "none", evaluate = TRUE)
save(al_glm,file = "al_glm_notrat.RData")
#load("buoed_dred_ran.RData")
m<-get.models(al_glm,subset = delta < 2)
avg_al<-model.avg(m)
summary(avg_al) #Notrat significativo Viva


