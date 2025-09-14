
## LMM --- Prosocial TASTK, modelanding the effect of 
## the effort in the Choise to work 
## by José Bórquez -- August 2025






rm(list=ls())

#set directory
setwd("C:/Users/Usuario/Documents/DATOS SEBA/Datos seba")

library(lme4)
library(ggplot2)

## read in data


hist(E[alldata$Choise == 2])

#Here the colnames and the way we are understud the variables are definaid 
datos_long_v2_trab <-read.csv("datos_long_v2_trab.csv",header=T)
alldata<-data.frame(as.factor(datos_long_v2_trab$ID_check), 
                    as.numeric(as.character(datos_long_v2_trab$trail)), 
                    as.factor(datos_long_v2_trab$Choise), 
                    as.factor(datos_long_v2_trab$reward),
                    as.factor(datos_long_v2_trab$effort), 
                    as.factor(datos_long_v2_trab$condition), 
                    as.factor(datos_long_v2_trab$GRUPO))
colnames(alldata)<-c('ID_check', "c.trial", 'Choise', "reward",'effort','condition','GRUPO')

#Rescaling
#Now the columns values are parametrized
numcols <- grep("^c\\.",names(alldata))
alldata.sc <- alldata
alldata.sc[,numcols] <- scale(alldata.sc[,numcols])

######################## Decision Models###########################

#Simple vs Interoception model
#Here are made the model

     #In this case, we view::

#lm is for reward an only reward
lm.reward <-glmer(Choise ~ reward*condition*effort + (1+effort+reward|ID_check)
                    ,data=alldata.sc, 
                    family=binomial,
                    control = glmerControl(optimizer = "bobyqa",
                                           optCtrl=list(maxfun=2e5)))

lm.reward.3 <-glmer(Choise ~ reward*condition + effort + (1+effort+condition|ID_check)
                  ,data=alldata.sc, 
                  family=binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl=list(maxfun=2e5)))

lm.reward.2 <-glmer(Choise ~ effort*condition + reward + (1+effort+condition|ID_check)
                  ,data=alldata.sc, 
                  family=binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl=list(maxfun=2e5)))




  #lm effort (numerico) con interacción de reward      
lm.effort <-glmer(Choise ~ c.effort*condition*reward + (1+c.effort+reward|ID_check)
                      ,data=alldata.sc, 
                      family=binomial,
                      control = glmerControl(optimizer = "bobyqa",
                                             optCtrl=list(maxfun=2e5)))

   #lm Reward factor  sin interacción con effort y este mismo como numeric. (sin reescalar)
lm.effort.2 <-glmer(Choise ~ c.effort*condition + reward + (1+c.effort+reward|ID_check)
                  ,data=alldata.sc, 
                  family=binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl=list(maxfun=2e5)))

   #lm Reward como numeric, sin interacción con effort y este como factor
lm.effort.3 <-glmer(Choise ~ effort*condition + c.reward + (1+effort+c.reward|ID_check)
                  ,data=alldata.sc, 
                  family=binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl=list(maxfun=2e5)))
    #lm effort y reward como factores
lm.effort.7 <-glmer(Choise ~ effort*condition*reward + (1+effort+reward|ID_check)
                  ,data=alldata.sc, 
                  family=binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl=list(maxfun=2e5)))


    #lm effort y reward como numericos (sin reescalar)
lm.effort.5 <-glmer(Choise ~ c.effort*condition*c.reward + (1+c.effort+c.reward|ID_check)
                    ,data=alldata.sc, 
                    family=binomial,
                    control = glmerControl(optimizer = "bobyqa",
                                           optCtrl=list(maxfun=2e5)))



   #se intento pero demoró excesivo
lm.effort.6 <-glmer(Choise ~ poly(effort, 2)*condition*reward + (1+effort+reward+condition|ID_check)
                    ,data=alldata.sc, 
                    family=binomial,
                    control = glmerControl(optimizer = "bobyqa",
                                           optCtrl=list(maxfun=2e5)))

#Effort and reward are interactive
lm.dec_full <- glmer(Choise ~ reward*effort*condition + (1+reward+effor|ID_check)
                   ,data=alldata.sc, 
                   family=binomial,
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl=list(maxfun=2e5)))



lmInt.dec_full <-glmer(Choise ~ c.reward*condition*effort*c.trial + (1+effort+c.reward|ID_check),
                       data=alldata.sc, 
                       family=binomial,
                       control = glmerControl(optimizer = "bobyqa",
                                              optCtrl=list(maxfun=2e5)))

ml.comp <- anova(lm.dec_full,lmInt.dec_full) # model comparison



  