library(lme4)
library(ggplot2)
library(tidyverse)

#Here the colnames and the way we are understud the variables are definaid 
datos_long_t1 <-read.csv("datos_long_t1.csv",header=T)
alldata<-data.frame(as.factor(datos_long_t1$ID_check), 
                    as.numeric(as.character(datos_long_t1$trail)), 
                    as.factor(datos_long_t1$Choise), 
                    as.numeric(as.character(datos_long_t1$reward)),
                    as.numeric(as.character(datos_long_t1$effort)), 
                    as.factor(datos_long_t1$condition), 
                    as.factor(datos_long_t1$GRUPO))
colnames(alldata)<-c('ID_check', "c.trial", 'Choise', "c.reward",'c.effort','condition','GRUPO')




#Rescaling
#Now the columns values are parametrized
numcols <- grep("^c\\.",names(alldata))
alldata.sc <- alldata
alldata.sc[,numcols] <- scale(alldata.sc[,numcols])



#1Modelo coparation------------------------------------------------------------

modelo_1a <-glmer(Choise ~ c.reward*condition*c.effort + (1|ID_check)
                  ,data=alldata.sc, 
                  family=binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl=list(maxfun=2e5)))

modelo_1b <-glmer(Choise ~ c.reward*condition*(c.effort^2) + (1|ID_check)
                  ,data=alldata.sc, 
                  family=binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl=list(maxfun=2e5)))

modelo_2a <- glmer(Choise ~ c.reward*condition + c.effort*condition + (1|ID_check)
                   ,data = alldata.sc,
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl = list(maxfun=2e5)))

modelo_2b <- glmer(Choise ~ c.reward*condition + (c.effort^2)*condition + (1|ID_check)
                   ,data = alldata.sc,
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl = list(maxfun=2e5)))


#---Change reward and effort as factor--------------------------------------------------

modelo_1a.2  <-glmer(Choise ~ reward*condition*effort + (1|ID_check)
                     ,data=alldata.sc, 
                     family=binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl=list(maxfun=2e5)))

modelo_1b.2 <-glmer(Choise ~ reward*condition*(effort^2) + (1|ID_check)
                    ,data=alldata.sc, 
                    family=binomial,
                    control = glmerControl(optimizer = "bobyqa",
                                           optCtrl=list(maxfun=2e5)))

modelo_2a.2 <- glmer(Choise ~ reward*condition + effort*condition + (1|ID_check)
                     ,data = alldata.sc,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun=2e5)))

modelo_2b.2 <- glmer(Choise ~ reward*condition +(effort^2)*condition + (1|ID_check)
                     ,data = alldata.sc,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun=2e5)))

model_comp <- anova(modelo_1a,modelo_1b,modelo_2a,modelo_2b,modelo_1a.2,modelo_1b.2,modelo_2a.2,modelo_2b.2)




#2Model comparation----------------------------------------------------------------------------------

modelo_A1 <-glmer(Choise ~ c.reward*condition*c.effort + (1+c.effort+c.reward|ID_check)
                  ,data=alldata.sc, 
                  family=binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl=list(maxfun=2e5)))

modelo_A1.a.D <-glmer(Choise ~ c.reward*condition*c.effort + (1+c.effort+c.reward||ID_check)
                      ,data=alldata.sc, 
                      family=binomial,
                      control = glmerControl(optimizer = "bobyqa",
                                             optCtrl=list(maxfun=2e5)))

modelo_B1 <-glmer(Choise ~ c.reward*condition*(c.effort^2) + (1+c.effort*c.reward|ID_check)
                  ,data=alldata.sc, 
                  family=binomial,
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl=list(maxfun=2e5)))

modelo_A2 <- glmer(Choise ~ c.reward*condition + c.effort*condition + (1+c.effort*c.reward*condition|ID_check)
                   ,data = alldata.sc,
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl = list(maxfun=2e5)))

modelo_B2 <- glmer(Choise ~ c.reward*condition + (c.effort^2)*condition + (1+c.effort+c.reward+condition|ID_check)
                   ,data = alldata.sc,
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl = list(maxfun=2e5)))

Modelo_C1 <- glmer(Choise ~ c.reward*condition*c.effort + (1+c.effort*condition+c.reward*condition|ID_check)
                   ,data = alldata.sc,
                   family = binomial,
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl = list(maxfun=2e5)))

#---Change reward and effort as factor--------------------------------------------------

modelo_A1.a  <-glmer(Choise ~ reward*condition*effort + (1+effort+reward|ID_check)
                     ,data=alldata.sc, 
                     family=binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl=list(maxfun=2e5)))

modelo_A1.a.dbl  <-glmer(Choise ~ reward*condition*effort + (1+effort+reward||ID_check)
                         ,data=alldata.sc, 
                         family=binomial,
                         control = glmerControl(optimizer = "bobyqa",
                                                optCtrl=list(maxfun=2e5)))

modelo_B1.b <-glmer(Choise ~ reward*condition*(effort^2) + (1+effort*reward|ID_check)
                    ,data=alldata.sc, 
                    family=binomial,
                    control = glmerControl(optimizer = "bobyqa",
                                           optCtrl=list(maxfun=2e5)))

modelo_A2.a <- glmer(Choise ~ reward*condition + effort*condition + (1+effort*reward*condition|ID_check)
                     ,data = alldata.sc,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun=2e5)))

modelo_B2.b <- glmer(Choise ~ reward*condition + (effort^2)*condition + (1+effort+reward+condition|ID_check)
                     ,data = alldata.sc,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun=2e5)))

model_comp_2 <-anova(modelo_A1,modelo_A1.a,modelo_A1.a.D,modelo_A1.a.dbl,modelo_A2,modelo_B1,modelo_B2,Modelo_C1)

