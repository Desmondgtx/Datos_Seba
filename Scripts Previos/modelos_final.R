

rm(list=ls())


library(lme4)
library(tidyverse)
library(readxl)
library(car)
library(ggeffects)
library(emmeans)
library(performance)



glm <- read.csv("datos_long_glmm.csv", header = T)



alldata <- data.frame(as.factor(glm$sub),
                      as.factor(glm$decision),
                      as.factor(glm$grupo),
                      as.factor(glm$agent),
                      as.factor(glm$AIM_2),
                      as.numeric(as.character(glm$success)),
                      as.numeric(as.character(glm$tasa_fallo_other)),
                      as.numeric(as.character(glm$tasa_fallo_self)),
                      as.numeric(as.character(glm$zeros_OTHER)),
                      as.numeric(as.character(glm$zeros_SELF)),
                      as.numeric(as.character(glm$reward)),
                      as.numeric(as.character(glm$effort)),
                      as.numeric(as.character(glm$trail)),
                      as.numeric(as.character(glm$AIM_num)),
                      as.numeric(as.character(glm$AIM_3)),
                      as.numeric(as.character(glm$AIM_4))
)

colnames(alldata)<-c("sub", "decision", "grupo", "agent", "AIM_2", "success", "tasa_fallo_other", 
                     "tasa_fallo_self", "zeros_other", "zeros_self", "c.reward", "c.effort", "c.trail",
                     "c.AIM_num", "c.AIM_3", "c.AIM_4")


numcols <- grep("^c\\.",names(alldata))
alldata.sc <- alldata
alldata.sc[,numcols] <- scale(alldata.sc[,numcols])


alldata.sc <- subset(alldata.sc, zeros_other <= 25)
alldata.sc <- subset(alldata.sc, zeros_self <= 25)
alldata.sc <- subset(alldata.sc, decision!= 2)


#ESTE MODELO ES EL QUE SE DISCUTIO 
#EL LUNES QUE TENIA LA ESTRUCTURA MÁS COHERENTE
modelo_decision_4.1 <-glmer(decision ~ c.reward*agent*grupo + c.effort*agent*grupo + (1 + c.effort + c.reward|sub)
                            ,data=alldata.sc,
                            family=binomial,
                            control = glmerControl(optimizer = "bobyqa",
                                                   optCtrl=list(maxfun=2e5))) 

plot(ggpredict(modelo_decision_4.1, terms = c("c.effort", "agent", "grupo"))) +
  ggtitle("Modelo 4.1  Effort × Agent × Grupo") +
  ylab("Probabilidad predicha de decisión") +
  xlab("Nivel de esfuerzo (c.effort)") +
  theme_minimal()


modelo_AIM_3 <-glmer(decision ~ c.reward + c.effort*grupo*agent + (1 + c.effort + c.reward |sub)
                     ,data=alldata.sc,
                     family=binomial,
                     control = glmerControl(optimizer = "bobyqa",
                                            optCtrl=list(maxfun=2e5)))
vif_3 <- check_collinearity(modelo_AIM_3)

plot(ggpredict(modelo_AIM_3, terms = c("c.effort", "agent", "grupo"))) +
  ggtitle("Modelo 3  Effort × Agent × Grupo") +
  ylab("Probabilidad predicha de decisión") +
  xlab("Nivel de esfuerzo (c.effort)") +
  theme_minimal()



#Estos modelos aún no estoy seguro si estan bien o responden a lo que quería el seba 
alldata.sc_a <- subset(alldata.sc, decision!= 0)

modelo_succes_21 <-glmer(success ~ c.reward*agent*grupo + agent*c.effort*grupo + (1|sub)
                         ,data=alldata.sc_a,
                         family=binomial,
                         control = glmerControl(optimizer = "bobyqa",
                                                optCtrl=list(maxfun=2e5)))

modelo_succes_31 <-glmer(success ~ c.reward*agent*grupo + agent*c.effort*grupo + (1 + c.effort + c.reward|sub)
                         ,data=alldata.sc_a,
                         family=binomial,
                         control = glmerControl(optimizer = "bobyqa",
                                                optCtrl=list(maxfun=2e5)))

mm<- ggpredict(modelo_succes_31, c("grupo","agent")) %>% plot()
mm



#post-hoc
pairs()
try_up <- emmeans(modelo_AIM_3, ~ agent)
ttp <- pairs(try_up)
print(ttp)


emmip(modelo_AIM_3, type = decision | c.effort)





# Definimos los niveles exactos de tu escala
niveles_esfuerzo <- list(c.effort = c(1, 2, 3, 4))

# Calculamos las medias marginales
# La fórmula "~ grupo | agent + c.effort" significa:
# "Quiero comparar los GRUPOS, desglosado por cada combinación de AGENTE y ESFUERZO"
em_posthoc <- emmeans(modelo_AIM_3, 
                      ~ grupo * agent * c.effort, 
                      at = niveles_esfuerzo,
                      type = "response") # Para ver probabilidades en la salida (opcional)

# Ejecutamos las comparaciones por pares (pairwise)
# Esto comparará Grupo A vs Grupo B en Esfuerzo 1 (Agente 1), luego en Esfuerzo 2, etc.
contrastes <- pairs(em_posthoc)

# Ver resultados
print(contrastes)

# Opcional: Para ver intervalos de confianza de la diferencia (en escala log-odds)
confint(contrastes)



