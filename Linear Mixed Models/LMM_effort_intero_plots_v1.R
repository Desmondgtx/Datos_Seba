
rm(list=ls())

#set directory
setwd('/Users/Sebastian/Dropbox/OSF_repository/Interoception_study/To_upload')

library(lme4)
library(ggplot2)


## read in data
merge.data<-read.csv('/Users/Sebastian/Dropbox/OSF_repository/Interoception_study/To_upload/effort_intero_Bdata.CSV',header=T)
alldata<-data.frame(as.factor(merge.data$sub),as.factor(merge.data$decision),as.numeric(as.character(merge.data$reward)),as.numeric(as.character(merge.data$effort)),as.factor(merge.data$agent),as.numeric(as.character(merge.data$Interoception)))
colnames(alldata)<-c('sub','decision', 'c.reward','c.effort','agent','c.Interoception')

#Rescaling
numcols <- grep("^c\\.",names(alldata))
alldata.sc <- alldata
alldata.sc[,numcols] <- scale(alldata.sc[,numcols])

######################## Decision Models###########################

#Simple vs Interoception model
lm.dec_full <-glmer(decision ~ c.reward*c.effort*agent + (1+c.effort+c.reward|sub),data=alldata.sc, family=binomial,control = glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))
lmInt.dec_full <-glmer(decision ~ c.reward*agent*c.effort*c.Interoception + (1+c.effort+c.reward|sub),data=alldata.sc, family=binomial,control = glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))

ml.comp <- anova(lm.dec_full,lmInt.dec_full) # model comparison

################### Plots with observed data #######################

data.plot <-read.csv('/Users/Sebastian/Dropbox/OSF_repository/Interoception_study/To_upload/for_plots_Intero_rew.CSV',header=T)

#Reorganise data
rew <- data.frame(Prop_reward = c(data.plot[,"SELF_REW1"], data.plot[,"SELF_REW2"], data.plot[,"SELF_REW3"], data.plot[,"SELF_REW4"], data.plot[,"SELF_REW5"], data.plot[,"OTHER_REW1"], data.plot[,"OTHER_REW2"], data.plot[,"OTHER_REW3"], data.plot[,"OTHER_REW4"], data.plot[,"OTHER_REW5"]), Condition=c(rep("Self",305),rep("Other",305)),Rew_level=c(rep("Rew1",61),rep("Rew2",61),rep("Rew3",61),rep("Rew4",61),rep("Rew5",61),rep("Rew1",61),rep("Rew2",61),rep("Rew3",61),rep("Rew4",61),rep("Rew5",61)), Interoception=c(data.plot[,"Interoception"],data.plot[,"Interoception"],data.plot[,"Interoception"],data.plot[,"Interoception"],data.plot[,"Interoception"],data.plot[,"Interoception"],data.plot[,"Interoception"],data.plot[,"Interoception"],data.plot[,"Interoception"],data.plot[,"Interoception"]))
rew$Condition <- factor(rew$Condition,
                        levels = c("Self", "Other"))


int.selfother <-data.frame(Prop = c(data.plot[,"PropSelf"], data.plot[,"PropOther"]),Condition=c(rep("Self",61),rep("Other",61)), Interoception=c(data.plot[,"Interoception"],data.plot[,"Interoception"]))
int.selfother$Condition <- factor(int.selfother$Condition,
                                  levels = c("Self", "Other"))

#2-way interaction plot
agentplotInt <- ggplot(int.selfother,aes(x=Interoception, y = Prop, color=Condition)) + 
  geom_point(size=3) +
  geom_smooth(method = lm,level=0.70,aes(fill=Condition)) +
  scale_color_manual(values=c('red','blue'))+
  theme_classic() + labs(x = "Interoception" , y = "Proportion of choice")+
  theme(axis.title = element_text(size=20),axis.text =element_text(size=18),axis.line = element_line(size = 1, linetype = "solid"),legend.text = element_text(size=18),legend.title = element_blank(),legend.position="top")

# 3-way Interaction Reward*Interoception*Agent plot

rewSelf  <- subset(rew, Condition!="Other")
rewOther <- subset(rew, Condition!="Self")

rewplotSelf <- ggplot(rewSelf,aes(x=Interoception, y=Prop_reward,color=Rew_level,shape=Rew_level)) + 
  geom_point(size=3) +
  geom_smooth(method = lm,level=0.70,aes(fill=Rew_level)) +
  scale_color_manual(values=c("lightcoral","tomato","red1","red3","red4"))+
  scale_fill_manual(values=c("lightpink1","pink","pink1","pink2","pink3"))+
  theme_classic() + labs(x = "Interoception" , y = "Proportion of choice")+
  theme(axis.title = element_text(size=20),axis.text =element_text(size=18),axis.line = element_line(size = 1, linetype = "solid"),legend.text = element_text(size=18),legend.title = element_blank(),legend.position="top")


rewplotOther <- ggplot(rewOther,aes(x=Interoception, y=Prop_reward,color=Rew_level,shape=Rew_level)) + 
  geom_point(size=3) +
  geom_smooth(method = lm,level=0.70,aes(fill=Rew_level)) +
  scale_color_manual(values=c("blue","blue3","blue4","royalblue4","navyblue"))+
  scale_fill_manual(values=c("skyblue1","skyblue2","steelblue1","steelblue2","steelblue3"))+
  theme_classic() + labs(x = "Interoception" , y = "Proportion of choice")+
  theme(axis.title = element_text(size=20),axis.text =element_text(size=18),axis.line = element_line(size = 1, linetype = "solid"),legend.text = element_text(size=18),legend.title = element_blank(),legend.position="top")


########################## Self/Other betas ##########################################
#subdata for self and other
data.self<- subset(alldata, agent!="2")
data.other<- subset(alldata, agent!="1")

#rescaling 
numcols <- grep("^c\\.",names(data.self))
data.self.sc <- data.self
data.self.sc[,numcols] <- scale(data.self.sc[,numcols])

numcols <- grep("^c\\.",names(data.other))
data.other.sc <- data.other
data.other.sc[,numcols] <- scale(data.other.sc[,numcols])

#self and other models
lmself.dec_full <-glmer(decision ~ c.reward*c.effort + (1+c.effort+c.reward|sub),data=data.self.sc, family=binomial,control = glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))
lmother.dec_full <-glmer(decision ~ c.reward*c.effort + (1+c.effort+c.reward|sub),data=data.other.sc, family=binomial,control = glmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=2e5)))

#extracting bettas
indvself <- coef(lmself.dec_full)
indvother <- coef(lmother.dec_full)


################### Correlations and plots #######################

corr.data <- read.csv('/Users/Sebastian/Dropbox/OSF_repository/Interoception_study/To_upload/BehaviourCorr.CSV',header=T)

int_rewdiff <- cor.test(corr.data$Interoception, corr.data$rewdiff, method=c("pearson"))
int_rewself <- cor.test(corr.data$Interoception, corr.data$rew_self, method=c("pearson"))
int_rewother <- cor.test(corr.data$Interoception, corr.data$rew_other, method=c("pearson"))

int_effdiff <- cor.test(corr.data$Interoception, corr.data$effdiff, method=c("pearson"))
int_effself <- cor.test(corr.data$Interoception, corr.data$eff_self, method=c("pearson"))
int_effother <- cor.test(corr.data$Interoception, corr.data$eff_other, method=c("pearson"))

# Scatter plots with extracted slopes

#self reward plot
plot.selfrew <- ggplot(corr.data,aes(x=Interoception, y=rew_self)) + 
  geom_point(size=5, color="red") +
  geom_smooth(method = lm,linetype="dashed", color="red", fill="lightpink") +
  theme_classic() + labs(x = "Interoception" , y = "Sensitivity for Self Rewards")+
  theme(axis.title = element_text(size=20),axis.text =element_text(size=18),axis.line = element_line(size = 1, linetype = "solid"),legend.text = element_text(size=18),legend.title = element_blank())

#other reward plot
plot.otherrew <- ggplot(corr.data,aes(x=Interoception, y=rew_other)) + 
  geom_point(size=5, color="blue") +
  geom_smooth(method = lm,linetype="dashed", color="blue", fill="dodgerblue2") +
  theme_classic() + labs(x = "Interoception" , y = "Sensitivity for Other Rewards")+
  theme(axis.title = element_text(size=20),axis.text =element_text(size=18),axis.line = element_line(size = 1, linetype = "solid"),legend.text = element_text(size=18),legend.title = element_blank())

#difference reward plot
plot.diffrew <- ggplot(corr.data,aes(x=Interoception, y=rewdiff)) + 
  geom_point(size=5, color="purple") +
  geom_smooth(method = lm,linetype="dashed", color="purple", fill="plum1") +
  theme_classic() + labs(x = "Interoception" , y = "Sensitivity for Self - Other Rewards")+
  theme(axis.title = element_text(size=20),axis.text =element_text(size=18),axis.line = element_line(size = 1, linetype = "solid"),legend.text = element_text(size=18),legend.title = element_blank())

