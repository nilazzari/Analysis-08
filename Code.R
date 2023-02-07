library(foreign)
library(survival)
library(eha)
library(lme4)
coca <- read.csv("data_cocaine.csv", header=TRUE, sep=",")
head(coca)
table(coca$CENSOR) # KM function for two groups, i.e. 0=non-smokers; 1=smokers 
table(coca$EARLYMJ) # time from age 17 to the first experience 
coca$dur<-coca$COKEAGE-16
summary(coca$dur)
surv0<-Surv(coca$dur,1-as.numeric(coca$CENSOR)) # KM function
plot(surv0)
surv1 <- survfit(Surv(coca$dur,1-as.numeric(coca$CENSOR))~as.factor(coca$EARLYMJ), data=coca)
plot(surv1,col=c("red","blue"), conf.int=TRUE)
legend("bottomleft",c("non-smokers","smokers"),lty=1,col=c("red","blue"), cex = 0.5)
summary(surv1)

# Cumulative Hazards
plot(surv1,col=c("red","blue"), fun="cumhaz", conf.int=TRUE)
legend("topleft",c("non-smokers","smokers"),lty=1,col=c("red","blue"), cex = 0.5)

# Test differences between smokers and non-smokers
logrank(surv0,group=coca$EARLYMJ)

# Parametric modelling: Weibull
weib1 <-  phreg(Surv(dur,1-CENSOR)~
                   BIRTHYR+I(EARLYMJ==1), dist="weibull", data=coca)
weib1
piecew <- piecewise(0, coca$dur, 1-coca$CENSOR, c(5,10,15,20,22)) # Base risk distribution
piecew
plot(c(0,5,10,15,20,22),
     piecew$intensity,type="s",lwd=1, col="blue",
     xlab="months since age 17", ylab="hazard function")

# Cox model with time-varying variable
# Dataset splitting, the first including individual variables only
dati1 <- coca[,c(1,4,5,6,7,9,11,13,15)]
head(dati1)
dati2 <- coca[,c(1,2,3,8,10,12,14,16)] # the second with time variables
head(dati2)

# we have info on the timing of smoking, but we need to code it equal to missing
#when the episode is before age 17, and use the time variable starting from age 17
dati2$MJAGE <- ifelse(dati2$MJAGE<17,NA,dati2$MJAGE-16)
dati2$MJAGE <- ifelse(dati2$MJAGE>dati2$dur,NA,dati2$MJAGE)
# creating the new dataset including time-dependent variables
dati3 <- tmerge(data1=dati1, data2=dati2,id=ID,
                event=event(dur,1-CENSOR),
                tstart=0,tstop=dur, mjtime=tdc(MJAGE))
head(dati3)

# Semiparametric modelling: Cox model
cox2 <-  coxph(Surv(tstop-tstart,event)~
                 BIRTHYR+I(EARLYMJ==1)+mjtime, data=dati3)
summary(cox2)

# Fitting frailty models
cox3 <-  coxph(Surv(dur,1-CENSOR)~
                 EARLYOD+EARLYMJ+RURAL, data=coca)
summary(cox3)

# Univariate frailty model
cox4 <-  coxph(Surv(dur,1-CENSOR)~
                 EARLYOD+EARLYMJ+RURAL+frailty(ID), data=coca)
summary(cox4)
cox.zph(cox4)

# Shared frailty model
cox5 <-  coxph(Surv(dur,1-CENSOR)~
                 EARLYOD+EARLYMJ+RURAL+frailty(BIRTHYR), data=coca)
summary(cox5)
cox.zph(cox5)

# Multilevel modelling
user <- coca[-which(coca$CENSOR==1),]
hist(user$COKEAGE)
nopool <- lm(COKEAGE ~ EARLYMJ+EARLYOD+RURAL+as.factor(BIRTHYR)-1, data=user) # Unpooled model
summary(nopool)
me <- lmer(COKEAGE ~ EARLYMJ+EARLYOD+RURAL+(1|BIRTHYR), data=user,REML=FALSE) # Random intercept model
summary(me)
anova(me,nopool) # comparison
me2 <- lmer(COKEAGE ~ EARLYMJ+EARLYOD+RURAL+(1|BIRTHYR), data=user) #Inter-class correlation
summary(me2)
pool <- lm(COKEAGE ~ EARLYMJ+EARLYOD+RURAL, data=user) # Pooled model
summary(pool)
n <- c (0, 4, 10)
coke.range <- range (user$COKEAGE[!is.na(match(user$BIRTHYR,n))])
uniq <- unique(user$BIRTHYR)
options (digits=2)
a.hat.M <- fixef(me)[1] + ranef(me)$BIRTHYR
b.hat.M <- fixef(me)[2]+ fixef(me)[4]*1
for (j in n){
  index <- which(uniq==j)
  plot (user$EARLYMJ[user$BIRTHYR==j], user$COKEAGE[user$BIRTHYR==j], xlim=c(-.05,1.05), ylim=coke.range)
  curve (coef(nopool)[index+4] + coef(nopool)[1]*x+coef(nopool)[3], lty=2, col="blue", add=TRUE)
  curve (coef(pool)[1] + coef(pool)[2]*x+coef(pool)[4], lty=2, col="red", add=TRUE)
  curve (a.hat.M[index,] + b.hat.M*x , lwd=1, col="black", add=TRUE)
}