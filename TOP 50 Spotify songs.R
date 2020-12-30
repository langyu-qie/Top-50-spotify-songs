library(MASS)
library(car)
library(faraway)
spop<-read.csv("top50.csv")
head(spop)
pairs(spop)
#par(mfrow=c(2,3))
hist(spop$BPM)
hist(spop$Energy)
hist(spop$Valence)
hist(spop$Acousticness)
hist(spop$Speechiness)
hist(spop$Popularity)

#par(mfrow=c(2,3))
hist(log(spop$BPM))
hist(log(spop$Energy))
hist(log(spop$Valence))
hist(log(spop$Acousticness))
hist(log(spop$Speechiness))

fm1<-lm(log(Popularity)~.,data=spop)
summary(fm1)
anova(fm1)

plot(fm1)

#pred.fm2<-predict(fm2,data=spop)
#plot(pred.fm2,col="blue",type="l")
#lines(log(spop$Popularity))
#plot(log(pred.fm2),type="l",col="red",lty=3)
#lines(log(spop$Popularity),lty=2)
#lines(pred.fm2,col="blue",lty=1)

#high leverage
plot(fm1$fitted,fm1$res)
lev.Pop=hatvalues(fm1)
lev.Pop
stdres.Pop=rstandard(fm1)
stdres.Pop
plot(stdres.Pop^2,lev.Pop,xlab="squared standardized residual",ylab="leverage")
#identify(stdres.Pop^2,lev.Pop)
lines(c(-2,15),c(2*5/50,2*5/50))

#principle component
x.sc=scale(x)
x.pr=prcomp(x.sc)
print(x.pr)
summary(x.pr)
plot(x.pr)


#check colinearity between predictors,VIF>5,strong
vif(fm1)
pairs(spop)
cor(spop[1:5])


#stepAIC来选择模型
mod.all=lm(Popularity~1,data=spop)
mod.fw=stepAIC(mod.all,scope=list(upper=fm1),direction="forward")
mod.bw=stepAIC(fm1,scope=list(lower=mod.all),direction="backward")
mod.both=stepAIC(mod.all,scope=list(lower=mod.all,upper=fm1),direction="both")
summary(mod.fw)
fm2<-lm(Popularity~Valence+Energy+Acousticness,data=spop)#得到三个变量回归的模型结果
summary(fm2)


#boxcox
bc.spop= boxcox(fm1)
which.max(bc.spop$y)
lamada = bc.spop$x[54]
Popularity.bc = (spop$Popularity^lamada -1)/lamada
spopadd = spop
spopadd = spopadd[,-1]
spopadd= cbind(Popularity.bc,spopadd)
spop.bc.mod = lm(Popularity.bc~., data=spop)
summary(spop.bc.mod)
qqnorm(studres(spop.bc.mod ))  
qqline(studres(spop.bc.mod ))
plot(spop.bc.mod)



#QQplot
qqnorm(studres(fm1))  
qqline(studres(fm1))
plot(fm1)
qqnorm(studres(fm2))  
qqline(studres(fm2))
plot(fm2)
shapiro.test(studres(fm1))#w=0.983 接近1 所以student residual满足正态分布
test = spop$Popularity
ks.test(test+runif(length(test),-0.05,0.05),"pnorm",mean(test),sd(test))
qqPlot(fm1,simulate=TRUE,main="Q-Q Plot",labels=FALSE)
stdres.Pop=rstandard(fm2)
stdres.Pop
cooks.distance(fm2)
plot(cooks.distance(fm2))
outlierTest(fm1)
#从qqplot和outliertest中发现没有离群点

spopnew<-read.csv("top50new.csv")
head(spopnew)
fm2new<-lm(Popularity~Energy+Valence+Acousticness,data=spop)
summary(fm2new)
summary(fm2)

#boxcox
bc.spop2= boxcox(fm2new)
which.max(bc.spop2$y)
lamada2 = bc.spop2$x[54]
Popularity.bc2 = (spopnew$Popularity^lamada2 -1)/lamada2
spopadd2 = spopnew
spopadd2 = spopadd2[,-1]
spopadd2= cbind(Popularity.bc2,spopadd2)
spop.bc.mod2 = lm(Popularity.bc2~Energy+Valence+Acousticness, data=spopnew)
summary(spop.bc.mod2)
qqnorm(studres(spop.bc.mod2 ))  
qqline(studres(spop.bc.mod2 ))
plot(spop.bc.mod2)







