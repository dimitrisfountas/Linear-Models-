data<-read.csv(file=prestige, header=TRUE)
head(prestige)
profs<-prestige$Professions
edu<-prestige$education
inc<-prestige$income
women<-prestige$women
pres<-prestige$prestige
type<-prestige$type
type_freq=table(type)
type_freq
profs_freq=table(profs)
barplot(type_freq,main="Type of Worker", xlab="Number of workers", col="black")
pie(type_freq, main="Type of worker", col=c("grey10", "grey20", "grey30"))
par(mfrow=c(1,2))
hist(edu,main="Education")
hist(inc, main="Income")
hist(women, main="Women")
hist(pres, main="Prestige")
boxplot(edu,main="Education")
boxplot(inc, main="Income")
boxplot(women, main="Women")
boxplot(pres, main="Prestige")
qqnorm(edu)
 qqline(edu)
qqnorm(inc)
qqline(inc)
qqnorm(women)
qqline(women)
qqnorm(pres)
qqline(pres)
shapiro.test(edu)
shapiro.test(inc)
shapiro.test(women)
shapiro.test(pres)
mu=mean(pres)
mu
variance=var(pres)
variance
sd=sd(pres)
sd
median(edu)
quantile(edu)
median(inc)
quantile(inc)
median(women)
quantile(women)
min(inc)
incom<-which.min(inc)
profs[incom]
max(inc)
inco<-which.max(inc)
profs[inco]
min(pres)
prestig<-which.min(pres)
profs[prestig]
max(pres)
prest<-which.max(pres)
profs[prest]
plot(pres,edu,
     main="Scatter Plot",
     preslab="Prestige",
     edulab="Education")
plot(pres,inc,
     main="Scatter Plot",
     preslab="Prestige",
     inclab="Income")
y<-prestige$pres
x<-prestige$edu
reg<-lm(y~x) 
summary(reg)
reg$coefficients
reg$residuals
reg$fitted.values
plot(x,y,
     main="Scatterplot F.R.L.",
     xlab="Education",
     ylab="Prestige")
lines(x,reg$fitted.values)
x<-prestige$inc
reg<-lm(y~x) 
summary(reg)
reg$coefficients
reg$residuals
reg$fitted.values
plot(x,y,
     main="Scatterplot F.R.L.",
     xlab="Income",
     ylab="Prestige")
lines(x,reg$fitted.values)
reg1=lm(pres~inc)
reg1$coefficients
reg1$residuals
reg1$fitted.values
z=pres
w=inc
plot(w,z,
     main="Scatterplot F.R.L.",
     xlab= "Prestige",
     ylab="income")
lines(w,reg1$fitted.values)
shapiro.test(edu)
shapiro.test(inc)
shapiro.test(pres)
x<-edu
t<-sum((x-mean(x))^2)
b1<-sum((x-mean(x))*(y-mean(y)))/t
b0<-mean(y)-b1*mean(x)
yhat<-b0+b1*x
ehat<-y-yhat
sigma2hat<-sum(ehat^2)/(100)
sigmahat<-sqrt(sigma2hat)
sigma2_b0<-sigma2hat*(1/100+mean(x)^2/t)
std_b0=sqrt(sigma2_b0)
sigma2_b1<-sigma2hat/t
std_b1=sqrt(sigma2_b1)
c = 0
t_stat<-(b1-c)/std_b1    
t_stat  
a<-0.05
qt_a<-qt(1-a, 100)
qt_a
t<-sum((w-mean(w))^2)
b1<-sum((w-mean(w))*(y-mean(y)))/t
b0<-mean(y)-b1*mean(w)
yhat<-b0+b1*w
ehat<-y-yhat
n<-102
sigma2hat<-sum(ehat^2)/(n-2)
sigmahat<-sqrt(sigma2hat)
sigma2_b0<-sigma2hat*(1/n+mean(w)^2/t)
std_b0=sqrt(sigma2_b0)
sigma2_b1<-sigma2hat/t
std_b1=sqrt(sigma2_b1)
c = 0
t_stat<-(b1-c)/std_b1    
t_stat  
a<-0.05
qt_a<-qt(1-a, n-2)
qt_a
reg=lm(pres~edu)
reg1=lm(pres~inc)
confint(reg, level=0.90)
confint(reg1, level=0.90)
summary(reg)
summary(reg1)
regg=lm( pres ~ edu + inc )
summary(regg)
confint(regg)
futurex<-10 
futurew<-4000
future<-data.frame(edu=futurex,  inc=futurew)
predict(reg, newdata=future, level=0.90, interval="confidence")
predict(reg, newdata=future, level=0.90, interval="prediction")
head(prestige)
pres<-prestige$prestige
edu<-prestige$education
inc<-prestige$income
pairs(cbind(pres,edu,inc))
reg <- lm(pres ~ edu + inc)
summary(reg)
confint(reg)
coef=reg$coefficients
res =reg$residuals
fitted=reg$fitted.values
plot(fitted,res,main = "Residuals",xlab = "",ylab = "",
     pch=20)
lines(x=c(0,100),y=c(0,0),col=("red"))
hist(res,main="Histogram of Residuals")
boxplot(res,main="Box Plot of Residuals")
n=length(profs)
ones=rep(1,n)
X=cbind(ones,edu,inc)
betas=solve(t(X)%*%X)%*%t(X)%*%pr??s
ehat=y-X%*%betas
qqnorm(res,main="Q-Q Plot of Residuals")
qqline(ehat)
shapiro.test(res)
simple.edu<-lm(pres~edu)
simple.inc<-lm(pres~inc)
summary(simple.edu)
summary(simple.inc)
summary(reg)
res.edu =simple.edu$residuals
res.inc =simple.inc$residuals
res =reg$residuals
edu.avg<-10 
inc.avg<-4000
newjob<-data.frame(edu=edu.avg, inc= inc.avg)
predict(reg, newdata=newjob, level=0.90, interval="confidence")
predict(reg, newdata=newjob, level=0.90, interval="prediction")
betas<-solve(t(X)%*%X)%*%t(X)%*%y
betas
yhat<-X%*%betas
ehat<-y-X%*%betas
ehat
sigma2.hat<-t(ehat)%*%ehat/(n-2)
sigma.hat<-sqrt(sigma2.hat)
var.betas<-as.vector(sigma2.hat)*solve(t(X)%*%X)
std.betas<-sqrt(diag(var.betas))
var.betas
std.betas
betas
c<-4
t_stat<-(betas[2]-c)/std.betas[2]
t_stat
a<- 0.05
qt_a<-qt(1-a, n-5)
qt_a
install.packages("gplots")
library(gplots)
head(prestige)
women<-prestige$women
women[women<50]<-0
women[women >=50]<-1
y<-prestige$income
n<-length(y)
y
n
type<-prestige$type
women
type
women<-factor(women)
levels(women)<- c("<50",">50")
two.way.aov<-aov(y~ women*type)
summary(two.way.aov)
two.way.aov<-aov(y~ women+type)
summary(two.way.aov)
resid.twoway<-two.way.aov$residuals
qqnorm(resid.twoway)
qqline(resid.twoway)
shapiro.test(resid.twoway)
interaction.plot(type,women,y,type="b",col=c(1:3),leg.bty="n",lwd=2,
                 pch=c(18,24),xlab="type",ylab="women",main="interaction plot")