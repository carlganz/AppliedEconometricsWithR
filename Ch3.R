library(AER)
library(dplyr)
library(KernSmooth)
set.seed(30)
data("Journals")
lm1<-lm(Journals$price~Journals$citations)
plot(lm1)
logLik(lm1)
AIC(lm1)
deviance(lm1)
vcov(lm1)
library(splines)
data("CPS1988")
cps<-tbl_df(CPS1988)
lm2<-lm(log(wage)~bs(experience,df=5)+education+ethnicity,cps)
lm2


# 1

x<-1:20
y<-x+rnorm(20)

lm3<-lm(y~x)
summary(lm3)

xmat<-cbind(1,x)

beta<-as.vector(t(solve(t(xmat)%*%xmat)%*%t(xmat)%*%y))
names(beta)<-c("beta0","beta1")
beta

lm4<-lm(y~I(x^2))
summary(lm4)

x2mat<-cbind(1,x^2)
beta2<-as.vector(t(solve(t(x2mat)%*%x2mat)%*%t(x2mat)%*%y))
beta2

lm5<-lm(y~I(x^3))
summary(lm5)

x3mat<-cbind(1,x^3)
beta3<-as.vector(t(solve(t(x3mat)%*%x3mat)%*%t(x3mat)%*%y))
beta3

lm6<-lm(y~I(x^6))
summary(lm6)

x4mat<-cbind(1,x^6)
beta4<-as.vector(t(solve(t(x4mat)%*%x4mat)%*%t(x4mat)%*%y))
beta4

lm7<-lm(y~I(x^7))
summary(lm7)

x5mat<-cbind(1,x^7)
# Error generated
beta5<-as.vector(t(solve(t(x5mat)%*%x5mat)%*%t(x5mat)%*%y))
beta5

# 2
## a

data("HousePrices")
hp<-tbl_df(HousePrices)
head(hp)
lm8<-lm(log(price)~.,hp)
summary(lm8)
hp$recreation<-relevel(hp$recreation,ref="yes")
lm9<-lm(log(price)~log(lotsize)+log(bedrooms)+log(bathrooms)+log(stories)+driveway+recreation+fullbase+gasheat+aircon+garage+prefer,hp)
summary(lm9)
#second model gives a slightly higer R^2

## b

newhouse<-predict(lm9,data.frame(lotsize=4700,bedrooms=3,bathrooms=2,stories=2,driveway="yes",recreation="no",fullbase="yes",gasheat="no",aircon="no",garage=2,prefer="no"),interval="prediction")
newhouse

## c

library(MASS)
boxcox(lm9)

# 3
## a

data("PSID1982")
psid<-tbl_df(PSID1982)
lm10<-lm(log(wage)~I(experience^2)+.,psid)
summary(lm10)

## b

lm11<-lm(log(wage)~I(experience^2)+experience:gender+.,psid)
summary(lm11)

lm12<-lm(log(wage)~I(experience^2)+education:gender+.,psid)
summary(lm12)

# No significiant interaction

# 4
data("USMacroG")
library(dynlm)
dynlm1<-dynlm(consumption~dpi+L(dpi),data=USMacroG)
summary(dynlm1)
dynlm2<-dynlm(consumption~dpi+L(consumption),data=USMacroG)
summary(dynlm2)
## a
jtest(dynlm1,dynlm2)
## b
coxtest(dynlm1,dynlm2)

# 5

data("PSID1982")
psid<-tbl_df(PSID1982)
M1<-lm(log(wage)~education+experience+I(experience^2)+weeks+married+gender+ethnicity+union,psid)
M2<-lm(log(wage)~education+experience+I(experience^2)+weeks+occupation+south+smsa+industry,psid)
M1
M2

## a

jtest(M1,M2)