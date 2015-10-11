library(AER)
library(dplyr)
library(KernSmooth)
# Chapter 2
## 1
A=diag(1,10)
A[1,1]<-1
A[10,10]<-1
for (i in 2:9) {
  A[i,i]<-2
}
for (i in 1:9) {
  A[i,i+1]<--1
}
for (i in 2:10) {
  A[i,i-1]<--1
}
A

amatrix<-function(n) {
  A=diag(1,n)
  for (i in 2:(n-1)) {
    A[i,i]<-2
  }
  for (i in 1:(n-1)) {
    A[i,i+1]<--1
  }
  for (i in 2:n) {
    A[i,i-1]<--1
  }
  return(A)
}
all(amatrix(10)==A)
## 2
data("Parade2005")
Parade<-tbl_df(Parade2005)
Parade %>% filter(state=="CA") %>% summarise(CaliMean=mean(earnings))
Parade %>% filter(state=="ID") %>% summarise (IdahoTotal=n())
Parade %>% filter(celebrity=="yes") %>% summarise(CelebMean=mean(earnings),CelebMedian=median(earnings))
plot(log(Parade$earnings)~Parade$celebrity,xlab="Celebrity",ylab="log(Earnings)")

## 3
density(log(Parade$earnings))
density(log(Parade$earnings),bw="SJ")

## 4
# a
data("CPS1988")
plot(log(CPS1988$wage)~CPS1988$experience,xlab="Experience",ylab="log(Wage)")
plot(log(CPS1988$wage)~CPS1988$education,xlab="Education",ylab="log(Wage)")
# b
CPS1988$education<-factor(CPS1988$education,levels = unique(CPS1988$education))
CPS1988$experience<-factor(CPS1988$experience,levels = unique(CPS1988$experience))
plot(log(CPS1988$wage)~CPS1988$experience,xlab="Experience",ylab="log(Wage)")
plot(log(CPS1988$wage)~CPS1988$education,xlab="Education",ylab="log(Wage)")
# c
plot(log(CPS1988$wage)~CPS1988$ethnicity)
plot(log(CPS1988$wage)~CPS1988$smsa)
plot(log(CPS1988$wage)~CPS1988$region)
plot(log(CPS1988$wage)~CPS1988$parttime)

