
data("Journals")
lm1<-lm(Journals$price~Journals$citations)
plot(lm1)
logLik(lm1)
AIC(lm1)
deviance(lm1)
vcov(lm1)