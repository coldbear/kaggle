#Experiment with regression
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
test <- multinom(interest_level~bathrooms+bedrooms+price+build_freq+postweek+postday+manager_grade+manager_rank+neighborhood+anger+fear+joy+anticipation+disgust+sadness+surprise+trust+negative+positive, dato1)
#Another package
library(ordinal)
ord.regr <- clm(interest_level~bathrooms+bedrooms+price+build_freq+postweek+postday+manager_grade+manager_rank+neighborhood+anger+fear+joy+anticipation+disgust+sadness+surprise+trust+negative+positive, data=dato1)
summary(ord.regr)

attach(dato1)
table(interest_level)

ord.regr <- clm(interest_level~bathrooms, data=dato1)
summary(ord.regr)

table(dato1$interest_level)
summary(dato1$price)

l = dim(dato1)[1]

dato1$int_lev = 0

for (i in 1:l) {    if(dato1$interest_level[i] == "medium") { dato1$int_lev[i] = 1 }
                    if(dato1$interest_level[i] == "high") { dato1$int_lev[i] = 2 }
               }
dato1$int_lev <- as.factor(dato1$int_lev)
table(dato1$int_lev)

ord.regr <- clm(int_lev~bathrooms+bedrooms+I(price/1000)+postweek+postday+manager_rank+anger+fear+joy+anticipation+disgust+sadness+surprise+trust+negative+positive, data=dato1)
summary(ord.regr)
which.max(price)
dato1 = dato1[-201,]

boxplot(price)

dato1 = dato1[ato1$build_freq!=8286]

detach(dato1)

ord.regr$Hessian

m = subset(dato1, neighborhood)

table(neighborhood=="Astoria",int_lev)

ord.regr <- clm(int_lev~bedrooms+I(price/1000)+postday+joy, data=dato1)
summary(ord.regr)

s = coef(ord.regr)[1:2]
beta = coef(ord.regr)[3:11]
x.pred = c(2,0.5,0,0,0,0,1,0,2)
mu = x.pred%*%beta
mu

plogis(s[1],location=mu)
plogis(s[2],location=mu)-plogis(s[1],location=mu)
1-plogis(s[2],location=mu)
