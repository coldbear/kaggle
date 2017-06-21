#Name of Quantlet: Sberbank Housing DataSet - Statistical Analysis
#Published in:     Statistical Programming Languages
#Description:      'P**
#Keywords:         statistical analysis, housing, real-estate
#Author:           Alisa Kolesnikova


library(tseries)
library(lmtest)

#The preset target variable is price_doc. Let's examine it
plot(rus$price_doc)
#Will delete couple of outliers
which.max(rus$price_doc)
rus <- rus[rus$price_doc<80000000,]

#examining normality

hist(rus$price_doc)#The distribution seems looking like the log distribution
hist(log(rus$price_doc))#This looks much better, might make sence to predict over the log of price_doc

jarque.bera.test(rus$price_doc)
#Jarque Bera Test
#data:  rus$price_doc
#X-squared = 1292100, df = 2, p-value < 2.2e-16
#the values is obviously not normally distributed

jarque.bera.test(log(rus$price_doc))
#Jarque Bera Test

#data:  log(rus$price_doc)
#X-squared = 8762.9, df = 2, p-value < 2.2e-16
#better but still not normally distributed

#Let's look at the meterprice
hist(rus$meterprice)
jarque.bera.test(rus$meterprice)
#Jarque Bera Test

#data:  rus$meterprice
#X-squared = 4087.3, df = 2, p-value < 2.2e-16
#also doesn't look too good but worths further consideration


#Examinig the set in regression, without meterprice(289) or without price_doc (286)
rus1 <- rus[,-288]
rus2 <- rus[,-286]
lm1<- lm(price_doc~., data=rus1)
summary(lm1)

#Adjusted R-squared:  0.623, a lot of important variables. However 15 coef not defined due to several singularities.

u.hat1 = resid(lm1)
jarque.bera.test(u.hat1)#residuals are not evenly distributed

hist(u.hat1, freq=F)
min=min(u.hat1) 
max=max(u.hat1)
mu = mean(u.hat1)
sigma = sd(u.hat1)
curve( dnorm(x,mu,sigma), min, max, add=T)

#Checking for heteroschedasticity

#Graphical examination
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(lm1)
#Heteroschedasticity is obviously present as on the graph with Resid vs Fitted there is a triangle-like pattern with a big curve, also, on the Scale-Location Graph one might see a very uneven distribution along the red line.

#Statistical test - we will use Breush Pagan Test
bptest(lm1)
#studentized Breusch-Pagan test

#data:  lm1
#BP = 5102.6, df = 309, p-value < 2.2e-16
#variance of the residuals is obviously not constant, so the graphical guess was right - we will have to account for heteroschedasticity

#We might try the Box-Cox  transformation or use the machines learning methods alternative to regressions in order to rectify the heteroschedasticity.
