#Name of Quantlet: Sberbank Housing DataSet - Var Selection
#Published in:     Statistical Programming Languages
#Description:      'P**
#Keywords:         'variable selection', housing, real-estate
#Author:           Alisa Kolesnikova

library(lubridate)
library(randomForest)
library(glmnet)
#####NEW VARIABLES and ADJUSTMENTS######
#Adding up the macro data
macro <- readRDS("macrotomerge")
rus <- rus2


#Scaling
rus$area_m <- scale(rus$area_m)
rus$raion_popul <- scale(rus$raion_popul)
rus[,c(39:65)] <- scale(rus[,c(39:65)])

#146 levels of sub_area will be replaced by the mean price per meter of the real estate object in that area (calculated form full_sq) - new variable is area_meterprice
rus$meterprice<- rus$price_doc/rus$full_sq
areas <- data.frame(levels(rus$sub_area), tapply(rus$meterprice, rus$sub_area, mean))
colnames(areas) <- c("sub_area","area_meterprice")
rus<- merge(rus, areas, by="sub_area") 
rus$sub_area <- NULL
rm(areas)

#Dealing with station IDs- transforming into dummies
ohe_feats = c('ID_big_road1', 'ID_big_road2', 'ID_railroad_terminal', 'ID_bus_terminal')
for (i in ohe_feats){
  df_all_dummy = acm.disjonctif(rus[i])
  rus[i] = NULL
  rus = cbind(rus, df_all_dummy)
}

#meterprice var awas already created

#Additional vars/ratios
rus$population_density=rus$raion_popul/rus$area_m
rus$healthcare_ratio=rus$healthcare_centers_raion/rus$raion_popul
rus$sport_objects_ratio=rus$sport_objects_raion/rus$raion_popul
rus$shopping_centers_ratio=rus$shopping_centers_raion/rus$raion_popul
rus$office_ratio=rus$office_raion/rus$work_all
rus$floor_inverse <- rus$max_floor - rus$floor

#Additional date variables
rus$day_of_week <-weekdays(rus$timestamp)
rus$day_of_week <- as.factor(rus$day_of_week)
rus$months <- strftime(rus$timestamp,"%m")
rus$months <- as.factor(rus$months)

#Preparing the sets for variable selection
rus1 <- rus[,-288]
rus2 <- rus[,-286]

#Trying out the step function for the AIC based variable selection
n = sample(1:dim(rus1)[1],1000)
s.rus1 = rus1[n,]
s1.lm<- lm(price_doc~., data=s.rus1)
step(s1.lm)
#Starting with AIC=28422.5

#Selecting vars with RandomFrest
f = sample(1:dim(rus1)[1],3000)
f.rus1 = rus1[f,]
rffit <- randomForest(price_doc~., data=f.rus1)
par(mfrow=c(1,1))
VI_F=importance(rffit)
varImpPlot(rffit,type=2)
str(set)
library(caret)
set$children_per_preschool <- NULL
x=preProcess(set, method=c("center","scale"))

set=predict(x,set)
str(set)

#Selecting with LASSO
#cvfit <-cv.glmnet(x, y)
#coef(cvfit, s = "lambda.1se")
x <- f.rus1[,c(3:30)]
y <- f.rus1[,286]
x <- as.matrix(x)
lasso <-glmnet(x = x, y = y, alpha = 1)

glmnet1<-cv.glmnet(x=x, y=y, type.measure='mse', nfolds=5, alpha=.5)
c<-coef(glmnet1,s='lambda.min',exact=TRUE)
inds<-which(c!=0)
variables<-row.names(c)[inds]
variables<-variables[variables %ni% '(Intercept)']
