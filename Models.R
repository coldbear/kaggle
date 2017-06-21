library(randomForest)
library(lmtest)
library(normtest)
library(tseries)
library(ggmap)
library(corrplot)
library(readr)
library(dplyr)
library(caret)
library(xgboost)
library(Metrics)
library(ggplot2)
library(DMwR)
library(klaR)
library(cluster)
library(fpc)
library(doParallel)
options(max.print=1000000)
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)
#Load set
set <- readRDS("workset_2_AK")
sett <- readRDS("workset_2_AK")

#######Transform the sub_area names into mean prices of squaremeter
areas <- data.frame(levels(set$sub_area), tapply(set$meterprice, set$sub_area, mean))
colnames(areas) <- c("sub_area","area_meterprice")
set<- merge(set, areas, by="sub_area") 
set$sub_area <- NULL


#####Two set with different target vars, set1 with price and set2 with price per meter

set1 <- set[,-234]
set2 <- set[,-224]

idx1 = sample(1:nrow(set1), size=0.8*nrow(set1))
idx2 = sample(1:nrow(set2), size=0.8*nrow(set2))

train1 <- set1[idx1,]
test1 <- set1[-idx1,]

train2 <- set2[idx2,]
test2 <- set2[-idx2,]

#sets 3 and 4 have the sub areas as levels
set3 <- sett[,-235]
set4 <- sett[,-225]

idx3 = sample(1:nrow(set3), size=0.8*nrow(set3))
train3 <- set3[idx3,]
test3 <- set3[-idx3,]

idx4 = sample(1:nrow(set4), size=0.8*nrow(set4))
train4 <- set4[idx4,]
test4 <- set4[-idx4,]

########Modelling - XGB

RMSLEsummary <- function (data,
                          lev = NULL,
                          model = NULL) {
  print(head(data))
  data$pred=data$pred
  out=(sum((log(data$pred+1)-log(data$obs+1))^2)/length(data$obs))^0.5
  #print(out)
  names(out) <- "RMSLE"
  out
}


tc = trainControl(
  method = "cv",  
  summaryFunction = RMSLEsummary,
  number = 5,
  returnData = FALSE
)
xgb.gr=expand.grid(nrounds=c(100), max_depth=c(7), 
                   colsample_bytree=1 , eta=0.01,
                   min_child_weight=c(1), gamma=0, subsample=1)

xgbmodel = train(price_doc~., 
                 data = train3,
                 method = "xgbTree",
                 trControl = tc,
                 metric="RMSLE",
                 tuneGrid=xgb.gr,
                 eval_metric="rmse"
)

#result of xgbTree is pretty bad. Linear is much better

xgb2.gr=expand.grid(nrounds=c(800), lambda=1, alpha=0 , eta=0.01)
#Increase nrounds for higher quality (try at least 1000) and lambda if you think there is overfit
xgbmodel1 = train(price_doc~., 
                  data = train1,
                  method = "xgbLinear",
                  trControl = tc,
                  metric="RMSLE",
                  allowParallel = TRUE,
                  tuneGrid=xgb2.gr,
                  eval_metric="rmse"
)
yhat_xgb_1 <- predict(xgbmodel2, test1)
RMSLE1 <- rmsle(yhat_xgb_1, test1$price_doc)
#RMSLE1=0.505

#now trying with train - with areas replaced by mean price per meter
xgbmodel_2 = train(meterprice~., 
                  data = train2,
                  method = "xgbLinear",
                  trControl = tc,
                  metric="RMSLE",
                  allowParallel = TRUE,
                  tuneGrid=xgb2.gr,
                  eval_metric="rmse"
)
yhat_xgb_2 <- predict(xgbmodel_2 , test2)
RMSE2 <- rmse(yhat_xgb_2, test2$meterprice)


#Now trying with log - very good resul so far, need to check what is wrong
train1.1 <- train1
test1.1 <- set1
train1.1$price_doc
train1.1$price_doc <- log(train1.1$price_doc)
test1.1$price_doc <- log(test1.1$price_doc)
xgbmodel5 = train(price_doc~., 
                  data = train1.1,
                  method = "xgbLinear",
                  trControl = tc,
                  metric="RMSLE",
                  tuneGrid=xgb2.gr
)
yhat_xgb_5 <- predict(xgbmodel5, test1.1)
RMSLE5 <- rmsle(exp(yhat_xgb_5), exp(test1.1$price_doc))
#RMSLE5=0.232


#Now to the sub_are predictions. I wonder why linear here is better than tree
xgbmodel3 = train(price_doc~., 
                  data = train3,
                  method = "xgbLinear",
                  trControl = tc,
                  metric="RMSLE",
                  tuneGrid=xgb2.gr
)
yhat_xgb_3 <- predict(xgbmodel3, test3)
RMSLE3 <- rmsle(yhat_xgb_3, test3$meterprice)
RMSLE3

xgbmodel4 = train(price_doc~., 
                  data = new,
                  method = "xgbLinear",
                  trControl = tc,
                  metric="RMSLE",
                  tuneGrid=xgb2.gr
)
yhat_xgb_4 <- predict(xgbmodel4, test4)
RMSLE4 <- rmsle(yhat_xgb_4, test4$meterprice)
RMSLE4
tuneGrid.nnet <- expand.grid(.decay = c(0.5, 0.1), .size = c(6,8,10))
netmodel = train(price_doc~., 
                  data = new,
                  method = "nnet",
                  trControl = tc,
                  metric="RMSLE",
                  tuneGrid=tuneGrid.nnet
)


#Normal xgboost - still in progress
xgb1 <- xgboost(price_doc~., label = 24235,
                data = matrix1,
               max_depth = 2, eta = 1, nthread = 2, nrounds = 400)
matrix1 <- data.matrix(train1)
pred <- predict(bst, agaricus.test$data)

setp <- set
x=preProcess(setp, method=c("center","scale"))
setp=predict(x,setp)
setp1 <- setp[,-234]
setp2 <- setp[,-224]
idxp= sample(1:nrow(setp1), size=0.8*nrow(setp1))
trainp1 <- setp1[idxp,]
testp1 <- setp[-idxp,]
#Lets try log with linear regression
ID_railroad_terminal
lin_sc <- lm(price_doc~full_sq+life_sq+floor+material+num_room+kitch_sq+state+product_type+preschool_quota+school_quota+school_education_centers_top_20_raion+additional_education_raion+additional_education_raion+culture_objects_top_25+thermal_power_plant_raion+incineration_raion+big_market_raion+metro_min_avto+metro_km_avto+green_zone_km+incineration_km+railroad_station_avto_km+railroad_station_avto_min+public_transport_station_km+sadovoe_km+kremlin_km+railroad_km+ID_railroad_terminal+bus_terminal_avto_km+ts_km+market_shop_km+basketball_km+detention_facility_km+workplaces_km+shopping_centers_km+office_km+church_synagogue_km+exhibition_km+exhibition_km+ecology+prom_part_500+church_count_500+prom_part_1000+leisure_count_1000+prom_part_1500+big_church_count_1500+leisure_count_1500+cafe_count_2000_price_2500+cafe_count_2000_price_4000+big_church_count_2000+church_count_2000+leisure_count_2000+office_count_3000+trc_sqm_3000+leisure_count_3000+office_count_5000+cafe_count_5000+cafe_count_5000_na_price+cafe_count_5000_price_500+cafe_count_5000_price_1000+cafe_count_5000_price_1500+cafe_count_5000_price_2500+cafe_count_5000_price_4000+big_church_count_5000+church_count_5000+market_count_5000+buildage+office_ratio+life_sq_ratio+day_of_week+months+gdp_quart+balance_trade+deposits_growth+grp+housing_fund_sqm +electric_stove_share+average_life_exp+hospital_bed_occupancy_per_year+area_meterprice, trainp1)

summary(lin_log2)
y_lin_sc_adj<- predict(lin_sc, testp1)
RMSLEsc <- rmsle(y_lin_sc_adj,testp1$price_doc)
#was Adjusted R-squared:  0.3978, adjusted set Adjusted R-squared:  0.3926 
y_lin_log<- predict(lin_log, test1.1)
RMSLE_linlog <- rmsle(exp(y_lin_log), exp(test1.1$price_doc))
RMSE_linlog <- rmse(exp(y_lin_log), exp(test1.1$price_doc))
#RMSLE_linlog=0.48, RMSE=4218559

