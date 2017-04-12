dat <- read.csv("C:\\Users\\ilias\\Desktop\\kaggle\\forwork.csv")
dat$X <- NULL
                 dat[,c("display_address", "street_address", "route", "station", "street_number")] <- lapply(dat[,c("display_address", "street_address", "route", "station", "street_number")], as.character)
                 dat[,c(9,16:52, 61:62)] <- lapply(dat[,c(9,16:52, 61:62)], as.factor) 

                 
#XGBOOST (Extreme Gradient Boosting)
tc = trainControl(method = "cv", number=10, classProbs = TRUE,
                  allowParallel = TRUE,
                  summaryFunction = mnLogLoss,
                  returnData = FALSE)
xgb.gr=expand.grid(nrounds=500, max_depth=5, 
                   colsample_bytree=0.7, eta=0.01,
                   min_child_weight=1, gamma=0, subsample=1)

xgbmodel = train(int_lev~., data = dat, method = "xgbTree",
                 tuneGrid=xgb.gr, trControl = tc, metric="logLoss", eval_metric="mlogloss")

#NEURAL NETWORKS
nnet.tg <- expand.grid(decay = 1, size = 6)

nn = train(int_lev~bathrooms+bedrooms+price+elevator, data = dat,  
           method = "nnet", maxit = 300, trace = TRUE, 
           tuneGrid = nnet.tg,
           metric = "logLoss",
           eval_metric="mlogloss",
           trControl = tc)

#RANDOM FOREST
rf.tg=expand.grid(mtry=2)
rf=train(int_lev~bathrooms+bedrooms+price+elevator, data=dat, method="rf", ntree=500, 
         trControl = tc, metric = "logLoss",
         eval_metric="mlogloss", tuneGrid=rf.tg)