library(xgboost)
library(varhandle)
library(cvms)

set.seed(0)

# XGBoost model

X_train <- unfactor(X_train)
X_test <- unfactor(X_test)

xgb_train <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
xgb_test <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
xgb_params <- list(booster = "gbtree",eta = 0.1, max_depth = 4,gamma = 4,subsample=1,
                   colsample_bytree = 0.7,objective = "binary:logistic", eval_metric = "error")

xgbcv <- xgb.cv(params = xgb_params, data = xgb_train, nrounds = 500, nfold = 5, showsd = T, 
                stratified = T, print_every_n = 10, early_stopping_rounds = 20, maximize = F)

xgb1 <- xgb.train(params = xgb_params, data = xgb_train, nrounds = 310, watchlist = list(val=xgb_test,train=xgb_train), 
                  print_every_n = 10, early_stopping_rounds = 10, maximize = F)

xgbpred <- predict(xgb1,as.matrix(X_test))
xgbpred <- ifelse(xgbpred > 0.5,1,0)

confusionMatrix (factor(xgbpred), factor(test$ACTION))

testing_error <- mean(xgbpred != test$ACTION)
testing_error

accuracy <- append(accuracy, sum(xgbpred == test$ACTION) / length(xgbpred))

mat <- xgb.importance(model = xgb1)
xgb.plot.importance(importance_matrix = mat[1:10], col="red", main="Important Variables of XGBoost") 

xgbpred_prob <- predict(xgb1,as.matrix(X_test), type="response")
xgb_ROCurve <- roc(test$ACTION, as.numeric(xgbpred_prob))
auc <- append(auc, auc(xgb_ROCurve))

