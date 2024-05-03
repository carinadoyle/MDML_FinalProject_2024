library(randomForest)

set.seed(0)

# random forest model

rf_model <- randomForest(pests~., train, importance=TRUE, 
                            ntree=1000, proximity=TRUE, nodesize=10, mtry=10)
rf_model

rf_pred <- predict(rf_model, test, type="class")
confusionMatrix(rf_pred, test$ACTION)
te_error <- mean(rf_pred!=test$ACTION)

accuracy <- append(accuracy, sum(rf_pred == test$ACTION) / length(rf_pred))

rf_pred_prob <- predict(rf_model, test, type="prob")
rf_ROCurve <- roc(test$ACTION, as.numeric(rf_pred_prob[,1]))
auc <- append(auc, auc(rf_ROCurve))