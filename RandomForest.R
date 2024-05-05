library(ranger)

set.seed(0)

# random forest model

rf_model <- ranger(formul=pests~., data=train, 
                            num.trees=1000, respect.unordered.factors=T, probability=T)
rf_model

rf_pred <- predict(rf_model, test, type="response")
rf_label <- ifelse(rf_pred$predictions[,2] > 0.5, 1, 0)
te_error <- mean(rf_label!=test$pests)

accuracy <- append(accuracy, sum(rf_label == test$pests) / length(rf_label))

rf_pred_prob <- rf_pred$predictions[,2]
rf_ROCurve <- roc(test$pests, as.numeric(rf_pred_prob))
auc <- append(auc, auc(rf_ROCurve))
