library(rpart)
library(rpart.plot)

set.seed(0)

# creating a decision tree

dt_model <- rpart(ACTION~., method="class", data = train)
rpart.plot(dt_model)

pred_exp_train <- predict(dt_model, train, type = "class")
mean(pred_exp_train != train$ACTION)

pred_exp_test <- predict(dt_model, test, type = "class")
pred_test <- list(pred_exp_test)
mean(pred_exp_test != test$ACTION)

accuracy <- append(accuracy, sum(pred_exp_test == test$ACTION) / length(pred_exp_test))

plot_confusion_matrix(as_tibble((confusionMatrix(factor(pred_exp_test), test$ACTION)$table)),
                      target_col = "Reference", prediction_col = "Prediction", counts_col = "n")

pred_exp_test_prob <- predict(dt_model, test, type = "prob")
dt_ROCurve <- roc(test$ACTION, as.numeric(pred_exp_test_prob[,1]))
auc <- append(auc, auc(dt_ROCurve))

