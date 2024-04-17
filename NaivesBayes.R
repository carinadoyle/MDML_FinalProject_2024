library(naivebayes)

set.seed(0)

# naive bayes classifier

nb_model <- naive_bayes(ACTION ~ ., train, usekernel = T, laplace=1) 
summary(nb_model)

train_control <- trainControl(method = "cv", number = 10)
# train the model on training set
model <- train(ACTION ~ .,data = train,trControl = train_control,
               method = "naive_bayes",family=binomial())
model

pred_train <- predict(nb_model, newdata=train, type="class")
tr_error <- mean(pred_train!=train$ACTION)

pred_test <- predict(nb_model, newdata=test, type="class")
te_error <- mean(pred_test!=test$ACTION)

tr_error
te_error

accuracy <- append(accuracy, sum(pred_test == test$ACTION) / length(pred_test))

plot_confusion_matrix(as_tibble((confusionMatrix(factor(pred_test), test$ACTION)$table)),
                      target_col = "Reference", prediction_col = "Prediction", counts_col = "n")

pred_prob_test <- predict(nb_model, newdata=test, type="prob")
nb_ROCurve <-roc(test$ACTION,as.numeric(pred_prob_test[,1]))
auc <- append(auc, auc(nb_ROCurve))
