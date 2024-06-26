
set.seed(0)

# modeling logistic regression

log_model <- glm(pests~., data=train, family="binomial")
summary(log_model)

train_control <- trainControl(method = "cv", number = 10)
# train the model on training set
model <- train(pests ~ .,data = resto,trControl = train_control,
                  method = "glm",family=binomial())
model

pred_train <- predict(log_model, newdata=train, type="response")
train_label <- ifelse(pred_train > 0.5, 1, 0)
tr_error <- mean(train_label!=train$pests)

pred_test <- predict(log_model, newdata=test, type="response")
test_label <- ifelse(pred_test > 0.5, 1, 0)
te_error <- mean(test_label!=test$pests)

tr_error
te_error

accuracy <- c()
accuracy <- append(accuracy, sum(test_label == test$pests) / length(test_label))

plot_confusion_matrix(as_tibble((confusionMatrix(factor(test_label), factor(test$pests))$table)),
                      target_col = "Reference", prediction_col = "Prediction", counts_col = "n")

pred_test <- predict(log_model, newdata=test, type="response")
log_roc <- roc(as.numeric(test$pests),as.numeric(pred_test))
auc <- c()
auc <- append(auc, auc(log_roc))


