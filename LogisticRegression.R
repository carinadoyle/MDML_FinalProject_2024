
set.seed(0)

# modeling logistic regression

log_model <- glm(ACTION~., data=train, family="binomial")
summary(log_model)

train_control <- trainControl(method = "cv", number = 10)
# train the model on training set
model <- train(ACTION ~ .,data = resto,trControl = train_control,
                  method = "glm",family=binomial())
model

pred_train <- predict(log_model, newdata=train, type="response")
train_label <- ifelse(pred_train > 0.5, 1, 0)
tr_error <- mean(train_label!=train$ACTION)

pred_test <- predict(log_model, newdata=test, type="response")
test_label <- ifelse(pred_test > 0.5, 1, 0)
te_error <- mean(test_label!=test$ACTION)

tr_error
te_error

accuracy <- c()
accuracy <- append(accuracy, sum(test_label == test$ACTION) / length(test_label))

plot_confusion_matrix(as_tibble((confusionMatrix(factor(test_label), factor(test$ACTION))$table)),
                      target_col = "Reference", prediction_col = "Prediction", counts_col = "n")

pred_test <- predict(log_model, newdata=test, type="response")
log_roc <- roc(as.numeric(test$ACTION),as.numeric(pred_test))
auc <- c()
auc <- append(auc, auc(log_roc))

imp_df <- data.frame(importance(model_18)$data)
sliced <- imp_df[1:10,]
ggplot(data=sliced) + geom_col(mapping=aes(x=Overall, y=names), fill="red") + 
  xlab("Importance Score") + ylab("Variable") + ggtitle("Important Variables For Logisitic Regression in 2018")

