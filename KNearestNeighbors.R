set.seed(0)

k_seq <- c(5, seq(1,40, by=1))

te_error<- c()

for(i in seq_along(k_seq)){
  knn_fit <-caret::knn3(ACTION~., train, k=k_seq[i])
  knn_pred <- predict(knn_fit, test, type="class")
  te_error[i] <- mean(knn_pred != test$ACTION)
}

df <- data.frame(k_seq, te_error)

ggplot(data = df, mapping = aes(x=k_seq, y=te_error))+geom_line(col="red") +
  geom_line()+xlab("k value")+ylab("Testing error")

trControl <- trainControl(method  = "cv", number  = 10)
fit <- train(ACTION ~ ., method = "knn", tuneGrid= expand.grid(k = 1:20),
             trControl= trControl, metric = "Accuracy", data=train_18)
fit

# best K - 

knn_best <- knn3(ACTION~.,train, k=15)
best_pred <- predict(knn_best, test, type="class")
best_te_error <- mean(best_pred != test$ACTION)

accuracy <- append(accuracy, sum(best_pred == test$ACTION) / length(best_pred))

plot_confusion_matrix(as_tibble((confusionMatrix(factor(best_pred), test$ACTION)$table)),
                      target_col = "Reference", prediction_col = "Prediction", counts_col = "n")

pred_prob_test <- predict(knn_best, test, type="prob")
knn_ROCurve <- roc(test$ACTION, as.numeric(pred_prob_test[,1]))
auc <- append(auc, auc(knn_ROCurve))


