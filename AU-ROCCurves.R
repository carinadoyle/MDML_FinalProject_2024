
# listing algorithms used
models <- c("Logistic Regression", "Naive Bayes", "K-Nearest Neighbors", 
            "Decision Trees", "Random Forest")

# table of accuracy & AUC values
cbind("Model"=models, "Accuracy"=round(accuracy,3), "AUC"=round(auc,3))

# plotting auc curves for each model
ggroc(list("Logistic Regression"=log_roc,"Naive Bayes"=nb_ROCurve, 
           "K-Nearest Neighbors"=knn_ROCurve, "Random Forest"=rf_ROCurve,
           "Decision Tree"=dt_ROCurve), legacy.axes = TRUE) + 
  labs(color="Algorithm") + ggtitle("ROC Curves By Model")
