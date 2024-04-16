
library(ROSE)

set.seed(0)

# checking dataset balance
barplot(prop.table(table(resto$ACTION)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Class Distribution")

n <- nrow(resto)
sample <- sample(n/1.20, replace=F) 

train <- resto[sample, ]
test <- resto[-sample, ]

train <- ovun.sample(ACTION~., data=resto, method = "both",
                        seed = 0, N = nrow(resto))$data
table(train$ACTION)

y_train <- as.integer(resto$ACTION) -1
y_test <- as.integer(resto$ACTION) - 1
X_train <- resto %>% select(-ACTION)
X_test <- resto %>% select(-ACTION)

barplot(prop.table(table(train$ACTION)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Class Distribution")


