
library(ROSE)

set.seed(0)

# checking dataset balance
barplot(prop.table(table(resto$pests)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Class Distribution")

n <- nrow(resto)
sample <- sample(n/1.20, replace=F) 

train <- resto[sample, ]
test <- resto[-sample, ]

y_train <- as.integer(resto$pests) -1
y_test <- as.integer(resto$pests) - 1
X_train <- resto %>% select(-pests)
X_test <- resto %>% select(-pests)

nrow(train)
nrow(test)

str(resto)

