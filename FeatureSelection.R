set.seed(0)

head(resto)
colnames(resto)

unique(resto$VIOLATION.DESCRIPTION)

pest_words <- c("rat","mice","mouse","roach","fly","flies","vermin", 
                "pests", "rodents", "insects", "pest", "roaches")
# creating a binary feature for whether or not a violation was due to pests
# also, just noticed we have some empties in description variable. do we want to drop those?
resto$pests <- ifelse(rowSums(sapply(pest_words, grepl, resto$VIOLATION.DESCRIPTION)>=1), 1, 0)

table(resto$pests)
# ok, this is about balanced. I think it's a good outcome variable

# making all variables factors
numeric_cols <- sapply(resto, is.numeric)
resto[numeric_cols] <- lapply(resto[numeric_cols], as.factor)
str(resto)
index <- 1:ncol(resto)
resto[ , index] <- lapply(resto[ , index], as.factor)
str(resto)
summary(resto)

str(resto)

# feature selection steps
n <- nrow(resto)
sample <- sample(n/2, replace=F) 

grep("pests", colnames(resto))

tr_dat <- resto[sample, ]
te_dat <- resto[-sample, ]

lasso_x_tr <- as.matrix(tr_dat[, -32])
lasso_y_tr <- tr_dat[, 32, drop = T]
lasso_x_te <- as.matrix(te_dat[, -32])
lasso_y_te <- te_dat[, 32, drop = T]

cv_fit_lasso <- cv.glmnet(data.matrix(tr_dat[, -32]), factor(lasso_y_tr), family="binomial", alpha=1)
lasso_lambda <- cv_fit_lasso$lambda.min
lasso_coef <- coef(cv_fit_lasso,lasso_lambda)
lasso.model <- glmnet(data.matrix(tr_dat[, -32]), factor(lasso_y_tr),  alpha=1, family="binomial",lambda = cv_fit_lasso$lambda.min)

lasso_tr_pred <- predict(cv_fit_lasso, s=lasso_lambda, newx =data.matrix(tr_dat[, -32]), type="class")
lasso_te_pred <- predict(cv_fit_lasso, s=lasso_lambda, newx =data.matrix(te_dat[, -32]), type="class")

lasso_tr_error <- mean(lasso_tr_pred != lasso_y_tr)
lasso_te_error <- mean(lasso_te_pred != lasso_y_te)
lasso_tr_error
lasso_te_error

cbind(lasso_coef)

colnames(resto)

# feature selection tells us to take out street, record.date, score, and foodpo.perzip.peryear
resto <- resto %>% select(-STREET, -RECORD.DATE, -score, -Foodpo.perZip.perYear)


