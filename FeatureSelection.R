set.seed(0)

# feature selection steps for 2018
n <- nrow(resto)
sample <- sample(n/2, replace=F) 

grep("ACTION", colnames(resto))

tr_dat <- resto[sample, ]
te_dat <- resto[-sample, ]

lasso_x_tr <- as.matrix(tr_dat[, -4])
lasso_x_tr
lasso_y_tr <- tr_dat[, 4, drop = T]
lasso_x_te <- as.matrix(te_dat[, -4])
lasso_y_te <- te_dat[, 4, drop = T]

cv_fit_lasso <- cv.glmnet(data.matrix(tr_dat[, -4]), factor(lasso_y_tr), family="binomial", alpha=1)
lasso_lambda <- cv_fit_lasso$lambda.min
lasso_coef <- coef(cv_fit_lasso,lasso_lambda)
lasso.model <- glmnet(data.matrix(tr_dat[, -4]), factor(lasso_y_tr),  alpha=1, family="binomial",lambda = cv_fit_lasso$lambda.min)

lasso_tr_pred <- predict(cv_fit_lasso, s=lasso_lambda, newx =data.matrix(tr_dat[, -4]), type="class")
lasso_te_pred <- predict(cv_fit_lasso, s=lasso_lambda, newx =data.matrix(te_dat[, -4]), type="class")

lasso_tr_error <- mean(lasso_tr_pred != lasso_y_tr)
lasso_te_error <- mean(lasso_te_pred != lasso_y_te)
lasso_tr_error
lasso_te_error

cbind(lasso_coef)

resto <- resto %>% select(-Community.Board)
