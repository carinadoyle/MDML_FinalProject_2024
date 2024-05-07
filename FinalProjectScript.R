#################################
#                               #
#       Library Import          #
#                               #
#################################

library(haven) #import data
library(tidyverse)
library(dplyr) #data mining/ data cleaning
library(naniar) #replace with NA
library(leaps) #best subset
library(glmnet) # lasso
library(caret) # KNN 
library(ggplot2) # plotting results
library(pROC) # AU-ROC curves
library(ROCR) # AU-ROC curves
library(cvms) # plot confusion matrices
library(naivebayes) # naive bayes model
library(ranger) # random forest model
library(readxl) # read excel file
library(rpart) # decision tree model
library(rpart.plot) # plotting decision tree model

#################################
#                               #
#         Data Import           #
#                               #
#################################

data <- read.csv("DOHMH_New_York_City_Restaurant_Inspection_Results_20240416.csv")
resto <- data
head(resto)
nrow(resto)

data2 <- read.csv("food_poisoning_dataset.csv")
foodpo <- data2
head(foodpo)
nrow(foodpo)

data3 <- read.csv("Rodent_Inspection.csv")
rodent <- data3
head(rodent)
nrow(rodent)

data4 <- readxl::read_excel("nyc2010census_tabulation_equiv.xlsx", 3)
neibrhd <- data4
head(neibrhd)
nrow(neibrhd)

#################################
#                               #
#   Data Cleaning & Recoding    #
#                               #
#################################

# removing cuisines that are presents only after 2022
resto <- resto %>% filter(CUISINE.DESCRIPTION!="Armenian",
                          CUISINE.DESCRIPTION!="Brazilian",
                          CUISINE.DESCRIPTION!="Indonesian",
                          CUISINE.DESCRIPTION!="Pancakes/Waffles",
                          CUISINE.DESCRIPTION!="Afghan",
                          CUISINE.DESCRIPTION!="German")

#converts the 3 date columns to date format and extracts inspection year 
resto <- resto %>% mutate(INSPECTION.DATE = as.Date(INSPECTION.DATE, format = "%m/%d/%Y")) %>%
  mutate(GRADE.DATE = as.Date(GRADE.DATE, format = "%m/%d/%Y")) %>%
  mutate(RECORD.DATE = as.Date(RECORD.DATE, format = "%m/%d/%Y")) %>%
  mutate(INSPECTION.YEAR = lubridate::year(INSPECTION.DATE)) %>%
  filter(nchar(ZIPCODE) == 5) %>% #remove rows with missing zipcode info
  mutate(ZIPCODE = as.factor(ZIPCODE))  #make zip code a factor

#Remove Restaurants that have not been inspected and other rows with missing data:
resto <- subset(resto, BORO!="0") #Removes rows with no boro information
resto <- subset(resto, INSPECTION.YEAR!="1900") #removes all rows that did not have an inspection
resto <- resto %>% drop_na(SCORE) #removes missing score rows
resto <- subset(resto, SCORE >= 0) #removes rows with negative scores
resto <- resto %>% drop_na(INSPECTION.TYPE) #remove rows with no inspection type
resto <- resto %>% group_by(CAMIS, INSPECTION.DATE) %>% 
  mutate(score = max(SCORE)) %>% ungroup()  #choose the highest score if more than 1 score listed for the same inspection

foodpo <- foodpo %>% filter(Location.Type == "Restaurant" | Location.Type =="Restaurant/Bar/Deli/Bakery") %>%
  filter(nchar(Incident.Zip) == 5) %>% mutate(Incident.Zip = as.factor(Incident.Zip)) %>%
  mutate(Created.Date = as.Date(Created.Date, format = "%m/%d/%Y")) %>%  #Convert Date and year info
  mutate(Created.Year = lubridate::year(Created.Date)) %>% filter(Created.Year >= 2015 & Created.Year <= 2022) #limit to 2015 to 2022

#Add a feature weighting the row by "Descriptor" field:  "1 or 2" cases = 1 and "3 or more" cases = 2
foodpo <- foodpo %>% mutate(Descriptor = as.factor(Descriptor)) %>% 
  mutate(weight = ifelse(Descriptor == "1 or 2", 1, 2)) %>%
  rename(ZIPCODE = Incident.Zip) %>%  #renamed for easier merging later
  group_by(ZIPCODE, Created.Year)%>%
  summarize(totalw = sum(weight)) %>%
  mutate(Percentage = (totalw / sum(totalw)) * 100) %>%
  complete(Created.Year = full_seq(2015:2022, 1)) %>% #fill in the missing years for each zipcode
  ungroup()  
foodpo <- replace(foodpo, is.na(foodpo), 0) #indicate a zero percentage and weight for years a zip was not reported

#merge into resto main data:
foodpo <- foodpo %>% rename(INSPECTION.YEAR = Created.Year) %>% #change year column name and remove totalw to match in merge
  select(-totalw)
#Join with restaurant data set by zip code and year:
resto <- left_join(resto, foodpo, by = c('ZIPCODE' = 'ZIPCODE', 'INSPECTION.YEAR' = 'INSPECTION.YEAR'))
resto <- resto %>%  rename(Foodpo.perZip.perYear = "Percentage")

#filter to observed cases of "Rat activity" only in RESULT column and clean up Zip code and date info
rodent <- rodent %>% filter(RESULT == "Rat Activity") %>%
  mutate(INSPECTION_DATE = as.Date(INSPECTION_DATE, format = "%m/%d/%Y")) %>%  #Convert Date and year info
  mutate(INSPECTION_YEAR = lubridate::year(INSPECTION_DATE)) %>% 
  filter(INSPECTION_YEAR >= 2015) %>% #Limit to the other data date range (2015-2024) 
  filter(INSPECTION_YEAR >= 2015 & INSPECTION_YEAR <= 2022) %>% #Limit to the other data date range (2015-2022) 
  filter(nchar(ZIP_CODE) == 5) %>% mutate(ZIP_CODE = as.factor(ZIP_CODE)) %>%
  rename(ZIPCODE = ZIP_CODE)   #renamed for easier merging later

#Add features to measure by zip code and year 
rodent <- rodent %>% group_by(INSPECTION_YEAR) %>%
  mutate(all_observations_byYear = n()) %>% ungroup() #find count of observations per year

rodent <- rodent %>% group_by(ZIPCODE, INSPECTION_YEAR) %>% #create a "Percentage" metric for % of total rat activity reports made each year from each zip code
  reframe(n_group = n(), Percentage = n_group/all_observations_byYear * 100) %>% ungroup() 

rodent <- rodent %>% distinct() %>% #keep all unique rows (year/zipcode combos)
  group_by(ZIPCODE) %>% complete(INSPECTION_YEAR = full_seq(2015:2022, 1)) %>% #fill in all years for each zip that was not reported
  ungroup() 

rodent <- replace(rodent, is.na(rodent), 0) #fill in zero percentage for years/zips that did not report any rat activity

#adjust column names and remove n_roups to prep for merge with main resto data
rodent <- rodent %>% rename(INSPECTION.YEAR = INSPECTION_YEAR) %>%
  select(-n_group)
#Join with restaurant data set by zip code and year:
resto <- left_join(resto, rodent, by = c('ZIPCODE' = 'ZIPCODE', 'INSPECTION.YEAR' = 'INSPECTION.YEAR'))
resto <- resto %>% rename(RatActivity.perZip.perYear = "Percentage")

resto$Foodpo.perZip.perYear[is.na(resto$Foodpo.perZip.perYear)] <- 0
resto$RatActivity.perZip.perYear[is.na(resto$RatActivity.perZip.perYear)] <- 0

#rename some columns and limit to those that can pinpoint neighborhood
neibrhd <- neibrhd %>% mutate(NTA = as.factor(NTA.Code)) %>%
  select(NTA, Neighborhood.Name)
neibrhd <- neibrhd %>% distinct()

#Prep and merge with resto data
resto <- resto %>% mutate(NTA = as.factor(NTA)) %>% mutate(BORO = as.factor(BORO))
resto <- left_join(resto, neibrhd, by = c('NTA' = 'NTA'))

resto <- resto %>% drop_na(Neighborhood.Name)  #drop any rows that are missing Neighborhood data

#If we want to filter to complete cases only (code below), we will still have almost 100K observations (99747):
# resto <- resto %>% filter(complete.cases(.))

# it looks like we only have NAs in grade date, council distrinct, census tract, bin, and location.point1
colnames(resto)[colSums(is.na(resto)) > 0]

unique(resto$GRADE.DATE)
unique(resto$Council.District)
unique(resto$Census.Tract)
unique(resto$BIN)
unique(resto$Location.Point1)

# this analysis shows that location point 1 is all NAs--I'm dropping this variable
resto <- resto %>% select(-Location.Point1)

# inspection_month
resto$INSPECTION.MONTH <- month(resto$INSPECTION.DATE)
# building odd or even
resto$BUILDING <- ifelse(as.numeric(resto$BUILDING)%%2==1, 1, 0)
# inspection day of the week
resto$INSPECTION.DAY <- weekdays(resto$INSPECTION.DATE)

pest_words <- c("rat","mice","mouse","roach","fly","flies","vermin", 
                "pests", "rodents", "insects", "pest", "roaches")
# creating a binary feature for whether or not a violation was due to pests
resto$pests <- ifelse(rowSums(sapply(pest_words, grepl, resto$VIOLATION.DESCRIPTION)>=1), 1, 0)

table(resto$pests)
# ok, this is about balanced. I think it's a good outcome variable

# dropping all variables that will not be included in final model
resto <- resto %>% select(-CAMIS, -DBA, -STREET, -PHONE, -INSPECTION.DATE, -VIOLATION.CODE, -VIOLATION.DESCRIPTION, 
                          -SCORE, -GRADE.DATE, -RECORD.DATE, -Latitude, -Longitude, -Census.Tract, -BIN, -BBL, 
                          -NTA, -ACTION, -BORO)

# stratifying data to only be lower manhattan (34th st and below) zip codes
zip_list <- c(10001, 10010, 10011, 10003, 10009, 10014, 10012, 10002, 
              10013, 10007, 10048, 10038, 10006, 10005, 10004)

resto <- resto %>% filter(ZIPCODE%in%zip_list)
str(resto)

# making all variables factors
numeric_cols <- sapply(resto, is.numeric)
resto[numeric_cols] <- lapply(resto[numeric_cols], as.factor)
str(resto)
index <- 1:ncol(resto)
resto[ , index] <- lapply(resto[ , index], as.factor)
str(resto)
summary(resto)

# score is numeric, so we'll replace that one
resto$score <- as.numeric(resto$score)
resto$Foodpo.perZip.perYear <- as.numeric(resto$Foodpo.perZip.perYear)
resto$RatActivity.perZip.perYear <- as.numeric(resto$RatActivity.perZip.perYear)
resto$ZIPCODE <- factor(resto$ZIPCODE, levels=zip_list)

# I think we should pull out 2023 and 2024 as our years to predict for our model
resto_assess <- resto %>% filter(INSPECTION.YEAR==2023|INSPECTION.YEAR==2024) %>% select(-INSPECTION.YEAR)
resto <- resto %>% filter(INSPECTION.YEAR!=2023 & INSPECTION.YEAR!=2024) %>% select(-INSPECTION.YEAR)

resto <- resto %>% filter(complete.cases(.)) %>% filter(Neighborhood.Name!="park-cemetery-etc-Manhattan")

#################################
#                               #
#   Training & Testing Sets     #
#                               #
#################################

# checking dataset balance
barplot(prop.table(table(resto$pests)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Class Distribution")

n <- nrow(resto)
sample <- sample(n/1.5, replace=F) 

train <- resto[sample, ]
test <- resto[-sample, ]

y_train <- as.integer(resto$pests) -1
y_test <- as.integer(resto$pests) - 1
X_train <- resto %>% select(-pests)
X_test <- resto %>% select(-pests)

#################################
#                               #
#   Logistic Regression Model   #
#                               #
#################################

log_model <- glm(pests~., data=train, family="binomial")
summary(log_model)

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

#################################
#                               #
#      Naive Bayes Model        #
#                               #
#################################

nb_model <- naive_bayes(pests ~ ., train, usekernel = T, laplace=1) 
summary(nb_model)

pred_train <- predict(nb_model, newdata=train, type="class")
tr_error <- mean(pred_train!=train$pests)

pred_test <- predict(nb_model, newdata=test, type="class")
te_error <- mean(pred_test!=test$pests)

tr_error
te_error

accuracy <- append(accuracy, sum(pred_test == test$pests) / length(pred_test))

plot_confusion_matrix(as_tibble((confusionMatrix(factor(pred_test), test$pests)$table)),
                      target_col = "Reference", prediction_col = "Prediction", counts_col = "n")

pred_prob_test <- predict(nb_model, newdata=test, type="prob")
nb_ROCurve <-roc(test$pests,as.numeric(pred_prob_test[,1]))
auc <- append(auc, auc(nb_ROCurve))

#################################
#                               #
#          KNN Model            #
#                               #
#################################

k_seq <- seq(1,25, by=1)

te_error<- c()

for(i in seq_along(k_seq)){
  knn_fit <-caret::knn3(pests~., train, k=k_seq[i])
  knn_pred <- predict(knn_fit, test, type="class")
  te_error[i] <- mean(knn_pred != test$pests)
}

df <- data.frame(k_seq, te_error)
ggplot(data = df, mapping = aes(x=k_seq, y=te_error))+geom_line(col="red") +
  geom_line()+xlab("k value")+ylab("Testing error")

knn_best <- knn3(pests~.,train, k=23)
best_pred <- predict(knn_best, test, type="class")
best_te_error <- mean(best_pred != test$pests)

accuracy <- append(accuracy, sum(best_pred == test$pests) / length(best_pred))

plot_confusion_matrix(as_tibble((confusionMatrix(factor(best_pred), test$pests)$table)),
                      target_col = "Reference", prediction_col = "Prediction", counts_col = "n")

pred_prob_test <- predict(knn_best, test, type="prob")
knn_ROCurve <- roc(test$pests, as.numeric(pred_prob_test[,1]))
auc <- append(auc, auc(knn_ROCurve))

#################################
#                               #
#      Decision Tree Model      #
#                               #
#################################

dt_model <- rpart(pests~., method="class", data = train)
rpart.plot(dt_model)

pred_exp_train <- predict(dt_model, train, type = "class")
mean(pred_exp_train != train$pests)

pred_exp_test <- predict(dt_model, test, type = "class")
pred_test <- list(pred_exp_test)
mean(pred_exp_test != test$pests)

accuracy <- append(accuracy, sum(pred_exp_test == test$pests) / length(pred_exp_test))

plot_confusion_matrix(as_tibble((confusionMatrix(factor(pred_exp_test), test$pests)$table)),
                      target_col = "Reference", prediction_col = "Prediction", counts_col = "n")

pred_exp_test_prob <- predict(dt_model, test, type = "prob")
dt_ROCurve <- roc(test$pests, as.numeric(pred_exp_test_prob[,1]))
auc <- append(auc, auc(dt_ROCurve))


#################################
#                               #
#      Random Forest Model      #
#                               #
#################################

rf_model <- ranger(formul=pests~., data=train, 
                   num.trees=1000, respect.unordered.factors=T, probability=T)
rf_model

rf_pred <- predict(rf_model, test, type="response")
rf_label <- ifelse(rf_pred$predictions[,2] > 0.5, 1, 0)
te_error <- mean(rf_label!=test$pests)

accuracy <- append(accuracy, sum(rf_label == test$pests) / length(rf_label))

rf_pred_prob <- rf_pred$predictions[,2]
rf_ROCurve <- roc(test$pests, as.numeric(rf_pred_prob))
auc <- append(auc, auc(rf_ROCurve))

#################################
#                               #
#   AU-ROC Curves & Accuracy    #
#                               #
#################################

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

#################################
#                               #
# 2023-24 Model Implementation  #
#                               #
#################################

unique(resto$INSPECTION.TYPE)
unique(resto_assess$INSPECTION.TYPE)

resto_assess <- resto_assess %>% filter(GRADE!="N")
resto_assess <- resto_assess %>% filter(INSPECTION.TYPE!="Cycle Inspection / Second Compliance Inspection",
                                        INSPECTION.TYPE!="Pre-permit (Non-operational) / Compliance Inspection")
resto_assess <- resto_assess %>% filter(Neighborhood.Name!="park-cemetery-etc-Manhattan")
resto_assess <- resto_assess %>% select(-pests)

resto_pred <- predict(log_model, newdata=resto_assess, type="response")

resto_results <- cbind(resto_assess, resto_pred)

# predictions for the neighborhoods with highest average probability of having pest violations

resto_predictions <- resto_results %>% group_by(Neighborhood.Name) %>% 
  summarise(average.probability=mean(resto_pred, na.rm=T)) %>% arrange(desc(average.probability))
resto_predictions

# average scores for each neighborhood

resto_scores <- resto_results %>% group_by(Neighborhood.Name) %>% 
  summarise(average.score=mean(score, na.rm=T)) %>% arrange(desc(average.score))
resto_scores

# predictions for the days of the week highest average probability of having pest violations

resto_predictions <- resto_results %>% group_by(INSPECTION.DAY) %>% 
  summarise(average.probability=mean(resto_pred, na.rm=T)) %>% arrange(desc(average.probability))
resto_predictions

# average scores for each day of the week

resto_scores <- resto_results %>% group_by(INSPECTION.DAY) %>% 
  summarise(average.score=mean(score, na.rm=T)) %>% arrange(desc(average.score))
resto_scores

# predictions for the month highest average probability of having pest violations

resto_predictions <- resto_results %>% group_by(INSPECTION.MONTH) %>% 
  summarise(average.probability=mean(resto_pred, na.rm=T)) %>% arrange(desc(average.probability))
resto_predictions

# average scores for each month

resto_scores <- resto_results %>% group_by(INSPECTION.MONTH) %>% 
  summarise(average.score=mean(score, na.rm=T)) %>% arrange(desc(average.score))
resto_scores

