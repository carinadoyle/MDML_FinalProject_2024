library(haven) #import data
library(tidyverse)
library(dplyr) #data mining/ data cleaning
library(naniar) #replace with NA
library(leaps) #best subset
library(glmnet) # lasso
library(caret) # KNN 
library(ggplot2) # plotting results
library(randomForest) # Random forest classifier
library(pROC) # AU-ROC curves
library(ROCR) # AU-ROC curves
library(DescTools) # mode imputation

data <- read.csv("DOHMH_New_York_City_Restaurant_Inspection_Results_20240416.csv")
resto <- data
head(resto)
# we have so much data !! <3 
nrow(resto)

### mice imputation?
# for now I am just removing NAs

# recoding violations, so it is a binary prediction - violation (1) or no violation (0)
resto$ACTION[resto$ACTION=="Violations were cited in the following area(s)." |
               resto$ACTION=="Establishment Closed by DOHMH. Violations were cited in the following area(s) and those requiring immediate action were addressed."] <- 1
resto$ACTION[resto$ACTION=="No violations were recorded at the time of this inspection." |
               resto$ACTION=="Establishment re-opened by DOHMH."|
               resto$ACTION=="Establishment re-closed by DOHMH."] <- 0
resto$ACTION <- as.numeric(resto$ACTION)


# this can maybe be deleted, it's for data balancing if we decide to do that
# string of column names with NAs to replace
resto <- resto %>% select(-Location.Point1)
missing_names <- colnames(resto)[colSums(is.na(resto)) > 0]

# loop to replace NAs with the mode for each question
for (i in missing_names){
  mode <- Mode(na.omit(resto[[i]]))[1]
  resto[[i]][which(is.na(resto[[i]]))] <- as.numeric(mode)
}



