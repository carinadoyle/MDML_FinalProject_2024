#TEST
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
library(cvms) # plot confusion matrices

data <- read.csv("DOHMH_New_York_City_Restaurant_Inspection_Results_20240416.csv")
resto <- data
head(resto)
# we have so much data !! <3 
nrow(resto)

# recoding violations, so it is a binary prediction - closed (1) or not closed (0)
resto$ACTION[resto$ACTION=="Establishment re-closed by DOHMH." |
               resto$ACTION=="Establishment Closed by DOHMH. Violations were cited in the following area(s) and those requiring immediate action were addressed."] <- 1
resto$ACTION[resto$ACTION=="No violations were recorded at the time of this inspection." |
               resto$ACTION=="Establishment re-opened by DOHMH."|
               resto$ACTION=="Violations were cited in the following area(s)."] <- 0
resto$ACTION <- as.numeric(resto$ACTION)

# removing cuisines that are presents only after 2022
resto <- resto %>% filter(CUISINE.DESCRIPTION!="Armenian",
                          CUISINE.DESCRIPTION!="Brazilian",
                          CUISINE.DESCRIPTION!="Indonesian",
                          CUISINE.DESCRIPTION!="Pancakes/Waffles")

