<<<<<<< HEAD

=======
#TEST
>>>>>>> 4ff8b065ecf0a8c7e20302647b5267587327c226
#Additional cleanup for the 2 created features Foodpo.perZip.perYear and RatActivity.perZip.perYear
#because some zip codes were not reported at all in the food poisoning and rodent datasets, 
#these fields have some NA values that should be zero:

resto$Foodpo.perZip.perYear[is.na(resto$Foodpo.perZip.perYear)] <- 0
resto$RatActivity.perZip.perYear[is.na(resto$RatActivity.perZip.perYear)] <- 0

#Cleaning: NEIGHBORHOOD DATA
data4 <- readxl::read_excel("nyc2010census_tabulation_equiv.xlsx", 3)
neibrhd <- data4
head(rodent)
nrow(rodent)

#rename some columns and limit to those that can pinpoint neighborhood
neibrhd <- neibrhd %>% mutate(NTA = as.factor(NTA.Code)) %>%
  select(NTA, Neighborhood.Name)
neibrhd <- neibrhd %>% distinct()

#Prep and merge with resto data
resto <- resto %>% mutate(NTA = as.factor(NTA)) %>% mutate(BORO = as.factor(BORO))
resto <- left_join(resto, neibrhd, by = c('NTA' = 'NTA'))

resto <- resto %>% drop_na(Neighborhood.Name)  #drop any rows that are missing Neighborhood data

#If we want to filter to complete cases only (code below), we will still have almost 100K observations (99747):
<<<<<<< HEAD
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

# for the other variables, I'm cool imputing with MICE or mode/mean or we can just filter complete cases. 
# for now, just doing complete cases here
nrow(resto)
resto <- resto %>% filter(complete.cases(.))
nrow(resto)

# inspection_month
resto$INSPECTION.MONTH <- month(resto$INSPECTION.DATE)
# building >1000 or less
resto$BUILDING <- ifelse(resto$BUILDING>1000, 1, 0)
# record day
resto$GRADE.DAY <- weekdays(resto$GRADE.DATE)

# two tables
# auc by model
# accuracy by model

# auc graph

# top 10 restaurants likely to have a violation in 2023 and 2024
# top 10 neighborhoods likely to have a violation in 2023 and 2024


pest_words <- c("rat","mice","mouse","roach","fly","flies","vermin", 
                "pests", "rodents", "insects", "pest", "roaches")
# creating a binary feature for whether or not a violation was due to pests
# also, just noticed we have some empties in description variable. do we want to drop those?
resto$pests <- ifelse(rowSums(sapply(pest_words, grepl, resto$VIOLATION.DESCRIPTION)>=1), 1, 0)

table(resto$pests)
# ok, this is about balanced. I think it's a good outcome variable

str(resto)

# final variables to use in our model : zip code, inspection year, inspection month, 
# grade day of week, building, boro, cuisine.description, action, critical.flag, as.numeric(score), 
# grade, inspection.type, community board, council district, inspection.year, 

# dropping all other variables
resto <- resto %>% select(-CAMIS, -DBA, -STREET, -PHONE, -INSPECTION.DATE, -VIOLATION.CODE, -VIOLATION.DESCRIPTION, 
                          -SCORE, -GRADE.DATE, -RECORD.DATE, -Latitude, -Longitude, -Census.Tract, -BIN, -BBL, 
                          -NTA, -ACTION, -BORO)

# stratifying data to only be lower manhattan (34th st and below) zip codes
zip_list <- c(10001, 10010, 10011, 10003, 10009, 10014, 10012, 10002, 10013, 10007, 10048, 10038, 10006, 10005, 10004)

resto <- resto %>% filter(ZIPCODE%in%zip_list)
str(resto)

# to remove categorical variables with too many levels, use the commented code below
#  resto <- resto %>% select(-Foodpo.perZip.perYear, -RatActivity.perZip.perYear, -ZIPCODE, 
# -Neighborhood.Name, -CUISINE.DESCRIPTION)

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

str(resto)

# I think we should pull out 2023 and 2024 as our years to predict for our model
resto_assess <- resto %>% filter(INSPECTION.YEAR==2023|INSPECTION.YEAR==2024) %>% select(-INSPECTION.YEAR)
resto <- resto %>% filter(INSPECTION.YEAR!=2023 & INSPECTION.YEAR!=2024) %>% select(-INSPECTION.YEAR)

str(resto)

=======
#resto <- resto %>% filter(complete.cases(.))
>>>>>>> 4ff8b065ecf0a8c7e20302647b5267587327c226
