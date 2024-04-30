<<<<<<< HEAD

#Further cleaning after the DataImportandCleaning.R is run: DOH DATA
#converts the 3 date columns to date format and extracts inspection year 
resto <- resto %>% mutate(INSPECTION.DATE = as.Date(INSPECTION.DATE, format = "%m/%d/%Y")) %>%
  mutate(GRADE.DATE = as.Date(GRADE.DATE, format = "%m/%d/%Y")) %>%
  mutate(RECORD.DATE = as.Date(RECORD.DATE, format = "%m/%d/%Y")) %>%
  mutate(INSPECTION.YEAR = lubridate::year(INSPECTION.DATE)) %>%
  filter(nchar(ZIPCODE) == 5) %>% #remove rows with missing zipcode info
  mutate(ZIPCODE = as.factor(ZIPCODE))  #make zip code a factor

#Remove Restarants that have not been inspected and other rows with missing data:
resto <- subset(resto, BORO!="0") #Removes rows with no boro information
resto <- subset(resto, INSPECTION.YEAR!="1900") #removes all rows that did not have an inspection
resto <- resto %>% drop_na(SCORE) #removes missing score rows
resto <- subset(resto, SCORE >= 0) #removes rows with negative scores
resto <- resto %>% drop_na(INSPECTION.TYPE) #remove rows with no inspection type
resto <- resto %>% group_by(CAMIS, INSPECTION.DATE) %>% 
  mutate(score = max(SCORE)) %>% ungroup()  #choose the highest score if more than 1 score listed for the same inspection


#Do we want to limit "inspection_type" values like we did in HW6?

#Cleaning: FOOD POISONING DATA and extracting a new metric that 
#scores each source zip code by % of total Food Poisoning reports for that year
data2 <- read.csv("food_poisoning_dataset.csv")
foodpo <- data2
head(foodpo)
nrow(foodpo)
#Limit to only the reports of food poisoning from a restaurant and remove rows with missing zip code info:
foodpo <- foodpo %>% filter(Location.Type == "Restaurant" | Location.Type =="Restaurant/Bar/Deli/Bakery") %>%
  filter(nchar(Incident.Zip) == 5) %>% mutate(Incident.Zip = as.factor(Incident.Zip)) %>%
  mutate(Created.Date = as.Date(Created.Date, format = "%m/%d/%Y")) %>%  #Convert Date and year info
  mutate(Created.Year = lubridate::year(Created.Date)) %>% filter(Created.Year >= 2015) #limit to 2015 to present (same as resto data)

#Add a feature weighting the row by "Descriptor" field:  "1 or 2" cases = 1 and "3 or more" cases = 2
foodpo <- foodpo %>% mutate(Descriptor = as.factor(Descriptor)) %>% 
  mutate(weight = ifelse(Descriptor == "1 or 2", 1, 2)) %>%
  rename(ZIPCODE = Incident.Zip) %>%  #renamed for easier merging later
  group_by(ZIPCODE, Created.Year)%>%
  summarize(totalw = sum(weight)) %>%
  mutate(Percentage = (totalw / sum(totalw)) * 100) %>%
  complete(Created.Year = full_seq(2015:2024, 1)) %>% #fill in the missing years for each zipcode
  ungroup()  
foodpo <- replace(foodpo, is.na(foodpo), 0) #indicate a zero percentage and weight for years a zip was not reported

#merge into resto main data:
foodpo <- foodpo %>% rename(INSPECTION.YEAR = Created.Year) %>% #change year column name and remove totalw to match in merge
  select(-totalw)
#Join with restaurant data set by zip code and year:
resto <- left_join(resto, foodpo, by = c('ZIPCODE' = 'ZIPCODE', 'INSPECTION.YEAR' = 'INSPECTION.YEAR'))
resto <- resto %>%  rename(Foodpo.perZip.perYear = "Percentage")
=======
#Further cleaning after the DataImportandCleaning.R is run: DOH DATA
#converts the 3 date columns to date format and extracts inspection year 
resto <- resto %>% mutate(INSPECTION.DATE = as.Date(INSPECTION.DATE, format = "%m/%d/%Y")) %>%
  mutate(GRADE.DATE = as.Date(GRADE.DATE, format = "%m/%d/%Y")) %>%
  mutate(RECORD.DATE = as.Date(RECORD.DATE, format = "%m/%d/%Y")) %>%
  mutate(INSPECTION.YEAR = lubridate::year(INSPECTION.DATE)) %>%
  filter(nchar(ZIPCODE) == 5) %>% #remove rows with missing zipcode info
  mutate(ZIPCODE = as.factor(ZIPCODE))  #make zip code a factor

#Remove Restarants that have not been inspected and other rows with missing data:
resto <- subset(resto, BORO!="0") #Removes rows with no boro information
resto <- subset(resto, INSPECTION.YEAR!="1900") #removes all rows that did not have an inspection
resto <- resto %>% drop_na(SCORE) #removes missing score rows
resto <- subset(resto, SCORE >= 0) #removes rows with negative scores
resto <- resto %>% drop_na(INSPECTION.TYPE) #remove rows with no inspection type
resto <- resto %>% group_by(CAMIS, INSPECTION.DATE) %>% 
  mutate(score = max(SCORE)) %>% ungroup()  #choose the highest score if more than 1 score listed for the same inspection


#Do we want to limit "inspection_type" values like we did in HW6?

#Cleaning: FOOD POISONING DATA and extracting a new metric that 
#scores each source zip code by % of total Food Poisoning reports for that year
data2 <- read.csv("food_poisoning_dataset.csv")
foodpo <- data2
head(foodpo)
nrow(foodpo)
#Limit to only the reports of food poisoning from a restaurant and remove rows with missing zip code info:
foodpo <- foodpo %>% filter(Location.Type == "Restaurant" | Location.Type =="Restaurant/Bar/Deli/Bakery") %>%
 filter(nchar(Incident.Zip) == 5) %>% mutate(Incident.Zip = as.factor(Incident.Zip)) %>%
  mutate(Created.Date = as.Date(Created.Date, format = "%m/%d/%Y")) %>%  #Convert Date and year info
  mutate(Created.Year = lubridate::year(Created.Date)) %>% filter(Created.Year >= 2015) #limit to 2015 to present (same as resto data)

#Add a feature weighting the row by "Descriptor" field:  "1 or 2" cases = 1 and "3 or more" cases = 2
foodpo <- foodpo %>% mutate(Descriptor = as.factor(Descriptor)) %>% 
  mutate(weight = ifelse(Descriptor == "1 or 2", 1, 2)) %>%
  rename(ZIPCODE = Incident.Zip) %>%  #renamed for easier merging later
  group_by(ZIPCODE, Created.Year)%>%
  summarize(totalw = sum(weight)) %>%
  mutate(Percentage = (totalw / sum(totalw)) * 100) %>% #create a "Percentage" metric for % of total complaints made each year from each zip code
  complete(Created.Year = full_seq(2015:2024, 1)) %>% #fill in the missing years for each zipcode
  ungroup()  
foodpo <- replace(foodpo, is.na(foodpo), 0) #indicate a zero percentage and weight for years a zip was not reported

#merge into resto main data:
foodpo <- foodpo %>% rename(INSPECTION.YEAR = Created.Year) %>% #change year column name and remove totalw to match in merge
  select(-totalw)
#Join with restaurant data set by zip code and year:
resto <- left_join(resto, foodpo, by = c('ZIPCODE' = 'ZIPCODE', 'INSPECTION.YEAR' = 'INSPECTION.YEAR'))
resto <- resto %>%rename(Foodpo.perZip.perYear = "Percentage")

