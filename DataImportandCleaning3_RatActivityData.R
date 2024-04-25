
#Cleaning/Merging: PEST CONTROL DATA
data3 <- read.csv("Rodent_Inspection.csv")
rodent <- data3
head(rodent)
nrow(rodent)

#filter to observed cases of "Rat activity" only in RESULT column and clean up Zip code and date info
rodent <- rodent %>% filter(RESULT == "Rat Activity") %>%
  mutate(INSPECTION_DATE = as.Date(INSPECTION_DATE, format = "%m/%d/%Y")) %>%  #Convert Date and year info
  mutate(INSPECTION_YEAR = lubridate::year(INSPECTION_DATE)) %>% 
  filter(INSPECTION_YEAR >= 2015) %>% #Limit to the other data date range (2015-2024) 
  filter(nchar(ZIP_CODE) == 5) %>% mutate(ZIP_CODE = as.factor(ZIP_CODE)) %>%
  rename(ZIPCODE = ZIP_CODE)   #renamed for easier merging later

#Add features to measure by zip code and year 
rodent <- rodent %>% group_by(INSPECTION_YEAR) %>%
  mutate(all_observations_byYear = n()) %>% ungroup() #find count of observations per year

rodent <- rodent %>% group_by(ZIPCODE, INSPECTION_YEAR) %>% #create a "Percentage" metric for % of total rat activity reports made each year from each zip code
  reframe(n_group = n(), Percentage = n_group/all_observations_byYear * 100) %>% ungroup() 

rodent <- rodent %>% distinct() %>% #keep all unique rows (year/zipcode combos)
  group_by(ZIPCODE) %>% complete(INSPECTION_YEAR = full_seq(2015:2024, 1)) %>% #fill in all years for each zip that was not reported
  ungroup() 

rodent <- replace(rodent, is.na(rodent), 0) #fill in zero percentage for years/zips that did not report any rat activity

#adjust column names and remove n_roups to prep for merge with main resto data
rodent <- rodent %>% rename(INSPECTION.YEAR = INSPECTION_YEAR) %>%
  select(-n_group)
#Join with restaurant data set by zip code and year:
resto <- left_join(resto, rodent, by = c('ZIPCODE' = 'ZIPCODE', 'INSPECTION.YEAR' = 'INSPECTION.YEAR'))
resto <- resto %>% rename(RatActivity.perZip.perYear = "Percentage")