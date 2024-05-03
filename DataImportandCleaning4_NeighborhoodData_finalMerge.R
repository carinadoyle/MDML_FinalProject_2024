#TEST
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
#resto <- resto %>% filter(complete.cases(.))
