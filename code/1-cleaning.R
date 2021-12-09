# load libraries
library(lubridate)
library(tidyverse)

# clean demographic data 
demographic_data_clean = demographic_data %>%
  select(fips,                        
         state,
         name,
         ends_with("2019"),
         -households_2019)          

View(demographic_data_clean)

# clean health data
health_data_clean = health_data %>%
  mutate(`Presence of violation` = 
           ifelse(`Presence of violation` == "Yes", 1, 0)) %>% 
  select(-starts_with("95%"),
    -starts_with("Quartile"),
    -starts_with("#"),
    -ends_with("(Black)"),
    -ends_with("(White)"),
    -ends_with("(Hispanic)"),
    -ends_with("Ratio"),
    -Unreliable,
    -Population,
    -`Cohort Size`,
    -`Labor Force`,
    `Income Ratio`, 
    `Presence of violation`) 

View(health_data_clean)

# clean additional features 
feature_data_clean = feature_data %>% 
  select


# join county health data with case data
covid_data = inner_join(county_health_data, case_data, by = "fips")

# write cleaned data to file
write_tsv(covid_data, file = "data/clean/covid_data.tsv")