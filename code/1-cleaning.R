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
    -`Annual Average Violent Crimes`,
    `Income Ratio`, 
    `Presence of violation`) %>%  
  rename(fips = FIPS, state = State, name = County) %>% 
  mutate(fips = as.double(fips))

View(health_data_clean)

# clean additional features 
feature_data_clean = feature_data %>% 
  select(-starts_with(c("95%", "#")), 
         -ends_with(c("(Black)","(White)","(Hispanic)",   
                      "Mortality", "Mortality Rate", "Ratio")), 
         -`% Disconnected Youth`, 
         -(c(`Segregation Index`, 
             `Segregation index`, 
             `Homicide Rate`, 
             `Firearm Fatalities Rate`, 
             `Other PCP Rate`))) %>% 
  rename(fips = FIPS, state = State, name = County) %>% 
  mutate(fips = as.double(fips))

View(feature_data_clean)

# join county health data with case data
mental_health = inner_join(health_data_clean, 
                        feature_data_clean, 
                        by = c("fips", "state", "name"))

mental_health = left_join(mental_health, 
                           demographic_data_clean, 
                           by = c("fips", "state")) %>% 
  select(-name.y)


View(mental_health)
View(colSums(is.na(mental_health)))

# write cleaned data to file
write_csv(mental_health, file = "data/clean/mental_health.csv")
