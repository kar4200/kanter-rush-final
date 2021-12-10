# load libraries
library(tidyverse)

# clean demographic data 
demographic_data_clean = demographic_data %>%
  select(c(fips, state, name, ends_with("_2019"))) %>%
  select(c(fips, state, name, starts_with("household_has"), housing_mobile_homes_2019, 
         per_capita_income_2019, persons_per_household_2019, 
         veterans_2019))
        
names(demographic_data_clean) = gsub(pattern = "_2019", replacement = "", 
                                  x = names(demographic_data_clean))

write_csv(demographic_data_clean, file = "data/clean/demographic_data_clean.csv")

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

write_csv(health_data_clean, file = "data/clean/health_data_clean.csv")

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

write_csv(feature_data_clean, file = "data/clean/feature_data_clean.csv")

# join county health data with case data
mental_health = inner_join(health_data_clean, 
                        feature_data_clean, 
                        by = c("fips", "state", "name"))

mental_health = left_join(mental_health, 
                           demographic_data_clean, 
                           by = c("fips", "state")) %>% 
  select(-name.y)

# clean the joined datasets 
mental_health_clean = mental_health %>%  
  select(-c(`Years of Potential Life Lost Rate`, `HIV Prevalence Rate`, 
            starts_with("poverty"), `% LBW`, 
            `% Frequent Mental Distress`, `% Fair/Poor`, `80th Percentile Income`,  
            `20th Percentile Income`, `% Frequent Physical Distress`, `% Uninsured...56`, 
            `% Uninsured...60`, Population)) %>% 
  na.omit() %>% 
  rename(name = name.x, prevent_hosp_rate = `Preventable Hosp. Rate`)

# table to determine which columns to remove 
colSums(is.na(mental_health))

# rename columns for tidyness 
names(mental_health_clean) = gsub(pattern = "% ", replacement = "perc ", 
                                  x = names(mental_health_clean))
names(mental_health_clean) = gsub(pattern = " ", replacement = "_", 
                                  x = names(mental_health_clean))
names(mental_health_clean) = gsub(pattern = "-", replacement = "_", 
                                  x = names(mental_health_clean))
names(mental_health_clean) = gsub(pattern = "<", replacement = "under", 
                                  x = names(mental_health_clean))
names(mental_health_clean) = gsub(pattern = "/", replacement = "_", 
                                  x = names(mental_health_clean))
names(mental_health_clean) = tolower(names(mental_health_clean))

# create binary response variable (mentally unhealthy: yes or no)
mental_health_clean %>% 
  summarise(mean = mean(mentally_unhealthy_days))

mental_health_clean = mental_health_clean %>%  
  mutate(mentally_unhealthy = ifelse(mentally_unhealthy_days >= 3.95, 1, 0) )

# write cleaned data to file
write_csv(mental_health_clean, file = "data/clean/mental_health_clean.csv")
