# load libraries
library(tidyverse)

# clean demographic data 
demographic_data_clean = demographic_data %>%
  select(c(fips, state, name, ends_with("_2019"))) %>%
  select(c(fips, state, name, starts_with("household_has"), 
           housing_mobile_homes_2019, per_capita_income_2019, 
           persons_per_household_2019, veterans_2019))
        
names(demographic_data_clean) = gsub(pattern = "_2019", replacement = "", 
                                  x = names(demographic_data_clean))

write_csv(demographic_data_clean, file = "data/clean/demographic_data_clean.csv")

# clean health data
health_data_clean = health_data %>%
  mutate(`Presence of violation` = 
           ifelse(`Presence of violation` == "Yes", 1, 0)) %>% 
  select(-c(starts_with(c("95%","Quartile", "#")), 
            ends_with(c("(Black)", "(White)", "(Hispanic)", "Ratio")), 
            c(Unreliable, 
              `Cohort Size`,
              `Annual Average Violent Crimes`,
              `Severe Housing Cost Burden`,
              `% Alcohol-Impaired`)), 
         c(`Income Ratio`)) %>%  
  rename(fips = FIPS, state = State, name = County) %>% 
  mutate(fips = as.double(fips))

write_csv(health_data_clean, file = "data/clean/health_data_clean.csv")

# clean additional features 
feature_data_clean = feature_data %>% 
  select(-c(starts_with(c("95%", "#")), 
         ends_with(c("(Black)","(White)","(Hispanic)",   
                      "Mortality", "Mortality Rate", "Ratio")), 
         c(`% Disconnected Youth`, 
             `Segregation index`, 
             `Segregation Index`, 
             `Homicide Rate`, 
             `Firearm Fatalities Rate`, 
             `Other PCP Rate`, 
             `Population`))) %>% 
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
  select(-c(`Years of Potential Life Lost Rate`, 
            `HIV Prevalence Rate`, 
            starts_with("poverty"), `% LBW`, 
            `% Frequent Mental Distress`, 
            `% Fair/Poor`, 
            `80th Percentile Income`,  
            `20th Percentile Income`, 
            `% Frequent Physical Distress`, 
            `% Uninsured...56`, 
            `% Uninsured...60`)) %>% 
  na.omit() %>% 
  rename(name = name.x, 
         prevent_hosp_rate = `Preventable Hosp. Rate`,
         perc_long_commute_drives_alone = `% Long Commute - Drives Alone`)

# table to determine which columns to remove 
colSums(is.na(mental_health_clean))

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

min(mental_health_clean$mentally_unhealthy_days) # 2.5
max(mental_health_clean$mentally_unhealthy_days) # 6

# set threshold for mentally unhealthy (yes vs. no) at 4.5 (justify later)
mental_health_clean = mental_health_clean %>%  
  mutate(mentally_unhealthy = ifelse(mentally_unhealthy_days >= 4.5, 1, 0))

# write cleaned data to file
write_csv(mental_health_clean, file = "data/clean/mental_health_clean.csv")
  
