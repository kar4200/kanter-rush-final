# load libraries
library(tidyverse)
library(ggcorrplot)

# read in the cleaned data
mental_health = read_csv("data/clean/mental_health_clean.csv")

# correlated features - health_outcomes
health_outcomes = mental_health %>% 
  select(-c(fips, state, name)) %>% 
  select(physically_unhealthy_days, 
         life_expectancy, perc_diabetic)

p4 = ggcorrplot(cor(health_outcomes),
                colors = c("blue", "white", "red"), 
                hc.order = TRUE, 
                type = "lower",
                lab = TRUE,
                lab_size = 4,
                ggtheme = ggplot2::theme_bw())

ggsave(filename = "results/response-health_outcomes.png", 
       plot = p4, 
       device = "png", 
       width = 5, 
       height = 5)

# correlated features - health-behavior
health_behavior = mental_health %>% 
  select(-c(fips, state, name)) %>% 
  select(perc_smokers, perc_obese, food_environment_index,
         perc_physically_inactive, perc_with_access, perc_excessive_drinking,
         perc_alcohol_impaired, chlamydia_rate, teen_birth_rate, perc_limited_access,
         perc_insufficient_sleep, perc_food_insecure)

p5 = ggcorrplot(cor(health_behavior),
                colors = c("blue", "white", "red"), 
                hc.order = TRUE, 
                type = "lower",
                lab = TRUE,
                lab_size = 2,
                tl.cex = 7,
                ggtheme = ggplot2::theme_bw())

ggsave(filename = "results/response-health_behavior.png", 
       plot = p5, 
       device = "png", 
       width = 5, 
       height = 5)

# correlated features - clinical care
clinical_care = mental_health %>% 
  select(-c(fips, state, name)) %>% 
  select(perc_uninsured, pcp_rate, 
         dentist_rate, mhp_rate, 
         prevent_hosp_rate,
         perc_mammography_screened, 
         perc_flu_vaccinated)

p6 = ggcorrplot(cor(clinical_care),
           colors = c("blue", "white", "red"),
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           tl.cex = 8,
           ggtheme = ggplot2::theme_bw())

ggsave(filename = "results/clinical-care.png", 
       plot = p6, 
       device = "png", 
       width = 5, 
       height = 5)

# correlated features - social_environment
social_economic_environment = mental_health %>% 
  select(-c(fips, state, name)) %>% 
  select(graduation_rate, perc_some_college, 
         perc_unemployed, perc_children_in_poverty, 
         perc_single_parent_households, 
         association_rate, violent_crime_rate, 
         injury_death_rate, income_ratio, 
         household_income, perc_free_or_reduced_lunch, 
         perc_homeowners, perc_severe_housing_cost_burden, 
         household_has_broadband, household_has_computer, 
         household_has_smartphone, housing_mobile_homes, 
         housing_one_unit_structures, housing_two_unit_structures, 
         per_capita_income, persons_per_household)

p7 = ggcorrplot(cor(social_economic_environment), 
           hc.order = FALSE, 
           type = "lower",
           lab = TRUE,
           lab_size = 1.5,
           tl.cex = 6.5,
           ggtheme = ggplot2::theme_bw())

ggsave(filename = "results/social-economic-environment.png", 
       plot = p7, 
       device = "png", 
       width = 6, 
       height = 6)

physical_environment = mental_health %>% 
  select(-c(fips, state, name)) %>% 
  select(average_daily_pm2.5, presence_of_violation, perc_severe_housing_problems,
         severe_housing_cost_burden, overcrowding,
         inadequate_facilities, perc_drive_alone, perc_long_commute__drives_alone,
         perc_rural)

p8 = ggcorrplot(cor(physical_environment),
           colors = c("blue", "white", "red"), 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE,
           lab_size = 2,
           tl.cex = 6.5,
           outline.color = "gray",
           ggtheme = ggplot2::theme_bw())

ggsave(filename = "results/physical-environment.png", 
       plot = p8, 
       device = "png", 
       width = 5, 
       height = 5)
 