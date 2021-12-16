# load libraries
library(tidyverse)

# read in the cleaned data
mental_health_clean = read_csv("data/clean/mental_health_clean.csv")

# read in training data 
mental_health_train = read_csv("data/clean/mental_health_train.csv")

# health outcomes 
# physically unhealthy days
p9 = ggplot(data = mental_health_train,                
       aes(x = physically_unhealthy_days, 
           y = mentally_unhealthy)) +
  geom_point() +
  stat_smooth(method="glm", se=FALSE, 
              method.args = list(family=binomial), 
              color = "red") + 
  theme_bw() +
  labs(x = "Physically Unhealthy Days", y = "Mentally Unhealthy") +
  ggtitle("Physically Unhealthy Days")

ggsave(filename = "results/physically_unhealthy_days.png", 
       plot = p9, 
       device = "png", 
       width = 5, 
       height = 5)

# health behaviors 
# smoking 
p11 = ggplot(data = mental_health_train,                
             aes(x = perc_smokers, 
                 y = mentally_unhealthy)) +
  geom_point() +
  stat_smooth(method="glm", se=FALSE, 
              method.args = list(family=binomial), 
              color = "red") + 
  theme_bw() +
  labs(x = "Percentage of adults that reported currently smoking", y = "Mentally Unhealthy") +
  ggtitle("Smoking")

ggsave(filename = "results/smoking.png", 
       plot = p11, 
       device = "png", 
       width = 5, 
       height = 5)

# clinical care
# mhp rate  
p13 = ggplot(data = mental_health_train,                
             aes(x = mhp_rate, 
                 y = mentally_unhealthy)) +
  geom_point() +
  xlab("Mental Health Providers per 100,000 population") +
  ylab("Mentally Unhealthy") +
  stat_smooth(method="glm", se=FALSE, 
              method.args = list(family=binomial), 
              color = "red") + 
  theme_bw() +
  ggtitle("MHP Providers")

ggsave(filename = "results/mhp_rate.png", 
       plot = p13, 
       device = "png", 
       width = 5, 
       height = 5)

# social / economic environment 
# household income 
p14 = ggplot(data = mental_health_train,                
             aes(x = household_income, 
                 y = mentally_unhealthy)) +
  geom_point() +
  xlab("Household Income") +
  ylab("Mentally Unhealthy") +
  stat_smooth(method="glm", se=FALSE, 
              method.args = list(family=binomial), 
              color = "red") + 
  theme_bw() +
  ggtitle("Household Income")

ggsave(filename = "results/household_income.png", 
       plot = p14, 
       device = "png", 
       width = 5, 
       height = 5)

# physical environment 
# inadequate facilities 
p16 = ggplot(data = mental_health_train,                
             aes(x = inadequate_facilities, 
                 y = mentally_unhealthy)) +
  geom_point() +
  xlab("Percentage of households without kitchen or plumbing facilities") +
  ylab("Mentally Unhealthy") +
  stat_smooth(method="glm", se=FALSE, 
              method.args = list(family=binomial), 
              color = "red") + 
  theme_bw() +
  ggtitle("Inadequate Household Facilities")

ggsave(filename = "results/inadequate_facilities.png", 
       plot = p16, 
       device = "png", 
       width = 5, 
       height = 5)

p17 = ggplot(data = mental_health_train,                
             aes(x = perc_excessive_drinking, 
                 y = mentally_unhealthy)) +
  geom_point() +
  xlab("Percentage of adults that reported excessive drinking") +
  ylab("Mentally Unhealthy") +
  stat_smooth(method="glm", se=FALSE, 
              method.args = list(family=binomial), 
              color = "red") + 
  theme_bw() +
  ggtitle("Excessive Drinking")

ggsave(filename = "results/excessive-drinking.png", 
       plot = p17, 
       device = "png", 
       width = 5, 
       height = 5)

