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
                y = mentally_unhealthy_days)) +
  geom_point() +
  geom_jitter() +
  stat_smooth(method="lm", se=FALSE, 
              color = "red") + 
  theme_bw() +
  labs(x = "Physically Unhealthy Days", 
       y = "Mentally Unhealthy Days") +
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
                 y = mentally_unhealthy_days)) +
  geom_point() +
  geom_jitter() +
  stat_smooth(method="lm", se=FALSE,
              color = "red") + 
  theme_bw() +
  labs(x = "Percentage of Adults that Reported Currently Smoking", 
       y = "Mentally Unhealthy Days") +
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
                 y = mentally_unhealthy_days)) +
  geom_point() +
  geom_jitter() +
  xlab("Mental Health Providers Per 100,000 Population") +
  ylab("Mentally Unhealthy Days") +
  stat_smooth(method="lm", se=FALSE,
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
                 y = mentally_unhealthy_days)) +
  geom_point() +
  geom_jitter() +
  xlab("Household Income") +
  ylab("Mentally Unhealthy Days") +
  stat_smooth(method="glm", se=FALSE,
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
             aes(x = overcrowding, 
                 y = mentally_unhealthy_days)) +
  geom_point() +
  geom_jitter() +
  xlab("Percentage of Households with Overcrowding") +
  ylab("Mentally Unhealthy Days") +
  stat_smooth(method="lm", se=FALSE, 
              color = "red") + 
  theme_bw() +
  ggtitle("Overcrowding")

ggsave(filename = "results/overcrowding.png", 
       plot = p16, 
       device = "png", 
       width = 5, 
       height = 5)

# excessive drinking: to observe data
p17 = ggplot(data = mental_health_train,                
             aes(x = perc_excessive_drinking, 
                 y = mentally_unhealthy_days)) +
  geom_point() +
  geom_jitter() +
  xlab("Percentage of Adults that Reported Excessive Drinking") +
  ylab("Mentally Unhealthy Days") +
  stat_smooth(method="lm", se=FALSE,
              color = "red") + 
  theme_bw() +
  ggtitle("Excessive Drinking")

ggsave(filename = "results/excessive-drinking.png", 
       plot = p17, 
       device = "png", 
       width = 5, 
       height = 5)

# demographic information
# percent female
p18 = ggplot(data = mental_health_train,                
             aes(x = perc_female, 
                 y = mentally_unhealthy_days)) +
  geom_point() +
  geom_jitter() +
  xlab("Percentage Female Population") +
  ylab("Mentally Unhealthy Days") +
  stat_smooth(method="lm", se=FALSE,
              color = "red") + 
  theme_bw() +
  ggtitle("Percentage Female")

ggsave(filename = "results/female.png", 
       plot = p18, 
       device = "png", 
       width = 5, 
       height = 5)
