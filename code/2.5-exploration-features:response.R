# load libraries
library(tidyverse)

# read in the cleaned data
mental_health = read_csv("data/clean/mental_health_clean.csv")

# health outcomes 
# physically unhealthy days
p9 = ggplot(data = mental_health,                
       aes(x = physically_unhealthy_days, 
           y = mentally_unhealthy)) +
  geom_point() +
  stat_smooth(method="glm", se=FALSE, 
              method.args = list(family=binomial), 
              color = "red") + 
  theme_bw()

ggsave(filename = "results/physically_unhealthy_days.png", 
       plot = p9, 
       device = "png", 
       width = 5, 
       height = 5)

colnames(mental_health)

# health behaviors 
# insufficient sleep
p10 = ggplot(data = mental_health,                
            aes(x = perc_insufficient_sleep, 
                y = mentally_unhealthy)) +
  geom_point() +
  xlab("Insufficient Sleep (%)") +
  ylab("Mentally Unhealthy") +
  stat_smooth(method="glm", se=FALSE, 
              method.args = list(family=binomial), 
              color = "red") + 
  theme_bw()

ggsave(filename = "results/insufficient_sleep.png", 
       plot = p10, 
       device = "png", 
       width = 5, 
       height = 5)

# smoking 
p11 = ggplot(data = mental_health,                
             aes(x = perc_smokers, 
                 y = mentally_unhealthy)) +
  geom_point() +
  xlab("Insufficient Sleep (%)") +
  ylab("Smokers (%)") +
  stat_smooth(method="glm", se=FALSE, 
              method.args = list(family=binomial), 
              color = "red") + 
  theme_bw()

ggsave(filename = "results/smoking.png", 
       plot = p11, 
       device = "png", 
       width = 5, 
       height = 5)

# obese 
p12 = ggplot(data = mental_health,                
             aes(x = perc_obese, 
                 y = mentally_unhealthy)) +
  geom_point() +
  xlab("Obese (%)") +
  ylab("Mentally Unhealthy") +
  stat_smooth(method="glm", se=FALSE, 
              method.args = list(family=binomial), 
              color = "red") + 
  theme_bw()

ggsave(filename = "results/obese.png", 
       plot = p12, 
       device = "png", 
       width = 5, 
       height = 5)

# clinical care
# mhp rate  
p13 = ggplot(data = mental_health,                
             aes(x = mhp_rate, 
                 y = mentally_unhealthy)) +
  geom_point() +
  xlab("Mental Health Provider Access (%)") +
  ylab("Mentally Unhealthy") +
  stat_smooth(method="glm", se=FALSE, 
              method.args = list(family=binomial), 
              color = "red") + 
  theme_bw()

ggsave(filename = "results/mhp_rate.png", 
       plot = p13, 
       device = "png", 
       width = 5, 
       height = 5)

# social / economic environment 
# household income 
p14 = ggplot(data = mental_health,                
             aes(x = household_income, 
                 y = mentally_unhealthy)) +
  geom_point() +
  xlab("Household Income") +
  ylab("Mentally Unhealthy") +
  stat_smooth(method="glm", se=FALSE, 
              method.args = list(family=binomial), 
              color = "red") + 
  theme_bw()

ggsave(filename = "results/household_income.png", 
       plot = p14, 
       device = "png", 
       width = 5, 
       height = 5)

# association rate 
p15 = ggplot(data = mental_health,                
             aes(x = association_rate, 
                 y = mentally_unhealthy)) +
  geom_point() +
  xlab("Association Rate") +
  ylab("Mentally Unhealthy") +
  stat_smooth(method="glm", se=FALSE, 
              method.args = list(family=binomial), 
              color = "red") + 
  theme_bw()

ggsave(filename = "results/association_rate.png", 
       plot = p15, 
       device = "png", 
       width = 5, 
       height = 5)

# physical environment 
# inadequate facilities 
p16 = ggplot(data = mental_health,                
             aes(x = inadequate_facilities, 
                 y = mentally_unhealthy)) +
  geom_point() +
  xlab("Inadequate Facilities") +
  ylab("Mentally Unhealthy") +
  stat_smooth(method="glm", se=FALSE, 
              method.args = list(family=binomial), 
              color = "red") + 
  theme_bw()

ggsave(filename = "results/inadequate_facilities.png", 
       plot = p16, 
       device = "png", 
       width = 5, 
       height = 5)
