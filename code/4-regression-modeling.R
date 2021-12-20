# load libraries
library(tidyverse)

# read in the training data
mental_health_train = read_csv("data/clean/mental_health_train.csv")

# read in the testing data
mental_health_test = read_csv("data/clean/mental_health_test.csv")

# running a logistic regression
lm_fit = lm(mentally_unhealthy_days ~ . -physically_unhealthy_days, 
              data = mental_health_train)

summary(lm_fit)

save(lm_fit, file = "results/lm_fit.Rda")

# extracting elements of the fit
coef(lm_fit)

# r-squared value
summary(lm_fit)$r.squared # 0.8274298