# load libraries
library(tidyverse)
library(glmnetUtils)                             
library(pROC)   
source("code/functions/plot_glmnet.R")       

# change 
# read in the training data
mental_health_train = read_csv("data/clean/mental_health_train.csv")

# read in the testing data
mental_health_test = read_csv("data/clean/mental_health_test.csv")

# running a logistic regression
lm_fit = lm(mentally_unhealthy_days ~ . -physically_unhealthy_days, 
              data = mental_health_train)

summary(lm_fit)

## extracting elements of the fit
coef(lm_fit)

# r-squared value
summary(lm_fit)$r.squared # 0.8274298

# check for distribution and homoscedasticity of the residuals to make sure we do not have to log data  
hist(lm_fit$residuals)
plot(lm_fit$residuals)
# add to appendix - but everything is good 

# extracting the fitted probabilities 
fitted_probabilities_test = predict(lm_fit, 
                               newdata = mental_health_test,
                               type = "response") 

fitted_probabilities_train = predict(lm_fit, 
                               newdata = mental_health_train,
                               type = "response")   

# evaluating the classifier - calculate rmse for train and test
RMSE_test_lm = sqrt(mean((fitted_probabilities_test - mental_health_test$mentally_unhealthy_days)^2)) %>%
  data.frame()

RMSE_train_lm = sqrt(mean((fitted_probabilities_train - mental_health_train$mentally_unhealthy_days)^2)) %>%
  data.frame()

write_csv(RMSE_test_lm, 
          file = "results/rmse_test_lm.csv")

write_csv(RMSE_train_lm, 
          file = "results/rmse_train.lm.csv")
