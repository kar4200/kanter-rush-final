# load libraries
library(tidyverse)
library(glmnetUtils)                    # to run ridge and lasso
library(pROC)   
source("code/functions/plot_glmnet.R")            # for lasso/ridge trace plots

# read in the training data
mental_health_train = read_csv("data/clean/mental_health_train.csv")

# running a logistic regression
glm_fit = glm(mentally_unhealthy ~ . -mentally_unhealthy_days, 
              family = "binomial",
              data = mental_health_train)

summary(glm_fit)

## extracting elements of the fit
coef(glm_fit)

# extracting the fitted probabilities 
fitted_probabilities = predict(glm_fit, 
                               newdata = mental_health_test,
                               type = "response")   

head(fitted_probabilities)

# make predictions 
predictions = as.numeric(fitted_probabilities > 0.5)
head(predictions)

# evaluating the classifier 
mental_health_test = mental_health_test %>% 
  mutate(predicted_mental_health = predictions)

# then calculate misclassification rate
mental_health_test %>% 
  summarise(mean(mentally_unhealthy != predicted_mental_health))

# confusion matrix 
mental_health_test %>% 
  select(mentally_unhealthy, predicted_mental_health) %>%
  table()

fpr = 

# ROC curve
roc_data = roc(mental_health_test %>% pull(mentally_unhealthy), 
               fitted_probabilities) 
tibble(FPR = 1-roc_data$specificities,
       TPR = roc_data$sensitivities) %>%
  ggplot(aes(x = FPR, y = TPR)) + 
  geom_line() + 
  geom_abline(slope = 1, linetype = "dashed") +
  geom_point(x = fpr, y = 1-fnr, colour = "red") +
  theme_bw()

# print the AUC
roc_data$auc

