# load libraries
library(tidyverse)
library(glmnetUtils)                             
library(pROC)   
source("code/functions/plot_glmnet.R")       

# read in the training data
mental_health_train = read_csv("data/clean/mental_health_train.csv")

# read in the testing data
mental_health_test = read_csv("data/clean/mental_health_test.csv")

# running a logistic regression
set.seed(1)
glm_fit = glm(mentally_unhealthy ~ . -mentally_unhealthy_days
                                     -physically_unhealthy_days, 
              family = "binomial",
              data = mental_health_train)

summary(glm_fit)

## extracting elements of the fit
coef(glm_fit)

# extracting the fitted probabilities 
fitted_probabilities = predict(glm_fit, 
                               newdata = mental_health_test,
                               type = "response")   

# make predictions 
predictions = as.numeric(fitted_probabilities > 0.3) # this halves fn rate in half but what is threshold we should go to?
head(predictions)

# evaluating the classifier 
mental_health_test = mental_health_test %>% 
  mutate(predicted_mental_health = predictions)

# calculate misclassification rate
misclassification_regression = mental_health_test %>% 
  summarise(mean(mentally_unhealthy != predicted_mental_health))

write_csv(misclassification_regression, 
          file = "results/misclassification_regression.csv")

# confusion matrix 
mental_health_test %>% 
  select(mentally_unhealthy, predicted_mental_health) %>%
  table()

fpr = 20 / (370 + 20)
fnr = 19 / (19 + 65)

# ROC curve
roc_data = roc(mental_health_test %>% 
                 pull(mentally_unhealthy), 
               fitted_probabilities) 

p_roc = tibble(FPR = 1-roc_data$specificities,
       TPR = roc_data$sensitivities) %>%
  ggplot(aes(x = FPR, y = TPR)) + 
  geom_line() + 
  xlab("False Positive Rate (FPR)") +
  ylab("True Positive Rate (TPR)") +
  geom_abline(slope = 1, linetype = "dashed", color = "red") +
  geom_point(x = fpr, y = 1-fnr, colour = "red") +
  theme_bw()

ggsave(filename = "results/roc-table.png", 
       plot = p_roc, 
       device = "png", 
       width = 5, 
       height = 5)

# print the AUC
roc_data$auc

