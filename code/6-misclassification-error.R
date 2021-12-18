# loading packages 
library(tidyverse)

# reading in misclassification errors 
misclassification_test_regression = 
  read_csv("results/misclassification_test_regression.csv")
misclassification_train_regression = 
  read_csv("results/misclassification_train_regression.csv")

misclassification_test_ridge = 
  read_csv("results/misclassification_test_ridge.csv")
misclassification_train_ridge = 
  read_csv("results/misclassification_train_ridge.csv")

misclassification_test_lasso = 
  read_csv("results/misclassification_test_lasso.csv")
misclassification_train_lasso = 
  read_csv("results/misclassification_train_lasso.csv")

misclassification_test_decision = 
  read_csv("results/misclassification_test_decision.csv")
misclassification_train_decision = 
  read_csv("results/misclassification_train_decision.csv")

misclassification_test_rf = 
  read_csv("results/misclassification_test_rf.csv")
misclassification_train_rf = 
  read_csv("results/misclassification_train_rf.csv")

# renaming column names 
misclassification_test_regression = misclassification_test_regression %>% 
  rename(value = `mean(mentally_unhealthy != predicted_mental_health)`)
 
misclassification_test_ridge = misclassification_test_ridge %>% 
   rename(value = `mean(mentally_unhealthy != predicted_mental_health)`)
 
misclassification_test_lasso = misclassification_test_lasso %>% 
   rename(value = `mean(mentally_unhealthy != predicted_mental_health)`)

misclassification_test_decision = misclassification_test_decision %>% 
  rename(value = `mean(mentally_unhealthy != predicted_mental_health)`)

# creating table of misclassification errors 
Model = c("Regression", "Ridge", "Lasso", "Decision", "Random Forest")

misclassification_test =  
   as_tibble(cbind(Model, rbind(misclassification_test_regression, 
      misclassification_test_ridge, 
      misclassification_test_lasso, 
      misclassification_test_decision, 
      misclassification_test_rf))) %>% 
  rename(`Test Error` = value) 

 
misclassification_train = cbind(misclassification_train_regression, 
            misclassification_train_ridge, 
            misclassification_train_lasso, 
            misclassification_train_decision, 
            misclassification_train_rf)

colnames(misclassification_train) = Model

misclassification_train = misclassification_train %>% 
  pivot_longer(1:5, 
               names_to = "Model", 
               values_to = "number")

misclassification = inner_join(misclassification_train, misclassification_test) 

colnames(misclassification) = c("Model", "Train Error", "Test Error")

misclassification %>% 
  kable(format = "latex",
        booktabs = TRUE, 
        digits = 4) %>%
  save_kable("misclassification-error.pdf")
 
