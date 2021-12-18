# loading packages 
library(tidyverse)

# reading in mean-square-errors
# OLS
mse_test_lm = 
  read_csv("results/mse_test_lm.csv")
mse_train_lm = 
  read_csv("results/mse_train.lm.csv")

# Ridge
mse_test_ridge = 
  read_csv("results/mse_test_ridge.csv")
mse_train_ridge = 
  read_csv("results/mse_train_ridge.csv")

# Lasso
mse_test_lasso = 
  read_csv("results/mse_test_lasso.csv")
mse_train_lasso = 
  read_csv("results/mse_train_lasso.csv")

# Decision
mse_test_decision = 
  read_csv("results/mse_decision_test.csv")
mse_train_decision = 
  read_csv("results/mse_decision_train.csv")

# Random Forest
mse_test_rf = 
  read_csv("results/mse_rf_test.csv")
mse_train_rf = 
  read_csv("results/mse_rf_train.csv")

# creating table of misclassification errors 
Model = c("Regression", "Ridge", "Lasso", "Decision", "Random Forest")

mse_test =  
   as_tibble(cbind(Model, rbind(mse_test_lm, 
                                mse_test_lasso, 
                                mse_test_lasso, 
                                mse_test_decision, 
                                mse_test_rf)))

 
mse_train = cbind(mse_train_lm, 
                  mse_train_lasso, 
                  mse_train_lasso, 
                  mse_train_decision, 
                  mse_train_rf)

colnames(mse_train) = Model

mse_train = mse_train %>% 
  pivot_longer(1:5, 
               names_to = "Model", 
               values_to = "number")

mse = inner_join(mse_test, mse_train) 

colnames(mse) = c("Model", "Test Error", "Train Error")

mse %>% 
  kable(format = "latex",
        booktabs = TRUE, 
        digits = 4) %>%
  save_kable("mse-all-models.pdf")
 
