# load libraries
library(glmnetUtils)
library(tidyverse)

# read in the training data
mental_health_train = read_csv("data/clean/mental_health_train.csv")

# read in the testing data
mental_health_test = read_csv("data/clean/mental_health_test.csv")

# OLS regression
load("results/lm_fit.Rda")

# load ridge fit object
load("results/ridge_fit.Rda")

# load lasso fit object
load("results/lasso_fit.Rda")

# load decision tree (optimal) object
load("results/optimal_tree.Rda")

# load random forest object
load("results/rf_fit_tuned.Rda")

# load boosting object 
load("results/gbm_fit_tuned.Rda")

# evaluate OLS mse
# test
predictions_test_lm = predict(lm_fit, newdata = mental_health_test) 
mse_test_lm = mean((predictions_test_lm - mental_health_test$mentally_unhealthy_days)^2)

# train
predictions_train_lm = predict(lm_fit, newdata = mental_health_train) 
mse_train_lm = mean((predictions_train_lm - mental_health_train$mentally_unhealthy_days)^2)

# evaluate ridge mse
# test
predictions_test_ridge = predict(ridge_fit, newdata = mental_health_test,  
                                 s = "lambda.1se") %>% as.numeric() 
mse_test_ridge = mean((predictions_test_ridge - mental_health_test$mentally_unhealthy_days)^2)

# train
predictions_train_ridge = predict(ridge_fit, newdata = mental_health_train,  
                                 s = "lambda.1se") %>% as.numeric() 
mse_train_ridge = mean((predictions_train_ridge - mental_health_train$mentally_unhealthy_days)^2)

# evaluate lasso mse
# test
predictions_test_lasso = predict(lasso_fit, newdata = mental_health_test,  
                                 s = "lambda.1se") %>% as.numeric() 
mse_test_lasso = mean((predictions_test_lasso - mental_health_test$mentally_unhealthy_days)^2)

# train
predictions_train_lasso = predict(lasso_fit, newdata = mental_health_train,  
                                 s = "lambda.1se") %>% as.numeric() 
mse_train_lasso = mean((predictions_train_lasso - mental_health_train$mentally_unhealthy_days)^2)

# evaluate decision tree mse
# test
pred_decision_test = predict(optimal_tree, 
                             newdata = mental_health_test)
mse_decision_test = mean((pred_decision_test - mental_health_test$mentally_unhealthy_days)^2)

# train
pred_decision_train = predict(optimal_tree, 
                             newdata = mental_health_train)
mse_decision_train = mean((pred_decision_train - mental_health_train$mentally_unhealthy_days)^2)

# evaluate random forest mse
# test
pred_rf_test = predict(rf_fit_tuned, 
                       newdata = mental_health_test)
mse_rf_test = mean((pred_rf_test - mental_health_test$mentally_unhealthy_days)^2)

# train
pred_rf_train = predict(rf_fit_tuned, 
                       newdata = mental_health_train)
mse_rf_train = mean((pred_rf_train - mental_health_train$mentally_unhealthy_days)^2)

# evaluate boosting mse
# test
predictions_test_gbm = predict(gbm_fit_tuned, 
                               n.trees = optimal_num_trees,
                               newdata = mental_health_test)
mse_gbm_test = mean((predictions_test_gbm - 
                       mental_health_test$mentally_unhealthy_days)^2)

# train
predictions_train_gbm = predict(gbm_fit_tuned, 
                               n.trees = optimal_num_trees,
                               newdata = mental_health_train)
mse_gbm_train = mean((predictions_train_gbm - 
                       mental_health_train$mentally_unhealthy_days)^2)

# print nice table
tibble(Method = c("OLS", "Ridge", "Lasso", "Decision Tree", "Random Forest", "Boosting"), 
       `Train mse` = c(mse_train_lm, mse_train_ridge, mse_train_lasso, mse_decision_train ,mse_rf_train, mse_gbm_train),
       `Test mse` = c(mse_test_lm, mse_test_ridge, mse_test_lasso, mse_decision_test ,mse_rf_test, mse_gbm_test)) %>%
  kable(format = "latex",
        booktabs = TRUE, 
        digits = 4,
        col.names = c("Model", "Train MSE", "Test MSE")) %>%
  save_kable("mse-all-models.pdf")

# %>%
  write_tsv("results/model-evaluation.tsv")