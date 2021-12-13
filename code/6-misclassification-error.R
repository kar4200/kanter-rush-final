# reading in misclassification errors 
misclassification_regression = read_csv("results/misclassification_regression.csv")
misclassification_ridge = read_csv("results/misclassification_ridge.csv")
misclassification_lasso = read_csv("results/misclassification_lasso.csv")
misclassification_decision = read_csv("results/misclassification_decision.csv")
misclassification_rf = read_csv("results/misclassification_rf.csv")

# renaming column names 
 misclassification_regression = misclassification_regression %>% 
  rename(value = `mean(mentally_unhealthy != predicted_mental_health)`)
 misclassification_ridge = misclassification_ridge %>% 
   rename(value = `mean(mentally_unhealthy != predicted_mental_health)`)
 misclassification_lasso = misclassification_lasso %>% 
   rename(value = `mean(mentally_unhealthy != predicted_mental_health)`)

# creating table of misclassification errors 
Model = c("Regression", "Ridge", "Lasso", "Decision", "Random Forest")

misclassification =  
   as_tibble(cbind(Model, rbind(misclassification_regression$value, 
      misclassification_ridge$value, 
       misclassification_lasso$value, 
       misclassification_decision$value, 
       misclassification_rf$value))) %>% 
  rename(`Misclassification Error` = V2) %>%
  kable(format = "latex",
        booktabs = TRUE, 
        digits = c(1, 4),
        col.names = c("Feature", "Coefficient"),
        caption = "Misclassification errors for each model.") %>%
  save_kable("misclassification-error.pdf")
 


