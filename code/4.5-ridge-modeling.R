# load libraries
library(glmnetUtils)                    
source("code/functions/plot_glmnet.R")  
library(tidyverse)                      

# read in the training data
mental_health_train = read_csv("data/clean/mental_health_train.csv")

# read in the test
mental_health_test = read_csv("data/clean/mental_health_test.csv")

# run ridge regression
set.seed(1)

ridge_fit = cv.glmnet(mentally_unhealthy ~ . - mentally_unhealthy_days 
                                             - physically_unhealthy_days,
                      alpha = 0, 
                      nfolds = 10, 
                      family = "binomial", 
                      type.measure = "class",  
                      data = mental_health_train)

plot(ridge_fit)
plot_glmnet(ridge_fit, mental_health_train, features_to_plot = 10, lambda = ridge_fit$lambda.1se)

coef(ridge_fit, s = "lambda.1se") 

# save the ridge fit object
save(ridge_fit, file = "results/ridge_fit.Rda")

# save ridge CV plot
png(width = 7, 
    height = 5,
    res = 300,
    units = "in", 
    filename = "results/ridge-cv-plot.png")
plot(ridge_fit)
dev.off()

# create lasso trace plot
p_ridge = plot_glmnet(ridge_fit, mental_health_train, features_to_plot = 10, lambda = ridge_fit$lambda.1se)
ggsave(filename = "results/ridge-trace-plot.png", 
       plot = p_ridge, 
       device = "png", 
       width = 8, 
       height = 5)

# extract features selected by lasso and their coefficients
beta_hat_std = extract_std_coefs(ridge_fit, mental_health_train, 
                                 lambda = ridge_fit$lambda.1se)

# reminder when saving kable: set working dictionary to "results"
beta_hat_std %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(coefficient))) %>%
  head(10) %>%
  kable(format = "latex",
        booktabs = TRUE, 
        col.names = c("Feature", "Coefficient")) %>%
  save_kable("ridge-coefficients.pdf")

# optimal lambda value 
lambda = ridge_fit$lambda.1se

# visualize the fitted coefficients as a function of lambda
probabilities_test = predict(ridge_fit,              
                        newdata = mental_health_test,  
                        s = "lambda.1se",       
                        type = "response") %>%   
  as.numeric()    

probabilities_train = predict(ridge_fit,              
                             newdata = mental_health_train,  
                             s = "lambda.1se",       
                             type = "response") %>%   
  as.numeric() 

# make predictions 
predictions_test = as.numeric(probabilities_test > 0.5) # class imbalance??

predicitions_train = as.numeric(probabilities_train > 0.5) # class imbalance??

# evaluating the classifier 
mental_health_test = mental_health_test %>% 
  mutate(predicted_mental_health = predictions_test)

mental_health_train = mental_health_train %>% 
  mutate(predicted_mental_health = predictions_train)

# calculate misclassification rate
misclassification_test_ridge = mental_health_test %>% 
  summarise(mean(mentally_unhealthy != predicted_mental_health))

misclassification_train_ridge = mental_health_train %>% 
  summarise(mean(mentally_unhealthy != predicted_mental_health))

write_csv(misclassification_test_ridge, 
          file = "results/misclassification_test_ridge.csv")

write_csv(misclassification_train_ridge, 
          file = "results/misclassification_train_ridge.csv")
