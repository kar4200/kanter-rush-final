# load libraries
library(glmnetUtils)                    
source("code/functions/plot_glmnet.R")  
library(tidyverse)      
library(kableExtra)

# read in the training data
mental_health_train = read_csv("data/clean/mental_health_train.csv")

# read in the test
mental_health_test = read_csv("data/clean/mental_health_test.csv")

# run lasso regression
set.seed(1)
lasso_fit = cv.glmnet(mentally_unhealthy_days ~. -physically_unhealthy_days,   
                      alpha = 1,                 
                      nfolds = 10,
                      data = mental_health_train)


plot(lasso_fit)
plot_glmnet(lasso_fit, mental_health_train, features_to_plot = 10, lambda = lasso_fit$lambda.1se)

lasso_fit$nzero[lasso_fit$lambda == lasso_fit$lambda.1se]

# save the lasso fit object
save(lasso_fit, file = "results/lasso_fit.Rda")

# create lasso CV plot
png(width = 7, 
    height = 5,
    res = 300,
    units = "in", 
    filename = "results/lasso-cv-plot.png")
plot(lasso_fit)
dev.off()

# create lasso trace plot
p_lasso = plot_glmnet(lasso_fit, mental_health_train, features_to_plot = 8)
ggsave(filename = "results/lasso-trace-plot.png", 
       plot = p_lasso, 
       device = "png", 
       width = 8, 
       height = 5)

# extract features selected by lasso and their coefficients
beta_hat_std = extract_std_coefs(lasso_fit, mental_health_train)

# reminder when saving kable: set working dictionary to "results"
beta_hat_std %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(coefficient))) %>%
  head(10) %>%
  kable(format = "latex",
        booktabs = TRUE, 
        col.names = c("Feature", "Coefficient")) %>%
  save_kable("lasso-coefficients.pdf")


# visualize the fitted coefficients as a function of lambda
predictions_test_lasso = predict(lasso_fit,              
                        newdata = mental_health_test,  
                        s = "lambda.1se") %>%   
  as.numeric() 

predictions_train_lasso = predict(lasso_fit,              
                             newdata = mental_health_train,  
                             s = "lambda.1se") %>%   
  as.numeric()

# rmse
mse_test_lasso = mean((predictions_test_lasso - mental_health_test$mentally_unhealthy_days)^2) %>%
  data.frame()

mse_train_lasso = mean((predictions_train_lasso - mental_health_train$mentally_unhealthy_days)^2) %>%
  data.frame()

write_csv(mse_test_lasso, 
          file = "results/mse_test_lasso.csv")

write_csv(mse_train_lasso, 
          file = "results/mse_train_lasso.csv")
