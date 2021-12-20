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
ridge_fit = cv.glmnet(mentally_unhealthy_days ~ .- physically_unhealthy_days,
                      alpha = 0, 
                      nfolds = 10, 
                      data = mental_health_train)

plot(ridge_fit)

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

# save lasso trace plot
p_ridge = plot_glmnet(ridge_fit, mental_health_train, 
                      features_to_plot = 10, lambda = ridge_fit$lambda.1se)

ggsave(filename = "results/ridge-trace-plot.png", 
       plot = p_ridge, 
       device = "png", 
       width = 10, 
       height = 5)

# extract features selected by lasso and their coefficients
beta_hat_std = extract_std_coefs(ridge_fit, mental_health_train, 
                                 lambda = ridge_fit$lambda.1se)

# standard error - percent smokers 
sd(mental_health_train$perc_smokers)

# save top 10 feature coefficients
beta_hat_std %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(coefficient))) %>%
  head(10) %>%
  kable(format = "latex",
        booktabs = TRUE, 
        col.names = c("Feature", "Coefficient")) %>%
  save_kable("ridge-coefficients.pdf")

# optimal lambda value (0.0756)
ridge_fit$lambda.1se 
