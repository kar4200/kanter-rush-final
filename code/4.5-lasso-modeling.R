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

lasso_fit$nzero[lasso_fit$lambda == lasso_fit$lambda.1se] # 46 features selected

# save the lasso fit object
save(lasso_fit, file = "results/lasso_fit.Rda")

# save lasso CV plot
png(width = 7, 
    height = 5,
    res = 300,
    units = "in", 
    filename = "results/lasso-cv-plot.png")
plot(lasso_fit)
dev.off()

# save lasso trace plot
p_lasso = plot_glmnet(lasso_fit, mental_health_train, features_to_plot = 8)
ggsave(filename = "results/lasso-trace-plot.png", 
       plot = p_lasso, 
       device = "png", 
       width = 10, 
       height = 5)

# extract features selected by lasso and their coefficients
beta_hat_std = extract_std_coefs(lasso_fit, mental_health_train)

# save top 10 feature coefficients
beta_hat_std %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(coefficient))) %>%
  head(10) %>%
  kable(format = "latex",
        booktabs = TRUE, 
        col.names = c("Feature", "Coefficient")) %>%
  save_kable("lasso-coefficients.pdf")

