# load libraries
library(glmnetUtils)                    # to run ridge and lasso
source("code/functions/plot_glmnet.R") # for lasso/ridge trace plots
library(tidyverse)                      # for everything else

# read in the training data
mental_health_train = read_csv("data/clean/mental_health_train.csv")

# run lasso regression
set.seed(1)
lasso_fit = cv.glmnet(mentally_unhealthy ~ . - mentally_unhealthy_days -physically_unhealthy_days,   
                      alpha = 1,                 
                      nfolds = 10,               
                      data = mental_health_train)

plot(lasso_fit)
plot_glmnet(lasso_fit, mental_health_train, features_to_plot = 6)

# NEED TO SAVE
# save the lasso fit object
save(lasso_fit, file = "results/lasso_fit.Rda")

# create lasso CV plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/lasso-cv-plot.png")
plot(lasso_fit)
dev.off()

# create lasso trace plot
p = plot_glmnet(lasso_fit, covid_train, features_to_plot = 6)
ggsave(filename = "results/lasso-trace-plot.png", 
       plot = p, 
       device = "png", 
       width = 6, 
       height = 4)

# extract features selected by lasso and their coefficients
beta_hat_std = extract_std_coefs(lasso_fit, mental_health_train)
beta_hat_std %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(coefficient))) 

# NEED TO LOOK AT PHYSICALLY UNHEALTHY DAYS BECAUSE STRONG CORRELATION 

