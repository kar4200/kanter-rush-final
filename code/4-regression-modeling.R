# load libraries
library(tidyverse)
library(glmnetUtils)                    # to run ridge and lasso
library(pROC)   
source("code/functions/plot_glmnet.R")            # for lasso/ridge trace plots

# read in the training data
mental_health_train = read_csv("data/clean/mental_health_train.csv")

# running a logistic regression
glm_fit = glm(mentally_unhealthy ~ . -fips -state -name -mentally_unhealthy_days, 
              family = "binomial",
              data = mental_health_train)

summary(glm_fit)

mental_health_test2 = mental_health_test %>%  
  select(-housing_two_unit_structures)
## extracting elements of the fit
coef(glm_fit)

# extracting the fitted probabilities 
fitted_probabilities = predict(glm_fit, 
                               newdata = mental_health_test2,
                               type = "response")   

head(fitted_probabilities)

# run ridge regression
set.seed(1)
ridge_fit = cv.glmnet(case_fatality_rate ~ . - state - county - fips,   
                      alpha = 0,                 
                      nfolds = 10,               
                      data = covid_train)

# save the ridge fit object
save(ridge_fit, file = "results/ridge_fit.Rda")

# run lasso regression
set.seed(1)
lasso_fit = cv.glmnet(case_fatality_rate ~ . - state - county - fips,   
                      alpha = 1,                 
                      nfolds = 10,               
                      data = covid_train)

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
beta_hat_std = extract_std_coefs(lasso_fit, covid_train)
beta_hat_std %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(coefficient))) %>% 
  write_tsv("results/lasso-features-table.tsv")
