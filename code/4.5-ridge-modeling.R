# load libraries
library(glmnetUtils)                    # to run ridge and lasso
source("code/functions/plot_glmnet.R")  # for lasso/ridge trace plots
library(tidyverse)                      # for everything else

# read in the training data
mental_health_train = read_csv("data/clean/mental_health_train.csv")

# run ridge regression
set.seed(1)

ridge_fit = cv.glmnet(mentally_unhealthy ~ . - mentally_unhealthy_days - physically_unhealthy_days,
alpha = 0, nfolds = 10, family = "binomial", type.measure = "class",  data = mental_health_train)

coef(ridge_fit, s = "lambda.1se") %>% head()

# extract features selected by lasso and their coefficients
beta_hat_std = extract_std_coefs(ridge_fit, mental_health_train)

# reminder when saving kable: set working dictionary to "results"
beta_hat_std %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(coefficient))) %>%
  head(10) %>%
  kable(format = "latex",
        booktabs = TRUE, 
        col.names = c("Feature", "Coefficient"),
        caption = "Top 10 features selected by lasso and their coefficients") %>%
  save_kable("ridge-coefficients.pdf")

lambda = ridge_fit$lambda.1se
sprintf("The value of lambda based on the one-standard-error rule: %f",
        lambda)

# visualize the fitted coefficients as a function of lambda
probabilities = predict(ridge_fit,               # fit object
                        newdata = mental_health_test,  # new data to test on
                        s = "lambda.1se",        # which value of lambda to use
                        type = "response") %>%   # to output probabilities
  as.numeric()                                   # convert to vector
head(probabilities)


# make predictions 
predictions = as.numeric(probabilities > 0.5)
head(predictions)

# evaluating the classifier 
mental_health_test = mental_health_test %>% 
  mutate(predicted_mental_health = predictions)

# then calculate misclassification rate
mental_health_test %>% 
  summarise(mean(mentally_unhealthy != predicted_mental_health))



