# load libraries
library(glmnetUtils)                    
source("code/functions/plot_glmnet.R")  
library(tidyverse)                      

# read in the training data
mental_health_train = read_csv("data/clean/mental_health_train.csv")

# read in the test
mental_health_test = read_csv("data/clean/mental_health_test.csv")

# run lasso regression
set.seed(1)
lasso_fit = cv.glmnet(mentally_unhealthy ~ . -mentally_unhealthy_days 
                                             -physically_unhealthy_days,   
                      alpha = 1,                 
                      nfolds = 10,  
                      family = "binomial",
                      type.measure = "class",
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

# interpret: negative coefficient (fewer mentally unhappy days), positive coefficient (more mentally unhappy days)

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
probabilities = predict(lasso_fit,              
                        newdata = mental_health_test,  
                        s = "lambda.1se",       
                        type = "response") %>%   
  as.numeric()                                   

# make predictions 
predictions = as.numeric(probabilities > 0.5)
head(predictions)

# evaluating the classifier 
mental_health_test = mental_health_test %>% 
  mutate(predicted_mental_health = predictions)

# calculate misclassification rate
misclassification_lasso = mental_health_test %>% 
  summarise(mean(mentally_unhealthy != predicted_mental_health))

write_csv(misclassification_lasso, file = "results/misclassification_lasso.csv")

