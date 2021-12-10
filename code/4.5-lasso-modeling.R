# load libraries
library(glmnetUtils)                    # to run ridge and lasso
source("code/functions/plot_glmnet.R")  # for lasso/ridge trace plots
library(tidyverse)                      # for everything else

# read in the training data
mental_health_train = read_csv("data/clean/mental_health_train.csv")

# run lasso regression
set.seed(1)
lasso_fit = cv.glmnet(mentally_unhealthy ~ . - mentally_unhealthy_days - physically_unhealthy_days,   
                      alpha = 1,                 
                      nfolds = 10,               
                      data = mental_health_train)

# removing physically unhealthy days because it is strongly correlated with mentally unhealthy days 

plot(lasso_fit)
plot_glmnet(lasso_fit, mental_health_train, features_to_plot = 10)

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
        col.names = c("Feature", "Coefficient"),
        caption = "Top 10 features selected by lasso and their coefficients") %>%
  save_kable("lasso-coefficients.pdf")

