# load libraries
library(glmnetUtils)                    
source("code/functions/plot_glmnet.R")  
library(tidyverse)                      

# read in the training data
mental_health_train = read_csv("data/clean/mental_health_train.csv")

# read in the test
mental_health_test = read_csv("data/clean/mental_health_test.csv")

# run elastic net regression
set.seed(1)
elnet_fit = cva.glmnet(mentally_unhealthy_days ~. -physically_unhealthy_days, 
                       nfolds = 10, 
                       data = mental_health_train) 

elnet_fit$alpha
plot_cva_glmnet(elnet_fit)

# optmal alpha 
elnet_fit_best = extract_best_elnet(elnet_fit)
elnet_fit_best$alpha
