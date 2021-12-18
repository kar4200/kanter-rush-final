# load libraries
library(rpart)         # to train decision trees
library(rpart.plot)    # to plot decision trees
library(randomForest)  # random forests
library(gbm)           # boosting
library(tidyverse)     # tidyverse
library(kableExtra)    # kable

# read in the training data
mental_health_train = read_csv("data/clean/mental_health_train.csv")

# read in the testing data
mental_health_test = read_csv("data/clean/mental_health_test.csv")

# fit boosted model
set.seed(1)
gbm_fit = gbm(mentally_unhealthy_days ~ . -physically_unhealthy_days,
              distribution = "gaussian",
              n.trees = 100,
              interaction.depth = 1,
              shrinkage = 0.1,
              cv.folds = 5,
              data = mental_health_train)
