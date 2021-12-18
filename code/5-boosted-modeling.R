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
gbm_fit_1 = gbm(mentally_unhealthy_days ~ . -physically_unhealthy_days,
              distribution = "gaussian",
              n.trees = 1000,
              interaction.depth = 1,
              shrinkage = 0.1,
              cv.folds = 5,
              data = mental_health_train)

set.seed(1)
gbm_fit_2 = gbm(mentally_unhealthy_days ~ . -physically_unhealthy_days,
              distribution = "gaussian",
              n.trees = 1000,
              interaction.depth = 2,
              shrinkage = 0.1,
              cv.folds = 5,
              data = mental_health_train)

set.seed(1)
gbm_fit_3 = gbm(mentally_unhealthy_days ~ . -physically_unhealthy_days,
              distribution = "gaussian",
              n.trees = 1000,
              interaction.depth = 3,
              shrinkage = 0.1,
              cv.folds = 5,
              data = mental_health_train)

ntrees = 1000
cv_errors = bind_rows(
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_1$cv.error, depth = 1), 
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_2$cv.error, depth = 2), 
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_3$cv.error, depth = 3),
)

cv_errors %>%
  ggplot(aes(x = ntree, y = cv_err, colour = factor(depth))) + 
  # add horizontal dashed lines at the minima of the three curves
  geom_hline(yintercept = min(gbm_fit_1$cv.error), 
             linetype = "dashed", color = "red") +
  geom_hline(yintercept = min(gbm_fit_2$cv.error), 
             linetype = "dashed", color = "green") +
  geom_hline(yintercept = min(gbm_fit_3$cv.error), 
             linetype = "dashed", color = "blue") +
  geom_line() + 
  # set colors to match horizontal line minima
  scale_color_manual(labels = c("1", "2", "3", "4"), 
                     values = c("red", "green", "blue", "purple")) +
  labs(x = "Number of trees", y = "CV error", colour = "Interaction depth") +
  theme_bw()
