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

# fit classification tree based on Gini Index with default "control" parameters
set.seed(1)
mental_health_fit = rpart(mentally_unhealthy ~ . - mentally_unhealthy_days -physically_unhealthy_days,
                 method = "class",
                 parms = list(split = "gini"),
                 data = mental_health_train)

rpart.plot(mental_health_fit)

# find deepest possible tree (to begin to find optimal tree)
set.seed(1)
mental_health_fit_deep = rpart(mentally_unhealthy ~ . - mentally_unhealthy_days -physically_unhealthy_days,
                   method = "class",
                   control = rpart.control(minsplit = 2, 
                                           minbucket = 1,
                                           cp = 0),
                   parms = list(split = "gini"),
                   data = mental_health_train)

cp_table = printcp(mental_health_fit_deep) %>%
  as_tibble()

cp_table %>% 
  filter(nsplit >= 2) %>%
  ggplot(aes(x = nsplit+1, y = xerror, 
             ymin = xerror - xstd, ymax = xerror + xstd)) + 
  geom_point() + 
  geom_line() +
  scale_x_log10() +
  geom_errorbar(width = 0.1) +
  xlab("Number of terminal nodes") + ylab("CV error") + 
  geom_hline(aes(yintercept = min(xerror)), linetype = "dashed") + 
  theme_bw()

# find optimal tree
set.seed(1)
optimal_tree_info = cp_table %>% 
  filter(xerror - xstd < min(xerror)) %>% 
  arrange(nsplit) %>% 
  head(1)

optimal_tree_info$nsplit # 9 splits in the optimal tree

# prune the optimal tree
optimal_tree = prune(mental_health_fit_deep, cp = optimal_tree_info$CP)
rpart.plot(optimal_tree)

# misclassification
pred_decision = predict(optimal_tree, newdata = mental_health_test, type = "class")
misclassification_decision <- mean(pred_decision != mental_health_test$mentally_unhealthy) # 14.98

