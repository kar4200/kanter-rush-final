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

optimal_tree_info$nsplit # 10 splits in the optimal tree

# prune the optimal tree
optimal_tree = prune(mental_health_fit_deep, cp = optimal_tree_info$CP)
rpart.plot(optimal_tree)

# misclassification
pred_decision = predict(optimal_tree, newdata = mental_health_test, type = "class")
misclassification_decision <- mean(pred_decision != mental_health_test$mentally_unhealthy) # 9.07

# RANDOM FORESTS (need to tune)
set.seed(1)
rf_fit = randomForest(factor(mentally_unhealthy) ~ . -mentally_unhealthy_days -physically_unhealthy_days,
                      data = mental_health_train)
rf_fit$mtry # value equal to 7 - should go through to see if more is needed

# tune random forests
set.seed(1)
mvalues = seq.int(1, 60, by = 5)
oob_errors = numeric(length(mvalues))
ntree = 100
for(idx in 1:length(mvalues)){
  m = mvalues[idx]
  rf_fit_test = randomForest(factor(mentally_unhealthy) ~ . -mentally_unhealthy_days -physically_unhealthy_days, 
                             mtry = m, data = mental_health_train)
  oob_errors[idx] = rf_fit_test$err.rate[,"OOB"][ntree]
}

tibble(m = mvalues, oob_err = oob_errors) %>%
  ggplot(aes(x = m, y = oob_err)) + 
  geom_line() + geom_point() + 
  scale_x_continuous(breaks = mvalues) +
  theme_bw()

# tune random forest
set.seed(1)
rf_fit_tuned = randomForest(factor(mentally_unhealthy) ~ . -mentally_unhealthy_days -physically_unhealthy_days, 
                            mtry = 21, 
                            ntree = 500, 
                            importance = TRUE,
                            data = mental_health_train)

# variable importance 
varImpPlot(rf_fit_tuned, n.var = 10, cex = 0.8)

# misclassification error
pred_rf = predict(rf_fit_tuned, newdata = mental_health_test, type = "class")
misclassification_rf <- mean(pred_rf != mental_health_test$mentally_unhealthy) # 7.38

