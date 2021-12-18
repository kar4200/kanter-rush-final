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
set.seed(400)
mental_health_fit = rpart(mentally_unhealthy ~ . -mentally_unhealthy_days 
                                                 -physically_unhealthy_days,
                 method = "class",
                 parms = list(split = "gini"),
                 data = mental_health_train)

rpart.plot(mental_health_fit)

png(width = 8, 
    height = 8,
    res = 300,
    units = "in", 
    filename = "results/default-tree.png")
rpart.plot(mental_health_fit)
dev.off()

# find deepest possible tree (to begin to find optimal tree)
set.seed(400)
mental_health_fit_deep = rpart(mentally_unhealthy ~ . -mentally_unhealthy_days 
                                                      -physically_unhealthy_days,
                   method = "class",
                   control = rpart.control(minsplit = 2, 
                                           minbucket = 1,
                                           cp = 0),
                   parms = list(split = "gini"),
                   data = mental_health_train)

cp_table = printcp(mental_health_fit_deep) %>%
  as_tibble()

cp = cp_table %>% 
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

ggsave(filename = "results/cp-cv-chart.png", 
       plot = cp, 
       device = "png", 
       width = 6, 
       height = 6)

# find optimal tree
set.seed(400)
optimal_tree_info = cp_table %>% 
  filter(xerror - xstd < min(xerror)) %>% 
  arrange(nsplit) %>% 
  head(1)

optimal_tree_info$nsplit # 8 splits in the optimal tree

# prune the optimal tree
optimal_tree = prune(mental_health_fit_deep, cp = optimal_tree_info$CP)
rpart.plot(optimal_tree)

save(optimal_tree, file = "results/optimal_tree.Rda")

png(width = 8, 
    height = 8,
    res = 300,
    units = "in", 
    filename = "results/classification-tree.png")
rpart.plot(optimal_tree)
dev.off()

# misclassification test 
pred_decision_test = predict(optimal_tree, 
                        newdata = mental_health_test, type = "class")

misclassification_test_decision = as_tibble(mean(pred_decision_test != mental_health_test$mentally_unhealthy)) 

write_csv(misclassification_test_decision, 
          file = "results/misclassification_test_decision.csv")

# misclassification train 
pred_decision_train = predict(optimal_tree, 
                        newdata = mental_health_train, type = "class")

misclassification_train_decision = as_tibble(mean(pred_decision_train != mental_health_train$mentally_unhealthy)) 

write_csv(misclassification_train_decision, 
          file = "results/misclassification_train_decision.csv")

# RANDOM FORESTS
set.seed(10)
rf_fit = randomForest(factor(mentally_unhealthy) ~ . -mentally_unhealthy_days -physically_unhealthy_days,
                      data = mental_health_train)
rf_fit$mtry

# tune random forests
set.seed(10)
mvalues = seq.int(1, 60, by = 5)
oob_errors = numeric(length(mvalues))
ntree = 100
for(idx in 1:length(mvalues)){
  m = mvalues[idx]
  rf_fit_test = randomForest(factor(mentally_unhealthy) ~ . -mentally_unhealthy_days -physically_unhealthy_days, 
                             mtry = m, data = mental_health_train)
  oob_errors[idx] = rf_fit_test$err.rate[,"OOB"][ntree]
}

rf_cv = tibble(m = mvalues, oob_err = oob_errors) %>%
  ggplot(aes(x = m, y = oob_err)) + 
  geom_line() + geom_point() + 
  scale_x_continuous(breaks = mvalues) +
  theme_bw()

png(width = 7, 
    height = 7,
    res = 300,
    units = "in", 
    filename = "results/rf-cv-plot.png")
plot(rf_cv)
dev.off()

# tune random forest
set.seed(10)
rf_fit_tuned = randomForest(factor(mentally_unhealthy) ~ . -mentally_unhealthy_days -physically_unhealthy_days, 
                            mtry = 46, 
                            ntree = 500, 
                            importance = TRUE,
                            data = mental_health_train)

save(rf_fit_tuned, file = "results/rf_fit_tuned.Rda")

# variable importance 
var_imp = varImpPlot(rf_fit_tuned, n.var = 10, cex = 0.5)
# find how to save this

# misclassification test
pred_test_rf = predict(rf_fit_tuned, newdata = mental_health_test, type = "class")

misclassification_test_rf = as_tibble(mean(pred_test_rf != mental_health_test$mentally_unhealthy))

write_csv(misclassification_test_rf, 
          file = "results/misclassification_test_rf.csv")

# misclassification train
pred_train_rf = predict(rf_fit_tuned, newdata = mental_health_train, type = "class")


misclassification_train_rf = as_tibble(mean(pred_train_rf != mental_health_train$mentally_unhealthy)) 

write_csv(misclassification_train_rf, 
          file = "results/misclassification_train_rf.csv")

