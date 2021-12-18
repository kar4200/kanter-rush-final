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

# fit decision tree
set.seed(1)
mental_health_fit = rpart(mentally_unhealthy_days ~ . -physically_unhealthy_days,
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
set.seed(1)
mental_health_fit_deep = rpart(mentally_unhealthy_days ~ . -physically_unhealthy_days,
                   control = rpart.control(minsplit = 2, 
                                           minbucket = 1,
                                           cp = 0),
                   data = mental_health_train)

printcp(mental_health_fit_deep)

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
set.seed(1)
optimal_tree_info = cp_table %>% 
  filter(xerror - xstd < min(xerror)) %>% 
  arrange(nsplit) %>% 
  head(1)

optimal_tree_info$nsplit # 17 splits in the optimal tree - has node with 0 (weird!)

# prune the optimal tree
optimal_tree_info
optimal_tree = prune(mental_health_fit_deep, cp = optimal_tree_info$CP)
rpart.plot(optimal_tree)

save(optimal_tree, file = "results/optimal_tree.Rda")

png(width = 11, 
    height = 8,
    res = 300,
    units = "in", 
    filename = "results/decision-tree.png")
rpart.plot(optimal_tree)
dev.off()

# mean-square-error 
pred_decision_test = predict(optimal_tree, 
                        newdata = mental_health_test)

pred_decision_train = predict(optimal_tree,
                              newdata = mental_health_train)

mse_decision_test = mean((pred_decision_test - mental_health_test$mentally_unhealthy_days)^2) %>%
  as_tibble()

mse_decision_train = mean((pred_decision_train - mental_health_train$mentally_unhealthy_days)^2) %>%
  as_tibble()

write_csv(mse_decision_test, 
          file = "results/mse_decision_test.csv")

write_csv(mse_decision_train, 
          file = "results/mse_decision_train.csv")

# Random Forests
set.seed(1)
rf_fit = randomForest(mentally_unhealthy_days ~ . -physically_unhealthy_days,
                      data = mental_health_train)
rf_fit$mtry

# tune random forests
set.seed(1)
mvalues = seq.int(1, 60, by = 5)
oob_errors = numeric(length(mvalues))
ntree = 100
for(idx in 1:length(mvalues)){
  m = mvalues[idx]
  rf_fit_tune = randomForest(mentally_unhealthy_days ~ . -physically_unhealthy_days, 
                             mtry = m, data = mental_health_train)
  oob_errors[idx] = rf_fit_tune$mse[ntree]
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
set.seed(1)
rf_fit_tuned = randomForest(mentally_unhealthy_days ~ . -physically_unhealthy_days, 
                            mtry = 31, 
                            ntree = 500, 
                            importance = TRUE,
                            data = mental_health_train)

save(rf_fit_tuned, file = "results/rf_fit_tuned.Rda")

png(width = 7, 
    height = 7,
    res = 300,
    units = "in", 
    filename = "results/rf-fit-tuned.png")
plot(rf_fit_tuned)
dev.off()

# variable importance 
png("varimp.png", width=8, height=8, res=300, units = "in")
varImpPlot(rf_fit_tuned, n.var = 10, cex = 0.8) 
dev.off()

# mean-squared error
pred_rf_test = predict(rf_fit_tune, 
                             newdata = mental_health_test)

pred_rf_train = predict(rf_fit_tune,
                              newdata = mental_health_train)

mse_rf_test = mean((pred_rf_test - mental_health_test$mentally_unhealthy_days)^2) %>%
  as_tibble()

mse_rf_train = mean((pred_rf_train - mental_health_train$mentally_unhealthy_days)^2) %>%
  as_tibble()

write_csv(mse_rf_test, 
          file = "results/mse_rf_test.csv")

write_csv(mse_rf_train, 
          file = "results/mse_rf_train.csv")

