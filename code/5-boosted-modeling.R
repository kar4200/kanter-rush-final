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

cv_boosting = cv_errors %>%
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

ggsave(filename = "results/cv_boosting.png", 
       plot = cv_boosting, 
       device = "png", 
       width = 6, 
       height = 4)

# Extracting Optimal Tree
gbm_fit_tuned = gbm_fit_3
save(gbm_fit_tuned, file = "results/gbm_fit_tuned.Rda")

optimal_num_trees = gbm.perf(gbm_fit_3, plot.it = FALSE)
optimal_num_trees

# Relative Influence 
summary(gbm_fit_tuned, n.trees = optimal_num_trees, plotit = FALSE) %>%
  remove_rownames() %>%
  top_n(10) %>% 
  kable(format = "latex", row.names = NA, 
        booktabs = TRUE,
        digits = 2,
        col.names = c("Variable", "Relative influence")) %>%
    save_kable("boosting-rel-inf.pdf")
        
# top three feaatures by relative influence
png("partial1.png")
plot(gbm_fit_tuned, i.var = "perc_smokers", n.trees = optimal_num_trees,
     xlab = "Percent Smokers",
     main = "Partial Dependence Plot: Smoking")
dev.off()

png("partial2.png")
plot(gbm_fit_tuned, i.var = "household_income", n.trees = optimal_num_trees,
     xlab = "Household Income",
     main = "Partial Dependence Plot: Household Income")
dev.off()

png("partial3.png")
plot(gbm_fit_tuned, i.var = "perc_insufficient_sleep", n.trees = optimal_num_trees,
     xlab = "Percent Insufficient Sleep",
     main = "Partial Dependence Plot: Insufficient Sleep")
dev.off()