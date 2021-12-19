# run all steps of the analysis pipeline
source("code/0-download.R")
source("code/1-cleaning.R")
source("code/2-exploration-response.R")

# train-test split before using mental health train and test in other files
source("code/3-train-test-split.R")

source("code/2.5-exploration-features.R")
source("code/2.5-exploration-features/response.R")
source("code/4-regression-modeling.R")
source("code/4.5-elasticnet-modeling.R")
source("code/4.5-lasso-modeling.R")
source("code/4.5-ridge-modeling.R")
source("code/5-tree-modeling.R")
source("code/5-boosted-modeling.R")
source("code/6-model-evaluation.R")
