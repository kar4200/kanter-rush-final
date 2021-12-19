# load libraries
library(tidyverse)

# download datasets
health_data <- read_csv("data/raw/measure_data.csv")
feature_data <- read_csv("data/raw/additional_features.csv")
demographic_data <- read_csv("data/raw/county_complete.csv")

