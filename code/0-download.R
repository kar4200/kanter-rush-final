# load libraries
library(tidyverse)

# download datasets
health_data <- read_csv("~/Desktop/Stat471/stat-471-fall-2021/kanter-rush-final/data/raw/measure_data.csv")
feature_data <- read_csv("~/Desktop/Stat471/stat-471-fall-2021/kanter-rush-final/data/raw/additional_features.csv")
demographic_data <- read_csv("~/Desktop/Stat471/stat-471-fall-2021/kanter-rush-final/data/raw/county_complete.csv")