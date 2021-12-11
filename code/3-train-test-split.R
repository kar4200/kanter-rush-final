# read in the cleaned data
mental_health = read_csv("data/clean/mental_health_clean.csv")

# set seed
set.seed(1)

mental_health_removed = mental_health %>%  
  select(-fips, -state, -name)

# split into train and test (80% for train and 20% for test)
n = nrow(mental_health_removed)
train_samples = sample(1:n, round(0.8*n))

# training set
mental_health_train = mental_health_removed %>% 
  filter(row_number() %in% train_samples)

# test set
mental_health_test = mental_health_removed %>% 
  filter(!(row_number() %in% train_samples))

# save the train and test data
write_csv(x = mental_health_train, 
          file = "data/clean/mental_health_train.csv")
write_csv(x = mental_health_test, 
          file = "data/clean/mental_health_test.csv")
