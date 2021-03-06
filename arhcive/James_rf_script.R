require(tidyverse)
require(randomForest)
require(ROCR)

#### 1
sqf_data <- read_csv("sqf_08_16.csv")
sqf <- sqf_data %>%
  filter( suspected.crime == "cpw") %>% 
  select(id, year, found.weapon, 
         precinct, 
         location.housing,
         starts_with("stopped.bc."),
         starts_with("additional."),
         suspect.age, suspect.build, suspect.sex, suspect.height, suspect.weight,
         inside, radio.run, officer.uniform,
         observation.period,
         day, month, time.period)

# Convert variable types as necessary
sqf <- sqf %>% mutate(id = as.factor(id),
                      location.housing = as.factor(location.housing),
                      suspect.build = as.factor(suspect.build),
                      suspect.sex = as.factor(suspect.sex),
                      found.weapon = as.factor(found.weapon),
                      day = as.factor(day),
                      month = as.factor(month),
                      time.period = as.factor(time.period),
                      precinct = as.factor(precinct))

#convert 'precinct' into binary variables
precinct_df <- sqf %>% 
  select(id, precinct) %>% 
  mutate(yesno = 1) %>%
  distinct %>% 
  spread(precinct, yesno, fill = 0) 
#change column names for modeling
colnames(precinct_df) <- c("id", paste0("precinct_", 2:78))             

# join precinct binary variables back to the main dataframe
sqf_binary <- sqf %>% 
  left_join(precinct_df, by = "id") %>% 
  select(-precinct)

## A
sqf_2013_2014 <- sqf_binary %>%
  filter(year %in% c(2013,2014))

set.seed(2013)
slice_idx = sample(nrow(sqf_2013_2014))

train_half <- sqf_2013_2014 %>%
  slice(slice_idx[1:(n()/2)]) %>%
  select(-id, -year)
test_half <- sqf_2013_2014 %>%
  slice(slice_idx[(n()/2+1):n()]) %>% 
  select(-id, -year)
test_later <- sqf %>%
  filter(year == 2015) %>% 
  select(-id, -year)


## B
#random forest classification model
memory.limit(100000)
rf_model <- randomForest(found.weapon ~., data = train_half, ntree = 200, na.action = na.omit)


## C
#predicted probabilities on test data
test_half$predicted.probability <- predict(rf_model, newdata = test_half, type = "response")

#predicted probabilities on test_later data
test_later$predicted.probability <- predict(rf_model, newdata = test_later, type = "response")

#AUC
test_half.pred <- prediction(test_half$predicted.probability, test_half$found.weapon)
test_half.perf <- performance(test_half.pred, "auc")
auc <- 100*test_half.perf@y.values[[1]]
paste0("the auc score for test_half is ", auc)

test_later.pred <- prediction(test_later$predicted.probability, test_later$found.weapon)
test_later.perf <- performance(test_later.pred, "auc")
auc <- 100*test_later.perf@y.values[[1]]
paste0("the auc score for test_later is ", auc)

