
### Set Up ###
library(tidyverse)
library(lubridate)
library(randomForest)
library(ROCR)

### Question 1 ###
raw_df <- read_csv("sqf_08_16.csv")

### Data Clean Up ###
sqf <- raw_df %>%
  filter( suspected.crime == "cpw") %>% 
  select(id, year, found.weapon, precinct,
         suspect.age, suspect.build, suspect.sex, suspect.height, suspect.weight,
         stopped.bc.desc, stopped.bc.violent, stopped.bc.other, stopped.bc.object, 
         stopped.bc.casing, stopped.bc.lookout, stopped.bc.drugs, stopped.bc.clothing, 
         stopped.bc.furtive, stopped.bc.bulge, 
         inside, observation.period, officer.uniform,
         additional.report, additional.investigation, additional.proximity, additional.evasive,
         additional.associating, additional.direction, additional.highcrime, additional.time, 
         additional.sights, additional.other, radio.run, day, month, time.period)

# Convert variable types as necessary
sqf <- sqf %>% mutate(id = as.factor(id),
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

### A ###
sqf_1314 <- sqf_binary %>% filter(year %in% c("2013", "2014"))

#randomly shuffle data
set.seed(123)
sqf_set <- sqf_1314 %>% slice(sample(1:n())) 

#split training and test sets
split_size = floor(nrow(sqf_set)/2)
train_half <- sqf_set %>% slice(1:split_size) %>% 
  select(-id, -year)
test_half <- sqf_set %>% slice(split_size+1:n()) %>% 
  select(-id, -year)

#restrict to only 2015 data for testing later
test_later <- sqf_binary %>% filter(year == "2015") %>% 
  select(-id, -year)

### B ###
#random forest classification model
rf_model <- randomForest(found.weapon ~., data = train_half, ntree = 200, na.action = na.omit, proximity = TRUE)
  # my vector memory exhuasted by trying to run this model. WBU?

### C ###

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

# NEED EXPLAINATIONS