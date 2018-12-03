library(tidyverse)
library(lubridate)
library(ROCR)
library(randomForest)
library(doParallel)
library(foreach)

##### Data Cleaning
  ## load and select the data 
  rawdf <- read.csv("sqf_08_16.csv")
  df <- rawdf %>% 
    filter(suspected.crime=="cpw") %>%
    select(id, year, found.weapon, 
           precinct, 
           location.housing,
           starts_with("stopped.bc."),
           starts_with("additional."),
           suspect.age, suspect.build, suspect.sex, suspect.height, suspect.weight,
           inside, radio.run, officer.uniform,
           observation.period,
           day, month, time.period)

  ## check and convert variable types
  str(df)
  df <- df %>% 
    mutate(id = as.factor(id),
           found.weapon = as.factor(found.weapon),
           precinct = as.factor(precinct),
           time.period = as.factor(time.period))
  
  ## change precinct variable into binary one for modelling
  df_prc <- df %>% 
    select(id, precinct) %>% mutate(yesno=1) %>% distinct %>%
    spread(precinct, yesno, fill=0)
  
  ## deal with the precinct variable for modeling
  colnames(df_prc) <- c("id", paste0("precinct", 2:78))
  
  sqf <- df %>% left_join(df_prc, by="id") %>% select(-precinct)

##### A
  ## restrict sqf to years 2013-2014
  sqf_1314 <- sqf %>% filter(year %in% c(2013,2014))
  
  ## randomly shuffle the data
  set.seed(1314)
  sqf_1314 <- sqf_1314 %>% slice(sample(1:n()))
  
  ## create train_half and test_half
  splitsize = floor(nrow(sqf_1314)/2)
  train_half <- sqf_1314 %>% slice(1:splitsize) %>% select(-id,-year)
  test_half <- sqf_1314 %>% slice(splitsize+1:n()) %>% select(-id,-year)
  
  ## restrict sqf to years 2015 and assign it to test_later
  test_later <- sqf %>% filter(year==2015) %>% select(-id,-year)

##### B
  ## fit a random forest model on train_half
  rfmodel <- randomForest(found.weapon ~., data=train_half, ntree=200, na.action=na.omit)
  
##### C
  ## generate predicted outcomes for test_half
  test_half$predicted.outcome <- predict(rfmodel, newdata=test_half, type="response")
  
  ## generate predicted outcomes for test_later
  test_later$predicted.outcome <- predict(rfmodel, newdata=test_later, type="response")
  
  ## compute the AUC on each test set
  test_half.pred <- prediction(as.numeric(test_half$predicted.outcome), as.numeric(test_half$found.weapon))
  test_half.perf <- performance(test_half.pred, "auc")
  auc <- 100*test_half.perf@y.values[[1]]
  cat('the auc score is ', 100*test_half.perf@y.values[[1]], "\n") 
  
  test_later.pred <- prediction(as.numeric(test_later$predicted.outcome), as.numeric(test_later$found.weapon))
  test_later.perf <- performance(test_later.pred, "auc")
  auc <- 100*test_later.perf@y.values[[1]]
  cat('the auc score is ', 100*test_later.perf@y.values[[1]], "\n") 
  
# AUC on test_half is higher because test_half contains data from the dataset and the same year as the training data
# used to build the random forest model. 
# Test_later is probably a better estimate of the model's performance on unseen data because it contains data taken 
# from a different year from the training set, despite both coming from the same dataset.
# Yes we should always shuffle the data and split randomly for training/test/validation sets. This helps disrupts
# the potential patterns in data entry and ordering, and therefore offers a more realistic and unbiased model.

  