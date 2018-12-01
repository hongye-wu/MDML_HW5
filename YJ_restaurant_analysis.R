library(tidyverse)
library(lubridate)
library(ROCR)
library(randomForest)
library(doParallel)
library(foreach)

##### A
### a
  ## drop the selected columns and make inspection date a date object
  rawdf <- read.csv("DOHMH_New_York_City_Restaurant_Inspection_Results.csv")
  df <- rawdf %>% 
    select(-BUILDING,-STREET,-PHONE,-DBA,-ZIPCODE,-RECORD.DATE,-VIOLATION.DESCRIPTION, -GRADE.DATE) %>%
    mutate(inspection_date=lubridate::mdy(INSPECTION.DATE),
           inspection_year=lubridate::year(inspection_date)) %>%
    select(-INSPECTION.DATE)
  
  ## rename columns and the values in the action and inspection_type
  df_rename <- df %>% 
    rename("id"="CAMIS", "borough"= "BORO", "cuisine"="CUISINE.DESCRIPTION", "action"="ACTION", 
           "code"="VIOLATION.CODE", "critical"="CRITICAL.FLAG", "score"="SCORE", 
           "grade"="GRADE", "inspection_type"="INSPECTION.TYPE") %>%
    mutate(action=case_when(action=="Violations were cited in the following area(s)." ~ "violation",
           action=="Establishment Closed by DOHMH.  Violations were cited in the following area(s) and those requiring immediate action were addressed." ~ "closed",
           action=="No violations were recorded at the time of this inspection." ~ "none",
           action=="Establishment re-opened by DOHMH" ~ "reopened",
           action=="Establishment re-closed by DOHMH" ~ "reclosed"))

### b
  ## deal with missing borough information
  df_remove <- df_rename %>%
    filter(borough != "Missing", !is.na(action), !is.na(code), 
           !is.na(score), score >=0, !is.na(grade), 
           !(inspection_type %in% c("Calorie Posting / Re-inspection",
                                    "Inter-Agency Task Force / Re-inspection",
                                    "Smoke-Free Air Act / Re-inspection",
                                    "Administrative Miscellaneous / Re-inspection",
                                    "Trans Fat / Re-inspection",
                                    "Inter-Agency Task Force / Initial Inspection")),
           !is.na(inspection_date), !is.na(inspection_year), inspection_year != 1900)
  
### c
  ## replace scores set on the given cases with its maximum score
  all_data <- df_remove %>%
    group_by(id, inspection_date) %>%
    mutate(score_max = max(score)) #which(all_data$score!=all_data$score_max)
             
##### B
  ## create a binary outcome variable 'outcome'
  
  ## keep the given features

  
##### C
  ## add month, weekday, and four given features 
  
  ## restrict to top 50 most common cuisines
  
##### D
  ## create a training set in 2015 and 2016 and a testing set in 2017
  train <- restaurant_data %>% filter(year %in% c(2015, 2016))
  test <- restaurant_data %>% filter(year==2017)
  
  ## randomly shuffle the data
  train <- train[sample(nrow(train)),]
  test <- test[sample(nrow(test)),]
  
  ## fit a standard logistic regression model
  memory.limit(100000)
  lm_model <- glm(outcome ~ cuisine, borough, month, weekday, num_previous_low_inspections, 
                  num_previous_med_inspections, num_previous_high_inspections, num_previous_previous_inspections,
                  data=train, family="binomial")
  
  ## compute AUC of this model on the test dataset
  test$predicted.probability <- predict(lm_model, newdata=test, type="response")
  test.pred <- prediction(test$predicted.probability, test$outcome)
  test.perf <- performance(test.pred, "auc")
  auc <- 100*test.perf@y.values[[1]]
  cat('the auc score is ', 100*test.perf@y.values[[1]], "\n") 
  
##### E
  ## fit a random forest model on train 
  rf_model <- randomForest(outcome ~ cuisine, borough, month, weekday, num_previous_low_inspections, 
                           num_previous_med_inspections, num_previous_high_inspections, num_previous_previous_inspections,
                           data=train, ntree=1000, na.action=na.omit, proximity=TRUE)
  
  ## compute AUC of this model on the test dataset  
  test$predicted.probability <- predict(rf_model, newdata=test, type="response")
  test.pred <- prediction(test$predicted.probability, test$outcome)
  test.perf <- performance(test.pred, "auc")
  auc <- 100*test.perf@y.values[[1]]
  cat('the auc score is ', 100*test.perf@y.values[[1]], "\n") 
  
##### F
  ## create a plot with the number of restaurants and the corresponding model precision
  
  
