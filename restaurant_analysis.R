library(tidyverse)
library(lubridate)
library(ROCR)
library(randomForest)
library(doParallel)
library(foreach)
library(snakecase)

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
    mutate(score=max(score))
             
##### B
  ## filter the rows with specific years and type of inspection and create a binary outcome variable 'outcome'
  restaurant_data <- all_data %>% 
    filter(inspection_year %in% c(2015,2016,2017),
           inspection_type=="Cycle Inspection / Initial Inspection") %>%
    arrange(id) %>%
    distinct(id, inspection_year, .keep_all = TRUE) %>%
    mutate(outcome=ifelse(score>28, 1, 0)) %>%
    select(id, borough, cuisine, inspection_date, inspection_year, outcome)
  
##### C
  ## add month, weekday, and four given features 
  restaurant_data <- restaurant_data %>%
    mutate(inspection_month=month(inspection_date),
           inspection_weekday=weekdays(inspection_date))
  
  tb_join <- merge(restaurant_data, all_data %>% select(id, score, action, inspection_date),
                     by="id", all.x=T, allow.cartesian=T, suffixes = c(".x",".y")) 
  
  tb_join <- as.tibble(tb_join)
  
  tb_features <- tb_join %>%
    filter(inspection_date.y < inspection_date.x) %>% ##952360 obs -> 302945 obs
    group_by(id, inspection_date.x) %>% 
    mutate(num_previous_low_inspections=sum(score<14),
           num_previous_med_inspections=sum(score>=14 & score<28),
           num_previous_high_inspections=sum(score>=28),
           num_previous_previous_closings=sum(action %in% c("closed","re-closed")))
  
  ## restrict to top 50 most common cuisines
  tb_top <- tb_features %>%
    group_by(cuisine) %>%
    count(cuisine, sort=T) %>%
    arrange(desc(n)) %>% head(50) %>%
    select(cuisine)
  
  tb_final <- tb_features %>%
    filter(cuisine %in% tb_top$cuisine)
  
  ## transform cuisine variable into many binary variables for fitting purposes
  tb_cuisine <- tb_final %>% 
    select(id, cuisine) %>% mutate(yesno=1) %>% distinct %>%
    spread(cuisine, yesno, fill=0)
  
  tb_all <- tb_final %>% 
    left_join(tb_cuisine, by = c("id", "inspection_date.x")) %>% 
    ungroup() %>% 
    select(-c(id, cuisine, inspection_date.x,
              score, action, inspection_date.y))
  
  ## convert variable types and clean up names for fitting purposes
  tb_all <- tb_all %>% 
    mutate(outcome = as.factor(outcome)) %>% 
    mutate_if(is.character, as.factor) %>% 
    rename_all(to_snake_case)
  
##### D
  ## create a training set in 2015 and 2016 and a testing set in 2017
  train <- tb_all %>% filter(inspection_year %in% c(2015, 2016))
  test <- tb_all %>% filter(inspection_year==2017)
  
  ## randomly shuffle the data
  train <- train[sample(nrow(train)),]
  test <- test[sample(nrow(test)),]
  
  ## fit a standard logistic regression model
  lm_model <- glm(outcome ~., data=train, family="binomial")
  
  ## compute AUC of this model on the test dataset
  test$predicted.probability.lm <- predict(lm_model, newdata=test, type="response")
  test.pred.lm <- prediction(test$predicted.probability.lm, test$outcome)
  test.perf.lm <- performance(test.pred.lm, "auc")
  auc <- 100*test.perf.lm@y.values[[1]]
  cat('the auc score is ', 100*test.perf.lm@y.values[[1]], "\n") 
  
##### E
  ## fit a random forest model on train 
  rf_model <- randomForest(outcome ~., data=train, ntree=1000, na.action=na.omit)
  
  ## compute AUC of this model on the test dataset  
  test$predicted.probability.rf <- predict(rf_model, newdata=test, type="response")
  test.pred.rf <- prediction(test$predicted.probability.rf, test$outcome)
  test.perf.rf <- performance(test.pred.rf, "auc")
  auc <- 100*test.perf.rf@y.values[[1]]
  cat('the auc score is ', 100*test.perf.rf@y.values[[1]], "\n") 
  
  
##### F
  ## generate precision plot
  lm.plot.data <- test %>% 
    group_by(predicted.probability.lm) %>% 
    mutate(restaurants = n(),
              percent.outcome = cumsum(as.numeric(outcome))/sum(as.numeric(outcome))) %>% 
    arrange(desc(predicted.probability.lm)) %>% 
    ungroup()
    select(restaurants, percent.outcome)
  
  rf.plot.data <- test %>% arrange(desc(predicted.probability.rf)) %>% 
    mutate(numsres = row_number(), percent.outcome = cumsum(outcome)/sum(outcome),
           restaurants = numsres/n()) %>% select(restaurants, percent.outcome)
  
  # create and save plot
  theme_set(theme_bw())
  p <- ggplot() +
          geom_line(data = lm.plot.data, aes(x=restaurants, y=percent.outcome))+ 
          geom_line(data = rf.plot.data, aes(x=restaurants, y=percent.outcome))
          scale_x_continuous('Number of restaurants', limits=c(100, 2000), breaks=c(100, 200, 500, 1000, 1500 ,2000), 
                         labels=c("100", "200", "500", "1000", "1500", "2000")) +
          scale_y_continuous("Percent of outcome predicted", limits=c(0, 1), labels=scales::percent)
  
  ggsave(plot=p, file='performance_plot.pdf', height=5, width=5)
  
  
  
  
