#### 2
require(tidyverse)
require(lubridate)
require(ROCR)
require(randomForest)

## A
raw_df <- read_csv("DOHMH_New_York_City_Restaurant_Inspection_Results.csv",na = c("","N/A"))

#Drop columns, make inspection data a date object, rename columns
all_data_clean <- raw_df %>%
  select(-BUILDING,-STREET,-PHONE,-DBA,-ZIPCODE,-`RECORD DATE`,-`VIOLATION DESCRIPTION`, -`GRADE DATE`) %>%
  mutate(inspection_date = lubridate::mdy(`INSPECTION DATE`),
         inspection_year = lubridate::year(inspection_date)) %>%
  select(-`INSPECTION DATE`) %>%
  rename(id = CAMIS,
         borough = BORO,
         cuisine = `CUISINE DESCRIPTION`,
         action = ACTION,
         code = `VIOLATION CODE`,
         critical = `CRITICAL FLAG`,
         score = SCORE,
         grade = GRADE,
         inspection_type = `INSPECTION TYPE`)

#Rename values in action and inspection_type with shorter, simpler values
all_data_clean <- all_data_clean %>%
  mutate(action = case_when(
    action == "Violations were cited in the following area(s)." ~ "Violations",
    action == "Establishment Closed by DOHMH.  Violations were cited in the following area(s) and those requiring immediate action were addressed." ~ "Closed",
    action == "No violations were recorded at the time of this inspection." ~ "No violations",
    action == "Establishment re-opened by DOHMH" ~ "Reopened",
    action == "Establishment re-closed by DOHMH" ~ "Reclosed",
    is.na(action) ~ action
  )
)

#Remove rows with 'Missing' borough information, action == NA, inspection_year == 1900, score < 0 | NA,
#and six inspection types.

all_data_clean <- all_data_clean %>%
  filter(borough != "Missing",
         !is.na(action),
         !is.na(inspection_date), 
         inspection_year != 1900,
         score >= 0,
         !is.na(score),
         !(inspection_type %in% c("Calorie Posting / Re-inspection",
                                  "Inter-Agency Task Force / Re-inspection",
                                  "Smoke-Free Air Act / Re-inspection",
                                  "Administrative Miscellaneous / Re-inspection",
                                  "Trans Fat / Re-inspection",
                                  "Inter-Agency Task Force / Initial Inspection")))

#Create a new column for the max scores on rows with same id and inspection_date
all_data <- all_data_clean %>%
  group_by(id, inspection_date) %>%
  mutate(score = max(score))


## B
restaurant_data <- all_data %>%
  filter(inspection_year %in% c(2015,2016,2017),
         inspection_type == "Cycle Inspection / Initial Inspection") %>%
  arrange(id) %>%
  distinct(id,inspection_date,inspection_year,.keep_all = T) %>%
  mutate(outcome = ifelse(score > 28,1,0)) %>%
  select(id,borough,cuisine,outcome,inspection_date,inspection_year)

## C
restaurant_data <- restaurant_data %>%
  mutate(inspection_month = month(inspection_date),
         inspection_weekday = weekdays(inspection_date))

temp <- all_data %>%
  distinct(id,inspection_date,inspection_year,.keep_all = T) %>%
  arrange(id,inspection_date) %>%
  group_by(id) %>%
  mutate(low_inspect = ifelse(score<14,1,0),
         medium_inspect =ifelse(score>=14 & score<28,1,0),
         high_inspect = ifelse(score>=28,1,0),
         previous_closings = ifelse(action %in% c("Closed","Reclosed"),1,0),
         num_previous_low_inspections = c(0, head(cumsum(low_inspect), -1)),
         num_previous_med_inspections = c(0, head(cumsum(medium_inspect), -1)),
         num_previous_high_inspections = c(0, head(cumsum(high_inspect), -1)),
         num_previous_previous_closings = c(0, head(cumsum(previous_closings), -1))
         ) %>%
  select(-low_inspect,-medium_inspect,-high_inspect,-previous_closings)

tb_features <-  restaurant_data %>%
  left_join(temp)

## restrict to top 50 most common cuisines
tb_top <- tb_features %>%
  group_by(cuisine) %>%
  count(cuisine, sort=T) %>%
  arrange(desc(n)) %>% head(50) %>%
  select(cuisine)


#Convert to factors
tb_final <- tb_features %>%
  filter(cuisine %in% tb_top$cuisine) %>% 
  ungroup %>%
  mutate_at(vars(cuisine,inspection_weekday,inspection_month,borough,outcome),funs(as.factor))


##### D
## create a training set in 2015 and 2016 and a testing set in 2017
train <- tb_final %>% filter(inspection_year %in% c(2015, 2016))
test <- tb_final %>% filter(inspection_year==2017)

## randomly shuffle the data
set.seed(1000)
train <- train[sample(nrow(train)),]
test <- test[sample(nrow(test)),]

## fit a standard logistic regression model
lm_model <- glm(outcome ~ cuisine + borough + inspection_month + inspection_weekday, data=train, family="binomial")

## compute AUC of this model on the test dataset
test$predicted.probability.lm <- predict(lm_model, newdata=test, type="response")
test.pred.lm <- prediction(test$predicted.probability.lm, test$outcome)
test.perf.lm <- performance(test.pred.lm, "auc")
auc <- 100*test.perf.lm@y.values[[1]]
cat('the auc score is ', 100*test.perf.lm@y.values[[1]], "\n") 

##### E
## fit a random forest model on train 
rf_model <- randomForest(outcome ~ cuisine + borough + inspection_month + inspection_weekday +
                           num_previous_low_inspections +num_previous_med_inspections +
                           num_previous_high_inspections + num_previous_previous_closings, 
                         data=train, ntree=1000)

## compute AUC of this model on the test dataset  
test$predicted.probability.rf <- predict(rf_model, newdata=test, type="prob")[,2]
test.pred.rf <- prediction(test$predicted.probability.rf, test$outcome)
test.perf.rf <- performance(test.pred.rf, "auc")
auc <- 100*test.perf.rf@y.values[[1]]
cat('the auc score is ', 100*test.perf.rf@y.values[[1]], "\n") 

##### F

