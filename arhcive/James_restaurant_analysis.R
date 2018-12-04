#### 2
require(tidyverse)
require(lubridate)
require(ROCR)
require(randomForest)
require(doParallel)
require(foreach)

## A
all_data <- read_csv("DOHMH_New_York_City_Restaurant_Inspection_Results.csv")

#Drop columns, make inspection data a date object, rename columns
all_data_clean <- all_data %>%
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
restaurant_data <- all_data_clean %>%
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

temp_tibble <- merge(x=restaurant_data,
                     y= all_data_clean %>% select(id,score_max,action,inspection_date),
                     all.x = T,
                     allow.cartesian = T)

temp <- all_data %>%
  arrange(id,inspection_date) %>%
  group_by(id) %>%
  mutate(low_inspect = ifelse(score<14,1,0),
         medium_inspect =ifelse(score>=14 & score<28,1,0),
         high_inspect = ifelse(score>=28,1,0),
         previous_closings = ifelse(action %in% c("closed","re-closed"),1,0),
         num_previous_low_inspections = c(0, head(cumsum(low_inspect), -1)),
         num_previous_med_inspections = c(0, head(cumsum(medium_inspect), -1)),
         num_previous_high_inspections = c(0, head(cumsum(high_inspect), -1)),
         num_previous_previous_closings = c(0, head(cumsum(previous_closings), -1))
         ) %>%
  select(-low_inspect,-medium_inspect,-high_inspect,-previous_closings)

restaurant_data1 <-  restaurant_data %>%
  left_join(temp)


