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
