library(tidyverse)
library(dplyr)
library(janitor)

# Import data
claims_raw <- read_csv("data/raw/Final Final Data.csv")

names(claims_raw)
dim(claims_raw)
glimpse(claims_raw)
head(claims_raw)
head(claims_raw)
head(claims_raw, 20)
summary(claims_raw)


# PATIENT FIRST JOURNEY

one_id <- claims_raw %>%
  filter(!is.na(NHISID)) %>%  #filter non-null IDs
  slice(1) %>%      #slice the first n entries
  pull(NHISID)    #pull NHISID and assign to the variable one_id


one_woman <- claims_raw |> 
  filter(NHISID == one_id) |> 
  arrange(desc(AttendDate)) |> 
  select(NHISID, AttendDate, FacilityName, DeliveryType)


one_woman



###### MODULE 2 #######

# Import and Standardize Names
claims <- claims_raw |> 
  clean_names()

# Standardize key text fields
claims <- claims |> 
  mutate(nhisid = as.character(nhisid),
        facility_name = str_squish(str_to_upper(facility_name)),
        district = str_squish(str_to_title(district)),
        type = str_squish(str_to_title(type)),
        ownership = str_squish(str_to_title(ownership))
)

#Establish canonical analytical visit date
#variable is attend_date and should be used directly
claims <- claims |> 
  mutate(
    attend_date_raw = attend_date,
    attend_date = lubridate::dmy(
      attend_date,
      quiet = TRUE
    )
  )

# # Check if attend_date parsed correctly
summary(claims$attend_date_raw)
summary(claims$attend_date)

# Checking null entries before date was parsed
claims |> 
  filter(is.na(attend_date_raw)) |> 
  select(
    nhisid,
    facility_name,
    age,
    folder_no, 
    attend_date, 
    attend_date_raw
  )

# Checking all null entries after date was parsed.
claims |> 
  filter(is.na(attend_date)) |> #filter all rows with parsed date null
  select(
    nhisid, 
    facility_name, 
    age, 
    folder_no, 
    attend_date_raw, 
    attend_date 
  ) |> #select only interested variables
  print(n = Inf) #print all entries


# # Optional audit of the source strings and parsed dates
claims |> 
  summarise( #summarise works like summary function but, but create variables interested in.
    total_records = n(), # total entries
    missing_source_attend_date = sum( #sum calculates no. of rows with codition evaluating to true
      is.na(attend_date_raw) | attend_date_raw == "" #condition in sum gets evaluated.
    ),
    missing_parsed_attend_date = sum(is.na(attend_date))
  )

# Assessing missingness
claims |> 
  summarise(
    missing_id = sum(is.na(nhisid) | nhisid == ""),
    missing_facility = sum(is.na(facility_name) | facility_name == ""),
    missing_date = sum(is.na(attend_date)),
    missing_age = sum(is.na(age))
  )

