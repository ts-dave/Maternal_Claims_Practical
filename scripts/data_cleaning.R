# Check if packages are available, install them if they are not (using pacman)
if(!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  dplyr,
  janitor
)

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
  ) |> 
  print(n = Inf)


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

# Investigate diplicates
possible_duplicates <- claims |> 
  count(nhisid, facility_name, attend_date) |> #counts how many times a combination occurs
  filter(n > 1) |> 
  arrange(desc(n))

# Check plausiblity and coding
claims %>% 
  filter(age < 10 | age > 60)

claims %>% 
  count(district, sort = TRUE)

claims %>%
  count(type, sort = TRUE)

claims %>%
  count(ownership, sort = TRUE)

claims %>%
  count(delivery_type, sort = TRUE)


# CREATE ANC & DELIVERY INDICATORS
# ------------------------------------------------------------
# FIRST: inspect the available coding
# ------------------------------------------------------------
claims %>% 
  count(gdrg, sort = TRUE)

claims %>%
  count(procedure, sort = TRUE)

claims %>%
  count(diagnosis, sort = TRUE)

claims %>%
  count(delivery_type, sort = TRUE)


# ============================================================
# APPROACH A: PREFERRED POSITIVE ANC DEFINITION
# ============================================================
# Use this approach when ANC-specific diagnosis, procedure,
# or GDRG coding is sufficiently complete and reliable.
# The exact codes must be verified in the dataset.
claims <- claims %>%
  mutate(
    is_delivery = !is.na(delivery_type) |
      str_detect(str_to_upper(gdrg), "OBGY34"),
    is_anc = str_detect(str_to_upper(diagnosis), "ANTENATAL") |
      str_detect(str_to_upper(gdrg), "OPDC02")
)

# ============================================================
# APPROACH B: DATA-QUALITY ADAPTED EXCLUSION DEFINITION
# ============================================================
# Use this only when poor or incomplete ANC coding means that
# Approach A would omit genuine ANC-related encounters.
# Logic: apart from delivery visits and known non-ANC visits,
# all remaining eligible visits are treated as ANC-related.
claims <- claims %>%
  mutate(
    # identify delivery visits first
    is_delivery = !is.na(delivery_type) |
      str_detect(str_to_upper(gdrg), "OBGY34"),

    # 2. Identify known non-ANC GDRG prefixes
    non_anc_gdrg = str_detect(
      str_to_upper(str_squish(gdrg)),
      "^(MEDI|ZOOM|Z00M|ASUR|ORTH|PAED|PSUR)"
    ),

    # 3. Standardize Procedure and Diagnosis for PNC variants
    # PNC, P N C and P.N.C become PNC.
    procedure_std = str_to_upper(procedure) %>%
      str_replace_all("[[:space:]\\.]", ""),

    diagnosis_std = str_to_upper(diagnosis) %>%
      str_replace_all("[[:space:]\\.]", ""),

    # 4. Exclude PNC* values after standardization
    # ^PNC reproduces the intended PNC* logic.
    non_anc_pnc = str_detect(procedure_std, "^PNC") |
      str_detect(diagnosis_std, "^PNC"),

    # 5. Overall non-ANC flag
    is_non_anc = coalesce(non_anc_gdrg, FALSE) | # if any non_anc_gdrg is null, replace with FALSE.
      coalesce(non_anc_pnc, FALSE),

    # 6. All other non-delivery visits are ANC-related
    is_anc = !is_delivery & !is_non_anc,
    
    # 7. Transparent visit classification for validation
    visit_class = case_when(
      is_delivery ~ "Delivery",
      is_non_anc ~ "Non-ANC",
      is_anc ~ "ANC-related",
      TRUE ~ "Unclassified"
    )
  )

# Validate the selected classification
claims %>% 
  count(visit_class, sort = TRUE)


claims %>%
  select(
    nhisid, attend_date, facility_name,
    gdrg, procedure, diagnosis, visit_class
  ) %>%
  arrange(nhisid, attend_date) %>%
  slice_head(n = 50)

## Create Derived Longitudinal Variables
claims <- claims %>%
  arrange(nhisid, attend_date) %>%
  group_by(nhisid) %>%
  mutate(
    visit_order = row_number(),
    previous_facility = lag(facility_name),
    next_facility = lead(facility_name),
    days_since_previous = as.numeric(attend_date - lag(attend_date))
  ) %>%
  ungroup()

# Build and Save the Analysis-Ready Dataset
claims_ready <- claims %>%
  filter(!is.na(nhisid), !is.na(facility_name), !is.na(attend_date)) %>%
  arrange(nhisid, attend_date)


# ------------------------------------------------------------
# Define the analytical cohort
# Retain women with at least 2 ANC-related visits
# ------------------------------------------------------------
eligible_women <- claims_ready %>%
  filter(is_anc) %>%
  count(
    nhisid,
    name = "anc_visits"
  ) %>%
  filter(anc_visits >= 2)

# Retain all encounters for eligible women
claims_eligible <- claims_ready %>%
  semi_join(
    eligible_women,
    by = "nhisid"
  ) %>%
  arrange(nhisid, attend_date)

# Save analysis-ready encounter data as CSV
write_csv(
  claims_ready,
  "data/cleaned/maternal_claims_ready.csv"
)
write_csv(
  claims_eligible,
  "data/cleaned/eligible_maternal_claims_ready.csv"
)
# Save the same datasets in R's native RDS format
saveRDS(
  claims_ready,
  "data/cleaned/maternal_claims_ready.rds"
)
saveRDS(
  claims_eligible,
  "data/cleaned/eligible_maternal_claims_ready.rds"
)


head(claims_raw, n = 5)

claims_raw |> 
  head(n = 5)


###### MODULE 3 ####### 
# #Data Wrangling and Exploratory Data Analysis

#Create a Patient-Level Summary
patient_summary <- claims_ready %>%
  group_by(nhisid) %>%
  summarise(
    age = first(age[!is.na(age)]),
    total_encounters = n(),
    anc_visits = sum(is_anc, na.rm = TRUE),
    facilities = n_distinct(facility_name),
    first_visit = min(attend_date),
    last_visit = max(attend_date),
    .groups = "drop"
  )

write_csv(
  patient_summary,
  "output/tables/patient_summary.csv"
)
saveRDS(
  patient_summary,
  "output/tables/patient_summary.rds"
)

# Create a Facility-Level Summary
facility_summary <- claims_ready %>%
  group_by(facility_name, district, type, ownership) %>%
  summarise(
    women = n_distinct(nhisid),
    encounters = n(),
    deliveries = sum(is_delivery, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(women))

write_csv(
  facility_summary,
  "output/tables/facility_summary.csv"
)
saveRDS(
  facility_summary,
  "output/tables/facility_summary.rds"
)

# Reconstruct All Patient Journeys
# Encounter-level journey: useful for inspection and plots
patient_journey <- claims_ready %>%
  arrange(nhisid, attend_date) %>%
  group_by(nhisid) %>%
  mutate(
    visit_order = row_number(),
    next_facility = lead(facility_name),
    next_date = lead(attend_date)
  ) %>%
  ungroup()

# ANC sequence for each woman
anc_journeys <- claims_eligible %>%
  filter(is_anc) %>%
  arrange(nhisid, attend_date) %>%
  group_by(nhisid) %>%
  summarise(
    anc_sequence = list(facility_name),
    anc_visits = n(),
    facilities_used = n_distinct(facility_name),
    .groups = "drop"
  )

# Delivery facility: one delivery facility per woman
delivery_data <- claims_eligible %>%
  filter(is_delivery) %>%
  arrange(nhisid, attend_date) %>%
  group_by(nhisid) %>%
  summarise(
    delivery_facility = last(facility_name),
    .groups = "drop"
  )

# Complete maternal-care visit sequence.
# The LAST facility in visit_sequence is the delivery facility.
patient_journeys <- anc_journeys %>%
  inner_join(delivery_data, by = "nhisid") %>%
  mutate(
    visit_sequence = map2(
      anc_sequence,
      delivery_facility,
      ~ c(.x, .y)
    )
  ) %>%
  select(
    nhisid, visit_sequence, anc_visits,
    facilities_used, delivery_facility
  )
# ------------------------------------------------------------
# Save journey objects for reuse in later modules
# ------------------------------------------------------------
# Encounter-level journey is rectangular, so save CSV and RDS
write_csv(
  patient_journey,
  "output/tables/patient_journey_encounters.csv"
)
saveRDS(
  patient_journey,
  "output/tables/patient_journey_encounters.rds"
)
# patient_journeys contains list-columns (visit_sequence), so RDS
# is the preferred format because it preserves the R structure.
saveRDS(
  patient_journeys,
  "output/patient_journeys.rds"
)

# Describe Utilization
patient_summary %>%
  summarise(
    women = n(),
    median_encounters = median(total_encounters, na.rm = TRUE),
    median_anc = median(anc_visits, na.rm = TRUE),
    median_facilities = median(facilities, na.rm = TRUE)
  )

patient_summary %>% 
  count(facilities, sort = TRUE)

facility_summary %>% 
  slice_max(women, n = 10)


# Visualize Encounters and Facility Use
ggplot(patient_summary, aes(total_encounters)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Healthcare encounters per woman",
       x = "Number of encounters", 
       y = "Women"
      ) +
  theme_minimal()


facility_summary %>%
  slice_max(women, n = 10) %>%
  ggplot(aes(reorder(facility_name, women), women)) +
  geom_col() + coord_flip() +
  labs(x = NULL, y = "Women", title = "Top facilities by patient volume") +
  theme_minimal()

# Visualize Selected Patient Journeys
selected_ids <- patient_summary %>%
  filter(facilities >= 2) %>%
  slice_head(n = 5) %>%
  pull(nhisid)

patient_journey %>%
  filter(nhisid %in% selected_ids) %>%
  ggplot(aes(attend_date, facility_name, group = nhisid, color = facility_name)) +
    geom_line() + geom_point() +
    facet_wrap(~ nhisid, scales = "free_y") +
    labs(x = "Date",
        y = "Facility", title = "Selected maternal care journeys") +
    theme_minimal()




##### MODULE 4 #####
#Module 4. Measuring Continuity of Care Using Longitudinal Health Claims Data

calculate_mfpc <- function(visit_sequence) {
  visit_sequence <- visit_sequence[!is.na(visit_sequence)]
  if (length(visit_sequence) < 2) 
    return(NA_real_)
  
  anc_sequence <- head(visit_sequence, -1)
  facility_counts <- table(anc_sequence)
  max(facility_counts) / length(anc_sequence)
}


people <- [k: 1, a: 2, m:4]


people[a]



calculate_pdc_prop <- function(visit_sequence) {
if (length(visit_sequence) < 2) return(NA_real_)
delivery_facility <- tail(visit_sequence, 1)
anc_sequence <- head(visit_sequence, -1)
anc_sequence <- anc_sequence[!is.na(anc_sequence)]
if (is.na(delivery_facility) || length(anc_sequence) == 0) {
return(NA_real_)
}
mean(anc_sequence == delivery_facility)
}


K <- c("TRUE", "FALSE", "TRUE"))
mean(k)