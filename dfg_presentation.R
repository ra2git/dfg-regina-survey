library(tidyverse)
library(survey)
library(srvyr)

# set the working directory in RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# import the dataset
# for more details about the dataset, including question wording, contact cam@ra2.io
cleaned_survey_data <- read_rds("./survey_data/survey_data.rds")

# key of postal code starting letter by region (used to group census data)
FSA_key <- c(
  "A" = "Atlantic",
  "B" = "Atlantic",
  "C" = "Atlantic",
  "E" = "Atlantic",
  "G" = "Quebec",
  "H" = "Quebec",
  "J" = "Quebec",
  "K" = "Ontario",
  "L" = "Ontario",
  "M" = "Ontario",
  "N" = "Ontario",
  "P" = "Ontario",
  "R" = "Prairies",
  "S" = "Prairies",
  "T" = "Prairies",
  "V" = "BC",
  "X" = "North",
  "Y" = "North"
)

# read census file from disk
# note: 2016 census by FSA https://www150.statcan.gc.ca/n1/en/catalogue/98-401-X2016046
# dataset is pre-subsetted to minimize git repo size
# see ./census_data/README_meta.txt for file details
census_file <- read_tsv(
  "./census_data/SUBSET_98-401-X2016046_English_TAB_data.csv",
  col_types = cols(.default = "c")
)

# run mice algorithm ------------------------------------------------------

# import the mice package
library(mice)

# select the variables to use in the imputation
cleaned_subset <- cleaned_survey_data %>% 
  select(Gender, Age, Education, Region, Rural_FSA, starts_with("Conflict_"), starts_with("Auth"))

# set some respondent Age values to NA so we have something to imput 
cleaned_subset$Age[c(10, 300, 152, 74, 12, 76, 444, 678, 932, 712, 394)] <- NA

# check what percent are NA

table(cleaned_subset$Gender, useNA = 'always') / nrow(cleaned_subset)
table(cleaned_subset$Age, useNA = 'always') / nrow(cleaned_subset)

# see the mice algorithm options
?mice

# run the mice algorithm with defaults
survey_mids <- cleaned_subset %>% 
  mice(seed = 4)
class(survey_mids)

# TODO: add diagnostics

survey_imputted <- survey_mids %>% 
  complete() %>% 
  mutate(ID = cleaned_survey_data$ID) %>% 
  # select only variables of interest
  select(ID, Gender, Age, Education, Region, Rural_FSA) %>% 
  as_tibble()
survey_imputted

summary(survey_imputted)

cleaned_survey_data %>% 
  select(ID, Gender, Age, Education, Region, Rural_FSA) %>% 
  summary()

# TODO: additional steps: check accuracy, etc.
# check imputed values distribution vs. sample distribution

# weight the data ---------------------------------------------------------

# import 2016 census data by FSA

census_clean <- census_file %>% 
  # filter out summary data
  filter(GEO_LEVEL == "2", CENSUS_YEAR == "2016") %>% 
  # rename columns
  select(
    GEO_NAME, 
    PROFILE_NAME = `DIM: Profile of Forward Sortation Areas (2247)`, 
    PROFILE_ID = `Member ID: Profile of Forward Sortation Areas (2247)`, 
    TOTAL = `Dim: Sex (3): Member ID: [1]: Total - Sex`, 
    MALE = `Dim: Sex (3): Member ID: [2]: Male`, 
    FEMALE = `Dim: Sex (3): Member ID: [3]: Female`
  ) %>% 
  # convert to numeric
  mutate(across(c(TOTAL, MALE, FEMALE), as.double)) %>% 
  # match FSAs to regions
  mutate(
    RegionCode = str_extract(GEO_NAME, "."),
    Region = FSA_key[RegionCode]  # note: this the key defined at the top of the script
  ) %>% 
  filter(Region != "Quebec", Region != "North")

census_clean

# calculate population parameters -----------------------------------------

# gender

weighting_gender <- census_clean %>% 
  filter(PROFILE_ID == "8") %>% 
  select(MALE, FEMALE) %>% 
  pivot_longer(MALE:FEMALE, names_to = "Gender", values_to = "Value") %>% 
  mutate(
    Gender = fct_recode(
      factor(Gender),
      "Male" = "MALE",
      "Female" = "FEMALE"
    ) %>% 
      factor(levels = c("Male", "Female"))
  ) %>% 
  group_by(Gender) %>% 
  summarise(N = sum(Value, na.rm = TRUE), .groups = "drop") %>% 
  # adjust frequency to sample size
  mutate(Freq = N / sum(N) * nrow(cleaned_survey_data)) %>% 
  select(-N)

weighting_gender

# region

weighting_region <- census_clean %>% 
  filter(PROFILE_ID == "8") %>% 
  mutate(
    Region = factor(Region, levels = c("Atlantic", "Ontario", "Prairies", "BC"))
  ) %>% 
  group_by(Region) %>% 
  summarise(N = sum(TOTAL, na.rm = TRUE), .groups = 'drop') %>% 
  mutate(Freq = N / sum(N) * nrow(cleaned_survey_data)) %>% 
  select(-N)

weighting_region

# education

weighting_edu <- census_clean %>% 
  filter(PROFILE_ID %in% c("1684" ,"1685", "1687", "1690", "1691", "1693", "1694", "1695", "1696", "1697")) %>% 
  mutate(
    Education = fct_recode(
      factor(PROFILE_ID),
      "HS" = "1684",
      "HS" = "1685",
      "Trades" = "1687",
      "College" = "1690",
      "Bachelors" = "1691",
      "Bachelors" = "1693",
      "Advanced" = "1694",
      "Advanced" = "1695",
      "Advanced" = "1696",
      "Advanced" = "1697"
    ) %>% 
      factor(levels = c("HS", "Trades", "College", "Bachelors", "Advanced"), ordered = TRUE)
  ) %>% 
  group_by(Education) %>% 
  summarise(N = sum(TOTAL, na.rm = TRUE), .groups = 'drop') %>% 
  mutate(Freq = N / sum(N) * nrow(cleaned_survey_data)) %>% 
  select(-N)

weighting_edu

# age

weighting_age <- census_clean %>% 
  filter(PROFILE_ID %in% c("14" ,"15", "16", "17", "18", "19", "20", "21", "22", "23", "24")) %>% 
  mutate(
    Age = fct_recode(
      factor(PROFILE_ID),
      "18-34" = "14",
      "18-34" = "15",
      "18-34" = "16",
      "18-34" = "17",
      "35-54" = "18",
      "35-54" = "19",
      "35-54" = "20",
      "35-54" = "21",
      "55+" = "22",
      "55+" = "23",
      "55+" = "24"
    ) %>% 
      factor(levels = c("18-34", "35-54", "55+"), ordered = TRUE)
  ) %>% 
  group_by(Age) %>% 
  summarise(N = sum(TOTAL, na.rm = TRUE), .groups = 'drop') %>% 
  mutate(Freq = N / sum(N) * nrow(cleaned_survey_data)) %>% 
  select(-N)

weighting_age

# rural FSA

weighting_rural <- census_clean %>% 
  filter(PROFILE_ID == "8") %>% 
  mutate(
    Rural_FSA = if_else(str_extract(GEO_NAME, "[:digit:]") == 0, "Yes", "No") %>% 
      factor(levels = c("No", "Yes"), ordered = TRUE),
  ) %>% 
  group_by(Rural_FSA) %>% 
  summarise(N = sum(TOTAL, na.rm = TRUE), .groups = 'drop') %>% 
  mutate(Freq = N / sum(N) * nrow(cleaned_survey_data)) %>% 
  select(-N)

weighting_rural

# check if levels match

levels(weighting_age$Age) == levels(cleaned_survey_data$Age)
levels(weighting_region$Region) == levels(cleaned_survey_data$Region)
levels(weighting_gender$Gender) == levels(cleaned_survey_data$Gender)
levels(weighting_edu$Education) == levels(cleaned_survey_data$Education)
levels(weighting_rural$Rural_FSA) == levels(cleaned_survey_data$Rural_FSA)


# rake to census population distributions ---------------------------------

survey_dsgn <- survey::svydesign(ids = ~1, data = survey_imputted)
class(survey_dsgn)

# attempt 1

survey_rake <- rake(
  design = survey_dsgn,
  sample.margins = list(
    ~ Age,
    ~ Education,
    ~ Region,
    ~ Gender,
    ~ Rural_FSA
  ),
  population.margins = list(
    weighting_age,
    weighting_edu,
    weighting_region,
    weighting_gender,
    weighting_rural
  )
)
survey_rake

max(weights(survey_rake))
min(weights(survey_rake))
plot(density(weights(survey_rake)))

# attempt 2

# drop rural margin

survey_rake_2 <- rake(
  survey_dsgn,
  sample.margins = list(
    ~ Age,
    ~ Education,
    ~ Region,
    ~ Gender
  ),
  population.margins = list(
    weighting_age,
    weighting_edu,
    weighting_region,
    weighting_gender
  )
)
survey_rake_2

max(weights(survey_rake_2))
min(weights(survey_rake_2))
plot(density(weights(survey_rake_2)))

# trim weights to reduce effect of single respondent

survey_wts <- survey_rake_2 %>% 
  trimWeights(upper = 3.3, lower = 0.3)

max(weights(survey_wts))
min(weights(survey_wts))
plot(density(weights(survey_wts)))

# see the actual values

survey_wts

survey_wts$variables %>% 
  as_tibble() %>% 
  mutate(WT = weights(survey_wts))


survey_wts %>% 
  as_survey_design() %>% 
  group_by(Age) %>% 
  summarise(N = survey_total(vartype = "ci"))


