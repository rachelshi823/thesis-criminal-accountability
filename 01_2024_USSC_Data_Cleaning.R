# 2024 USSC Sentencing Report #

install.packages("data.table")
install.packages("dplyr")
library(data.table)
library(dplyr)
library(readr)
library(tidyr)
library(writexl)

# load data and create dataset 

setwd("~/Downloads/USSC FY 2024")

mydata <- fread("opafy24nid.csv", 
                select = c("AGECAT", "MONRACE", "MONSEX", 
                           "OFFGUIDE", "SENTTOT",
                           "DISPOSIT", "DISTRICT", "NEWEDUC"))
View(mydata)
write_csv(mydata, "USSCFY24.csv")

################################################################################
# Census API key --> supplementary socioeconomic conditions:
install.packages("tidycensus")
library(tidycensus)

census_api_key("19613b5818384022ae60eb84c1458ce345d128a1", install = TRUE)

# Get data
acs_data <- get_acs(
  geography = "state",
  variables = c(
    total_population = "B17001_001", # total population for poverty calc
    poverty_count    = "B17001_002", # number below poverty line
    median_income    = "B19013_001"  # median household income
  ),
  year = 2023,  # most recent available
  survey = "acs5"
)
# reshape format
acs_wide <- acs_data %>%
  select(NAME, variable, estimate) %>%
  pivot_wider(
    names_from = variable,
    values_from = estimate
  ) %>%
  rename(State = NAME) %>%
  mutate(
    poverty_rate = round((poverty_count / total_population) * 100, 2)
  ) %>%
  select(State, median_income, poverty_rate)

head(acs_wide)

View(acs_wide)
################################################################################
# Change numbers into words
mydata_labelled <- mydata %>%
  mutate(
    
    # Race
    MONRACE = factor(MONRACE,
                     levels = c(1, 2, 3, 4, 5, 7, 8, 9, 10, 11),
                     labels = c("White/Caucasian",
                                "Black/African American",
                                "American Indian/Alaskan Native",
                                "Asian or Pacific Islander",
                                "Multi-racial",
                                "Other",
                                "Info on Race Not Available",
                                "Non-US American Indians",
                                "American Indians Citizenship Unknown",
                                "Missing")),
    
    # Sex
    MONSEX = factor(MONSEX,
                    levels = c(0,1,2,3),
                    labels = c("Male", "Female","Other","Missing")),
    
    # Age category
    AGECAT = factor(AGECAT,
                    levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                    labels = c("<20",
                               "21-25",
                               "26-30",
                               "31-35",
                               "36-40",
                               "41-50",
                               "51-60",
                               ">61",
                               "Missing")),
    
     # Offense
    OFFGUIDE = factor(OFFGUIDE,
                      levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,
                                 18,19,20,21,22,23,24,25,26,27,28,29,30,31),
                      labels = c("Administration of Justice ", "Antitrust", 
                                 "Arson", "Assault","Bribery/Corruption",
                                 "Burglary/Trespass ","Child Pornography",
                                 "Commercialized Vice", "Drug Possession",
                                 "Drug Trafficking","Environmental",
                                 "Extortion/Racketeering","Firearms",
                                 "Food and Drug","Forgery/Counter/Copyright ",
                                 "Fraud/Theft/Embezzlement","Immigration ",
                                 "Individual Rights ", "Kidnapping", 
                                 "Manslaughter","Money Launder ", "Murder",
                                 "National Defense ","Obscenity/Other Sex Offenses",
                                 "Prison Offenses","Robbery","Sex Abuse",
                                 "Stalking/Harassing","Tax","Other","Missing")),
    # Education
    NEWEDUC = factor(NEWEDUC,
                      levels = c(1, 3, 5, 6, 5),
                      labels = c("Less Than H.S. Graduate",
                                 "H.S. Graduate",
                                 "Some College",
                                 "College Graduate",
                                 "Missing or Indeterminable")),
                      
     # Disposition
    DISPOSIT = factor(DISPOSIT,
                      levels = c(0, 1, 2, 3, 4, 5, 6),
                      labels = c("No Imprisonment",
                                 "Guilty Plea",
                                 "Nolo Contendere",
                                 "Jury Trial",
                                 "Trial by Judge or Bench Trial",
                                 "Guilty Plea and Trial",
                                 "Missing or Indeterminable")),
      # Sentence Length
    SENTTOT = case_when(
      is.na(SENTTOT)    ~ NA_real_,
      SENTTOT == 470    ~ NA_real_,
      TRUE              ~ as.numeric(SENTTOT)
    ),
  )

View(mydata_labelled)
################################################################################
# create state name lookup for USSC district codes
unique(mydata_labelled$DISTRICT)

# convert district to names in USSC dataset
district_to_state <- data.frame(
  DISTRICT = c(00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,
               16,17,18,19,20,22,23,24,25,26,27,
               28,29,30,31,32,33,34,35,36,37,38,39,
               40,41,42,43,44,45,46,47,48,49,50,51,
               52,53,54,55,56,57,58,60,61,62,63,
               64,65,66,67,68,69,70,71,72,73,74,75,
               76,77,78,79,80,81,82,83,84,85,86,87,
               88,89,90,91,93,94,95,96),
  State = c("Maine","Massachusetts","New Hampshire",
            "Rhode Island","Puerto Rico","Connecticut","New York",
            "New York","New York","New York","Vermont",
            "Delaware","New Jersey","Pennsylvania","Pennsylvania",
            "Pennsylvania","Maryland","North Carolina","North Carolina",
            "North Carolina","South Carolina","Virginia","Virginia",
            "West Virginia","West Virginia","Alabama","Alabama","Alabama",
            "Florida","Florida","Florida","Georgia","Georgia","Georgia",
            "Louisiana","Louisiana","Mississippi","Mississippi","Texas","Texas",
            "Texas","Texas","Kentucky","Kentucky","Michigan","Michigan",
            "Ohio","Ohio","Tennessee","Tennessee","Tennessee","Illinois",
            "Illinois","Illinois","Indiana","Indiana",
            "Wisconsin","Wisconsin","Arkansas","Arkansas",
            "Iowa","Iowa","Minnesota","Missouri","Missouri",
            "Nebraska","North Dakota","South Dakota","Arizona",
            "California","California","California","California",
            "Hawaii","Idaho","Montana","Nevada","Oregon",
            "Washington","Washington","Colorado","Kansas",
            "New Mexico","Oklahoma","Oklahoma","Oklahoma",
            "Utah","Wyoming","District of Columbia","Virgin Islands",
            "Guam","N Mariana Islands","Alaska","Louisiana")
)
View(district_to_state)

# Merge everything
# first merge district codes to state names
mydata_state <- mydata_labelled %>%
  left_join(district_to_state, by = "DISTRICT")

# then merge Census SES data by state name
mydata_SES <- mydata_state %>%
  left_join(acs_wide, by = "State")

# check it worked
head(mydata_SES)
colnames(mydata_SES)
View(mydata_SES)
################################################################################
# IPUMS general population data
install.packages("ipumsr")
install.packages('R.utils')
library(ipumsr)
library(data.table)

mydata_ipums <- fread("~/Downloads/usa_00002.csv.gz")
mydata_ipums <- mydata_ipums %>%
  select(SEX, AGE, RACE)

colnames(mydata_ipums)
head(mydata_ipums)

# Step 1 - recode IPUMS age into the same age groups as the USSC data
mydata_ipums <- mydata_ipums %>%
  mutate(AGECAT = case_when(
    AGE < 20              ~ "<20",
    AGE >= 21 & AGE <= 25 ~ "21-25",
    AGE >= 26 & AGE <= 30 ~ "26-30",
    AGE >= 31 & AGE <= 35 ~ "31-35",
    AGE >= 36 & AGE <= 40 ~ "36-40",
    AGE >= 41 & AGE <= 50 ~ "41-50",
    AGE >= 51 & AGE <= 60 ~ "51-60",
    AGE > 60              ~ ">61",
    TRUE                  ~ "Missing"
  ))

# Step 2 - recode IPUMS race to match the USSC race labels
mydata_ipums <- mydata_ipums %>%
  mutate(MONRACE = case_when(
    RACE == 1 ~ "White/Caucasian",
    RACE == 2 ~ "Black/African American",
    RACE == 3 ~ "American Indian/Alaskan Native",
    RACE == 4 ~ "Chinese",
    RACE == 5 ~ "Japanese",
    RACE == 6 ~ "Other Asian or Pacific Islander",
    RACE == 7 ~ "Other race",
    RACE == 8 ~ "Two major races",
    RACE == 9 ~ "Three or more major races",
  ))

# Step 3 - recode IPUMS sex to match the USSC sex labels
mydata_ipums <- mydata_ipums %>%
  mutate(MONSEX = case_when(
    SEX == 1 ~ "Male",
    SEX == 2 ~ "Female",
    SEX == 9 ~ "Missing/Blank" 
  ))

View(mydata_ipums)
# Step 4 - count population by race, sex, age group
population_denominators <- mydata_ipums %>%
  group_by(MONRACE, MONSEX, AGECAT) %>%
  summarise(
    US_Population = n(),
    .groups = "drop"
  )

View(population_denominators)

population_denominators <- population_denominators %>%
  rename(
    "Age Group"          = AGECAT,
    "Race"               = MONRACE,
    "Sex"                = MONSEX,
  )
# merge population counts into the sentencing data
mydata_merged <- mydata_SES %>%
  left_join(population_denominators, 
            by = c("Race", "Sex", "Age Group")) %>%
  mutate(
    Rate_per_100k = round((1 / US_Population) * 100000, 4)
  )

View(mydata_merged)

# rename variables
mydata_merged <- mydata_merged %>%
  rename(
    "US Population (by Race, Sex, and Age Group)" = "US Population (grouped by Race, Sex, and Age Group)",
  )

View(mydata_merged)
# export
write_csv(mydata_merged, "USSC_2024_Final_Analysis.csv")

################################################################################
# ELITE COMPARISON PREP
################################################################################
# paste table names:
library(writexl)
setwd("~/Downloads/USSC FY 2024")

# Load USSC data
ussc <- read_excel("USSC_2024_Final_Analysis_official.xlsx")  # change file name

# Check the key variables
cat("DISPOSITION values:\n")
print(table(ussc$Disposition))

cat("\nOFFENSE values:\n")
print(table(ussc$Offense))

cat("\nSENTENCE LENGTH summary:\n")
print(summary(ussc$`Sentence Length (months)`))

cat("\nRACE breakdown:\n")
print(table(ussc$Race))

cat("\nSEX breakdown:\n")
print(table(ussc$Sex))

#──────────────────────────────────────────────────────────────────────────────#
# Fix sentence length first #

ussc <- ussc %>%
  mutate(
    sentence_months = suppressWarnings(
      as.numeric(`Sentence Length (months)`)
    )
  )

cat("Sentence length after conversion:\n")
print(summary(ussc$sentence_months))

cat("\nHow many converted successfully:\n")
print(sum(!is.na(ussc$sentence_months)))

cat("\nSample of raw values that failed to convert:\n")
print(head(ussc$`Sentence Length (months)`[
  is.na(as.numeric(ussc$`Sentence Length (months)`))], 20))

View(ussc)
write_csv(ussc, "USSC_Comparative_Dataset.csv")

