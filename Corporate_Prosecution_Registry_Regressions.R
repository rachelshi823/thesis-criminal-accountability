library(readxl)
library(stargazer)
library(dplyr)

setwd("~/Downloads/USSC FY 2024")

corp <- read_excel(
  "corp-crime.xlsx",
  sheet = "Corporate_Prosecution_Data"
)

# Create variables
corp$log_payment <- log(corp$TOTAL_PAYMENT + 1)
corp$YEAR <- format(as.Date(corp$DATE), "%Y")

# Convert controls to factors
corp$PRIMARY_CRIME_CODE <- as.factor(corp$PRIMARY_CRIME_CODE)

corp$NAICS <- as.factor(corp$NAICS)

# Collapse Crime Categories
corp$CRIME_GROUP <- dplyr::case_when(
  
  corp$PRIMARY_CRIME_CODE %in% c(
    "Fraud - General",
    "Fraud - Securities",
    "Fraud - Tax",
    "Fraud - Accounting",
    "Fraud - Health Care",
    "False Statements"
  ) ~ "Fraud",
  
  corp$PRIMARY_CRIME_CODE %in% c(
    "Money Laundering",
    "Bank Secrecy Act",
    "FCPA",
    "Bribery",
    "Kickbacks"
  ) ~ "Financial Crimes",
  
  corp$PRIMARY_CRIME_CODE %in% c(
    "Antitrust"
  ) ~ "Antitrust",
  
  corp$PRIMARY_CRIME_CODE %in% c(
    "Environmental",
    "OSHA / Workplace Safety / Mine Safety"
  ) ~ "Environmental & Safety",
  
  corp$PRIMARY_CRIME_CODE %in% c(
    "Controlled Substances / Drugs / Meth Act",
    "FDCA / Pharma",
    "Food"
  ) ~ "Drug / Food / Pharma",
  
  corp$PRIMARY_CRIME_CODE %in% c(
    "Immigration",
    "Import / Export"
  ) ~ "Immigration / Trade",
  
  TRUE ~ "Other"
)

corp$CRIME_GROUP <- as.factor(corp$CRIME_GROUP)


# Create Industry variable
corp$NAICS2 <- substr(as.character(corp$NAICS), 1, 2)
corp$NAICS2 <- as.factor(corp$NAICS2)
corp$INDUSTRY <- dplyr::case_when(
  corp$NAICS2 == "11" ~ "Agriculture",
  corp$NAICS2 == "21" ~ "Mining",
  corp$NAICS2 == "22" ~ "Utilities",
  corp$NAICS2 == "23" ~ "Construction",
  corp$NAICS2 %in% c("31","32","33") ~ "Manufacturing",
  corp$NAICS2 == "42" ~ "Wholesale Trade",
  corp$NAICS2 %in% c("44","45") ~ "Retail Trade",
  corp$NAICS2 %in% c("48","49") ~ "Transportation",
  corp$NAICS2 == "51" ~ "Information",
  corp$NAICS2 == "52" ~ "Finance",
  corp$NAICS2 == "53" ~ "Real Estate",
  corp$NAICS2 == "54" ~ "Professional Services",
  corp$NAICS2 == "56" ~ "Administrative Services",
  corp$NAICS2 == "61" ~ "Education",
  corp$NAICS2 == "62" ~ "Health Care",
  corp$NAICS2 == "71" ~ "Arts & Entertainment",
  corp$NAICS2 == "72" ~ "Accommodation & Food",
  corp$NAICS2 == "81" ~ "Other Services",
  TRUE ~ "Other"
)

corp$YEAR <- as.factor(corp$YEAR)

# Create Scale outcome Variable
table(corp$DISPOSITION_TYPE)

corp$LENIENCY_SCALE <- dplyr::case_when(
  
  corp$DISPOSITION_TYPE == "trial" ~ 0,
  
  corp$DISPOSITION_TYPE == "plea" ~ 1,
  
  corp$DISPOSITION_TYPE == "dismissal" ~ 2,
  
  corp$DISPOSITION_TYPE == "declination" ~ 3,
  
  corp$DISPOSITION_TYPE == "DP" ~ 4,
  
  corp$DISPOSITION_TYPE == "NP" ~ 5,
  
  TRUE ~ NA_real_
)
# REGRESSIONS
# Baseline Model (No Controls)
model1 <- lm(
  log_payment ~ ELITE,
  data = corp
)
summary(model1)

# Controls ONLY (No Eliteness)
model2 <- lm(
  log_payment ~
    PRIMARY_CRIME_CODE +
    INDUSTRY,
  data = corp
)

summary(model2)

# Full Model (Controls + Eliteness)
model3 <- lm(
  log_payment ~
    ELITE +
    PRIMARY_CRIME_CODE +
    INDUSTRY,
  data = corp
)

summary(model3)

# Lenient Outcome Model
model4 <- glm(
  LENIENT_OUTCOME ~
    ELITE +
    CRIME_GROUP +
    INDUSTRY,
  family = binomial(link = "logit"),
  data = corp
)

summary(model4)

# look at each model
stargazer(model1, model2, model3,model4,
          type = "text")


# Ordinal leniency scale model
model5 <- lm(
  LENIENCY_SCALE ~
    ELITE +
    CRIME_GROUP +
    INDUSTRY,
  data = corp
)

summary(model5)
