install.packages("readxl")
library(readxl)
library(dplyr)

df <- read_excel("~/Downloads/USSC FY 2024/USSC_2024_Final_Analysis_official.xlsx")
View(df)
colSums(is.na(df[, c("Age_Group", "Race", "Sex", "Highest_Education_Level", "Median_Income")]))
colMeans(is.na(df[, c("Age_Group", "Race", "Sex", "Highest_Education_Level", "Median_Income")]))
# remove space in variable names
df <- df %>%
  rename(Median_Income = `Median Income`)

df <- df %>%
  rename(Poverty_Rate = `Poverty Rate %`)

df <- df %>%
  rename(Age_Group = `Age Group`)

df <- df %>%
  rename(Sentence_Length = `Sentence Length (months)`)

df <- df %>%
  rename(Highest_Education_Level = `Highest Education Level`)

# convert "NA" to real NA
library(dplyr)
library(stringr)

df <- df %>%
  mutate(
    across(
      c(Sex, Race, Age_Group, Highest_Education_Level, Offense, Sentence_Length),
      ~ as.character(.)
    )
  ) %>%
  mutate(
    across(
      c(Sex, Race, Age_Group, Highest_Education_Level, Offense, Sentence_Length),
      ~ str_trim(.)
    )
  ) %>%
  mutate(
    across(
      c(Sex, Race, Age_Group, Highest_Education_Level, Offense, Sentence_Length),
      ~ case_when(
        . %in% c("", "NA", "N/A", "na", "Na", "Unknown", "unknown", "Not Available", "Missing") ~ NA_character_,
        TRUE ~ .
      )
    )
  ) %>%
  mutate(
    Sex = factor(Sex),
    Race = factor(Race),
    Age_Group = factor(Age_Group),
    Highest_Education_Level = factor(Highest_Education_Level),
    Offense = factor(Offense),
    Sentence_Length = factor(Sentence_Length)
  )

table(df$Sex, useNA = "ifany")
table(df$Race, useNA = "ifany")
table(df$Age_Group, useNA = "ifany")
table(df$Highest_Education_Level, useNA = "ifany")
table(df$Offense, useNA = "ifany")

# remove NA entirely
df <- df %>%
  filter(!is.na(Sex), !is.na(Race), !is.na(Age_Group), !is.na(Offense),
       !is.na(Highest_Education_Level),!is.na(Sentence_Length))
View(df)

# remove Non-US American Indian
df <- df %>%
  filter(race != "Non-US American Indians")
# make sentence length numeric
df <- df %>%
  mutate(
    sentence_num = as.numeric(as.character(Sentence_Length))
  )
# turning median income into a dummy 
df <- df %>%
  filter(!is.na(Median_Income))

df <- df %>%
  mutate(
    income_quartile = ntile(Median_Income, 4)
  )

df$income_quartile <- factor(
  df$income_quartile,
  levels = c(1, 2, 3, 4),
  labels = c(
    "Bottom Quartile",
    "Second Quartile",
    "Third Quartile",
    "Top Quartile"
  )
)

# set bottom quartile as control
df$income_quartile <- relevel(df$income_quartile, ref = "Bottom Quartile")
################################################################################
# REGRESSION OLS MODELS #
install.packages("modelsummary")
library(modelsummary)

# creating new column for dummies
library(fastDummies)

df <- dummy_cols(df, select_columns = "income_quartile", remove_first_dummy = TRUE)

# set up variables correctly
df <- df %>%
  mutate(
    Race = factor(Race),
    Sex = factor(Sex),
    Age_Group = factor(Age_Group),
    Offense = factor(Offense),
    Highest_Education_Level = factor(Highest_Education_Level)
  )
# clean names 
install.packages("janitor")
library(janitor)

df <- clean_names(df)

# Model 1: Baseline SES (no control)
m1 <- lm(
  log(sentence_num) ~ 
    income_quartile_second_quartile +
    income_quartile_third_quartile +
    income_quartile_top_quartile,
  data = df
)

summary(m1)

# Model 2: Demographics Only Model
m2 <- lm(
  log(sentence_num) ~ 
    sex+
    race+
    age_group,
  data = df
)

summary(m2)

# Model 3: Income-only vs Full Model
# Create new dummy variable to test top quartile against all others
df <- df %>%
  mutate(
    income_quartile_top_quartile = ifelse(
      income_quartile == "Top Quartile",
      1, 0
    )
  )

df$income_quartile_top_quartile <- factor(df$income_quartile_top_quartile, levels = c(0,1),
                          labels = c("Bottom 75%", "Top 25%"))

m3 <- lm(
  log(sentence_num) ~ 
    income_quartile_top_quartile+
    sex+
    race+
    age_group+
    offense,
  data = df
)

summary(m3)
levels(df$sex)
levels(df$race)
levels(df$age_group)
levels(df$offense)
################################################################################
# VISUAL PLOTS #
library(ggplot2)
library(ggeffects)

# Model 1
# code predicted values
coefs <- coef(m1)

# Build prediction table
pred_income <- data.frame(
  income_quartile = c(
    "Bottom Quartile",
    "Second Quartile",
    "Third Quartile",
    "Top Quartile"
  ),
  log_pred = c(
    coefs["(Intercept)"],
    coefs["(Intercept)"] + coefs["income_quartile_second_quartile"],
    coefs["(Intercept)"] + coefs["income_quartile_third_quartile"],
    coefs["(Intercept)"] + coefs["income_quartile_top_quartile"]
  )
)

# Convert back from log → actual sentence length
pred_income <- pred_income %>%
  mutate(
    predicted_sentence = exp(log_pred)
  )

# plot
ggplot(pred_income, aes(x = income_quartile, y = predicted_sentence, group = 1)) +
  geom_point(size = 5) +
  geom_line(linewidth = 1) +
  geom_text(
    aes(label = round(predicted_sentence, 1)),
    vjust = -0.8,
    size = 5,
    fontface = "bold"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
  labs(
    title = "Model 1: Predicted Sentence Length by Income Quartile",
    subtitle = "Unadjusted model (income only; no demographic or offense controls)",
    x = "Income Quartile",
    y = "Predicted sentence length"
  ) +
  theme_minimal(base_size = 18, base_family = "Times") +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Model 2
library(broom)
library(dplyr)
library(stringr)
library(forcats)
library(ggplot2)

m2_tidy <- tidy(m2, conf.int = TRUE) %>%
  mutate(
    estimate = exp(estimate),
    conf.low = exp(conf.low),
    conf.high = exp(conf.high)
  )

m2_tidy <- m2_tidy %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = recode(term,
                  
                  # Sex
                  "sexMale" = "Male",
                  
                  # Age
                  "age_group21-25" = "21–25",
                  "age_group26-30" = "26–30",
                  "age_group31-35" = "31–35",
                  "age_group36-40" = "36–40",
                  "age_group41-50" = "41–50",
                  "age_group51-60" = "51–60",
                  "age_group>61"   = ">61",
                  
                  # Race
                  "raceBlack/African American" = "Black/African American",
                  "raceWhite/Caucasian" = "White/Caucasian",
                  "raceAsian or Pacific Islander" = "Asian or Pacific Islander",
                  "raceInfo on Race Not Available" = "Info on Race Not Available",
                  "raceOther" = "Other",
                  "raceMulti-racial" = "Multi-racial"
                  
    )
  )
m2_tidy <- m2_tidy %>%
  mutate(term = factor(term, levels = rev(term)))

# Plot
library(ggplot2)

ggplot(m2_tidy, aes(x = estimate, y = term)) +
  
  # CI bars
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2, size = 0.8) +
  
  # points
  geom_point(size = 3) +
  
  # value labels
  geom_text(aes(label = round(estimate, 2)),
            nudge_y = 0.35, size = 4) +
  
  # reference line at 1
  geom_vline(xintercept = 1, linetype = "dashed") +
  
  # labels
  labs(
    title = "Model 2: Demographic Effects on Sentence Length",
    subtitle = "Points are exponentiated coefficients; bars show 95% confidence intervals",
    x = "Sentence length (relative to reference group)",
    y = NULL
  ) +
  
  # theme styling (to match your example)
  theme_minimal(base_family = "Times") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    panel.grid.major.y = element_line(color = "grey85"),
    panel.grid.minor = element_blank()
  ) +
  
  # expand so labels fit
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.15)))


# fix font
library(showtext)
showtext_auto()

font_add("Times", "/System/Library/Fonts/Times.ttc")
showtext_auto()

# Model 3
library(broom)
library(dplyr)
library(ggplot2)
library(forcats)
library(stringr)

# build coef plot 
coef_m3 <- tidy(m3, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    estimate_exp = exp(estimate),
    conf.low_exp = exp(conf.low),
    conf.high_exp = exp(conf.high),
    label = round(estimate_exp, 2),
    
    group = case_when(
      str_detect(term, "^income_quartile_top_quartile") ~ "Income (SES)",
      str_detect(term, "^sex") ~ "Sex",
      str_detect(term, "^race") ~ "Race",
      str_detect(term, "^age_group") ~ "Age",
      str_detect(term, "^offense") ~ "Offense",
      TRUE ~ "Other"
    )
  ) %>%
  
  # remove offense terms from plot 
  filter(group != "Offense") %>%
  
  mutate(
    term_clean = term %>%
      str_remove("^income_quartile_top_quartile") %>%
      str_remove("^sex") %>%
      str_remove("^race") %>%
      str_remove("^age_group") %>%
      str_replace_all("_", " ") %>%
      str_to_title()
  )

# plot visual model
ggplot(coef_m3, aes(x = estimate_exp, y = fct_reorder(term_clean, estimate_exp))) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(
    aes(xmin = conf.low_exp, xmax = conf.high_exp),
    height = 0.2
  ) +
  geom_point(size = 3.5) +
  geom_text(
    aes(x = conf.high_exp, label = label),
    hjust = -0.3,
    size = 3.8,
    fontface = "bold"
  ) +
  facet_wrap(~group, scales = "free_y", ncol = 1) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.25))) +
  scale_y_discrete(expand = expansion(mult = c(0.15, 0.15))) +
  labs(
    title = "Model 3: Effects of Being in the Top Income Quartile on Sentence Length (Adjusted)",
    x = "Sentence length (relative to reference group)",
    y = NULL
  ) +
  theme_minimal(base_size = 14, base_family = "Times") +
  theme(
    axis.title.x = element_text(size = 13),
    plot.title = element_text(size = 16)
      
  )
################################################################################
