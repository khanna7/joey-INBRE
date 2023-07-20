# analysis.R

# Top Matter  ------

# clear workspace
rm(list = ls())

# include libraries
library(here)
library(readxl)
library(dplyr)

# define paths to data files
data_file2 <- here("data", "STI_Data_Excel.xlsx")

# load data
df2 <- read_excel(data_file2)

# explore the data

dim(df2)

head(df2)

# Filter the data per the inclusion criteria  ------
filtered_data <- df2 %>%
  filter(`What is your gender?` == "Male",
         `How do you describe yourself?`%in% c("I am a man who has sex with both men and women", 
                                               "I am a man who only has sex with other men"),
         Age %in% c("55-64", "65-89"))

# Print the first few rows of the filtered data
head(filtered_data)
dim(filtered_data)

# Race/Ethnicity Distribution
table(filtered_data$`Urogenital Chlamydia Test Result`, exclude = NULL)
table(filtered_data$`What is your ethnicity?`, exclude = NULL)

# Chlamydia Results
table(filtered_data$`Rectal Chlamydia`, exclude = NULL)
table(filtered_data$`Oral Chlamydia`, exclude = NULL)
table(filtered_data$`Urogenital Chlamydia Test Result`, exclude = NULL)

# Below are the analysis on the specific #

#Tables:

  # Gonorrhea Tables
  table(filtered_data$`Urogenital Gonorrhea Test Result`, exclude = NULL)
table(filtered_data$`Rectal Gonorrhea`, exclude = NULL)
table(filtered_data$`Rectal Gonorrhea`, exclude = NULL)

# Chlamydia Tables

table(filtered_data$`Rectal Chlamydia`, exclude = NULL)
table(filtered_data$`Oral Chlamydia`, exclude = NULL)
table(filtered_data$`Urogenital Chlamydia Test Result`, exclude = NULL)

# Syphallis Test Result Table
table(filtered_data$`Syphilis Test Result`, exclude = NULL)

#Race Analysis

# Gonorrhea Rectal Race Analysis 
R_G_RECTAL_prevalence <- filtered_data %>%
     group_by(`What is your race?`) %>%
     summarize(
         Total = n(),
         Positive = sum(`Rectal Gonorrhea` == "Positive", na.rm = TRUE),
         "Not performed" = sum(`Rectal Gonorrhea` == "Not performed", na.rm = TRUE),
        Negative = Total - Positive - `Not performed`,
         Prevalence = ifelse((Negative + Positive) == 0, 0, Positive / (Negative + Positive) * 100)
       )

# Gonorrhea Oral Race Analysis
R_G_ORAL_prevalence <- filtered_data %>%
  group_by(`What is your race?`) %>%
  summarize(
    Total = n(),
    Positive = sum(`Oral Gonorrhea` == "Positive", na.rm = TRUE),
    "Not performed" = sum(`Oral Gonorrhea` == "Not performed", na.rm = TRUE),
    Negative = Total - Positive - `Not performed`,
    Prevalence = ifelse((Negative + Positive) == 0, 0, Positive / (Negative + Positive) * 100)
  )

# Gonorrhea Urogenital Race Analysis 
R_G_UROGEN_prevalence <- filtered_data %>%
  group_by(`What is your race?`) %>%
  summarize(
    Total = n(),
    Positive = sum(`Urogenital Gonorrhea Test Result` == "Positive", na.rm = TRUE),
    "Not performed" = sum(`Urogenital Gonorrhea Test Result` == "Not performed", na.rm = TRUE),
    Negative = Total - Positive - `Not performed`,
    Prevalence = ifelse((Negative + Positive) == 0, 0, Positive / (Negative + Positive) * 100)
  )

# Chlamydia Oral Race Analysis
R_C_ORAL_prevalence <- filtered_data %>%
  group_by(`What is your race?`) %>%
  summarize(
    Total = n(),
    Positive = sum(`Oral Chlamydia` == "Positive", na.rm = TRUE),
    "Not performed" = sum(`Oral Chlamydia` == "Not performed", na.rm = TRUE),
    Negative = Total - Positive - `Not performed`,
    Prevalence = ifelse((Negative + Positive) == 0, 0, Positive / (Negative + Positive) * 100)
  )

# Chlamydia Rectal Race Analysis
R_C_RECTAL_prevalence <- filtered_data %>%
  group_by(`What is your race?`) %>%
  summarize(
    Total = n(),
    Positive = sum(`Rectal Chlamydia` == "Positive", na.rm = TRUE),
    "Not performed" = sum(`Rectal Chlamydia` == "Not performed", na.rm = TRUE),
    Negative = Total - Positive - `Not performed`,
    Prevalence = ifelse((Negative + Positive) == 0, 0, Positive / (Negative + Positive) * 100)
  )

# Chlamydia Urogenital Race Analysis
R_C_UROGEN_prevalence <- filtered_data %>%
  group_by(`What is your race?`) %>%
  summarize(
    Total = n(),
    Positive = sum(`Urogenital Chlamydia Test Result` == "Positive", na.rm = TRUE),
    "Not performed" = sum(`Urogenital Chlamydia Test Result` == "Not performed", na.rm = TRUE),
    Negative = Total - Positive - `Not performed`,
    Prevalence = ifelse((Negative + Positive) == 0, 0, Positive / (Negative + Positive) * 100)
  )

# Syphalis Race Analysis 
R_SYP_prevalence <- filtered_data %>%
  group_by(`What is your race?`) %>%
  summarize(
    Total = n(),
    Positive = sum(`Syphilis Test Result` == "Positive", na.rm = TRUE),
    "Not performed" = sum(`Syphilis Test Result` == "Not performed", na.rm = TRUE),
    Negative = Total - Positive - `Not performed`,
    Prevalence = ifelse((Negative + Positive) == 0, 0, Positive / (Negative + Positive) * 100)
  )

####

# Gonorrhea Rectal Ethnicity Analysis 
E_G_RECTAL_prevalence <- filtered_data %>%
  group_by(`What is your ethnicity?`) %>%
  summarize(
    Total = n(),
    Positive = sum(`Rectal Gonorrhea` == "Positive", na.rm = TRUE),
    "Not performed" = sum(`Rectal Gonorrhea` == "Not performed", na.rm = TRUE),
    Negative = Total - Positive - `Not performed`,
    Prevalence = ifelse((Negative + Positive) == 0, 0, Positive / (Negative + Positive) * 100)
  )

# Gonorrhea Oral Ethnicity Analysis
E_G_ORAL_prevalence <- filtered_data %>%
  group_by(`What is your ethnicity?`) %>%
  summarize(
    Total = n(),
    Positive = sum(`Oral Gonorrhea` == "Positive", na.rm = TRUE),
    "Not performed" = sum(`Oral Gonorrhea` == "Not performed", na.rm = TRUE),
    Negative = Total - Positive - `Not performed`,
    Prevalence = ifelse((Negative + Positive) == 0, 0, Positive / (Negative + Positive) * 100)
  )

# Gonorrhea Urogenital Ethnicity Analysis 
E_G_UROGEN_prevalence <- filtered_data %>%
  group_by(`What is your ethnicity?`) %>%
  summarize(
    Total = n(),
    Positive = sum(`Urogenital Gonorrhea Test Result` == "Positive", na.rm = TRUE),
    "Not performed" = sum(`Urogenital Gonorrhea Test Result` == "Not performed", na.rm = TRUE),
    Negative = Total - Positive - `Not performed`,
    Prevalence = ifelse((Negative + Positive) == 0, 0, Positive / (Negative + Positive) * 100)
  )

# Chlamydia Oral Ethnicity Analysis
E_C_ORAL_prevalence <- filtered_data %>%
  group_by(`What is your ethnicity?`) %>%
  summarize(
    Total = n(),
    Positive = sum(`Oral Chlamydia` == "Positive", na.rm = TRUE),
    "Not performed" = sum(`Oral Chlamydia` == "Not performed", na.rm = TRUE),
    Negative = Total - Positive - `Not performed`,
    Prevalence = ifelse((Negative + Positive) == 0, 0, Positive / (Negative + Positive) * 100)
  )

# Chlamydia Rectal Ethnicity Analysis
E_C_RECTAL_prevalence <- filtered_data %>%
  group_by(`What is your ethnicity?`) %>%
  summarize(
    Total = n(),
    Positive = sum(`Rectal Chlamydia` == "Positive", na.rm = TRUE),
    "Not performed" = sum(`Rectal Chlamydia` == "Not performed", na.rm = TRUE),
    Negative = Total - Positive - `Not performed`,
    Prevalence = ifelse((Negative + Positive) == 0, 0, Positive / (Negative + Positive) * 100)
  )

# Chlamydia Urogenital Ethnicity Analysis
E_C_UROGEN_prevalence <- filtered_data %>%
  group_by(`What is your ethnicity?`) %>%
  summarize(
    Total = n(),
    Positive = sum(`Urogenital Chlamydia Test Result` == "Positive", na.rm = TRUE),
    "Not performed" = sum(`Urogenital Chlamydia Test Result` == "Not performed", na.rm = TRUE),
    Negative = Total - Positive - `Not performed`,
    Prevalence = ifelse((Negative + Positive) == 0, 0, Positive / (Negative + Positive) * 100)
  )

# Syphalis Ethnicity Analysis 
E_SYP_prevalence <- filtered_data %>%
  group_by(`What is your ethnicity?`) %>%
  summarize(
    Total = n(),
    Positive = sum(`Syphilis Test Result` == "Positive", na.rm = TRUE),
    "Not performed" = sum(`Syphilis Test Result` == "Not performed", na.rm = TRUE),
    Negative = Total - Positive - `Not performed`,
    Prevalence = ifelse((Negative + Positive) == 0, 0, Positive / (Negative + Positive) * 100)
  )

























