# analysis.R

# Top Matter  ------

# clear workspace
rm(list = ls())

# include libraries
library(here)
library(readxl)
library(dplyr)

# define paths to data files
data_file1 <- here("data", "Blank Tables.xlsx")
data_file2 <- here("data", "STI_Data_Excel.xlsx")

# load data
df1 <- read_excel(data_file1)
df2 <- read_excel(data_file2)

# explore the data

dim(df1)
dim(df2)

head(df1)
head(df2)

str(df1)

# Filter the data per the inclusion criteria  ------

filtered_data <- df2 %>%
  filter(`What is your gender?` == "Male",
         `How do you describe yourself?` %in% c("I am a man who has sex with both men and women", 
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

# Chlamydia rates by ethnicity ------

# Calculate prevalence rates for Urogenital Chlamydia
urogenital_prevalence <- filtered_data %>%
  group_by(`What is your ethnicity?`) %>%
  summarize(
    Total = n(),
    Positive = sum(`Urogenital Chlamydia Test Result` == "Positive", na.rm = TRUE),
    Prevalence = Positive / Total * 100
  )

# Calculate prevalence rates for Rectal Chlamydia
rectal_prevalence <- filtered_data %>%
  group_by(`What is your ethnicity?`) %>%
  summarize(
    Total = n(),
    Positive = sum(`Rectal Chlamydia` == "Positive", na.rm = TRUE),
    Prevalence = Positive / Total * 100
  )

# Calculate prevalence rates for Oral Chlamydia
oral_prevalence <- filtered_data %>%
  group_by(`What is your ethnicity?`) %>%
  summarize(
    Total = n(),
    Positive = sum(`Oral Chlamydia` == "Positive", na.rm = TRUE),
    Prevalence = Positive / Total * 100
  )

# Print the summaries
print(urogenital_prevalence)
print(rectal_prevalence)
print(oral_prevalence)


# Chlamydia rates by race  ------

# Calculate prevalence rates for Urogenital Chlamydia
urogenital_prevalence_race <- filtered_data %>%
  group_by(`What is your race?`) %>%
  summarize(
    Total = n(),
    Positive = sum(`Urogenital Chlamydia Test Result` == "Positive", na.rm = TRUE),
    Prevalence = Positive / Total * 100
  )

# Calculate prevalence rates for Rectal Chlamydia
rectal_prevalence_race <- filtered_data %>%
  group_by(`What is your race?`) %>%
  summarize(
    Total = n(),
    Positive = sum(`Rectal Chlamydia` == "Positive", na.rm = TRUE),
    Prevalence = Positive / Total * 100
  )

# Calculate prevalence rates for Oral Chlamydia
oral_prevalence_race <- filtered_data %>%
  group_by(`What is your race?`) %>%
  summarize(
    Total = n(),
    Positive = sum(`Oral Chlamydia` == "Positive", na.rm = TRUE),
    Prevalence = Positive / Total * 100
  )
