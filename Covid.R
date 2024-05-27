# Install necessary libraries if not already installed
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(lubridate)) install.packages("lubridate")
if (!require(tidyr)) install.packages("tidyr")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

# Load the datasets from CSV files
patients <- read.csv("patientsUG.csv")
conditions <- read.csv("conditionsUG.csv")
encounters <- read.csv("encountersUG.csv")

# Task 1: Distribution Analysis of COVID Patients

# Filter for COVID-19 patients (confirmed or suspected)
covid_conditions <- conditions %>%
  filter(grepl("COVID", DESCRIPTION, ignore.case = TRUE))

# Merge with patient data to get demographic information
covid_patients <- covid_conditions %>%
  left_join(patients, by = c("PATIENT" = "Id"))

# Calculate age
covid_patients$BIRTHDATE <- ymd(covid_patients$BIRTHDATE)
covid_patients$Age <- as.numeric(difftime(Sys.Date(), covid_patients$BIRTHDATE, units = "weeks")) / 52.25

# Define age groups
covid_patients$AgeGroup <- cut(covid_patients$Age, 
                               breaks = c(-Inf, 18, 35, 50, Inf), 
                               labels = c("0-18", "19-35", "36-50", "51+"))

# Distribution by county
county_distribution <- covid_patients %>%
  group_by(COUNTY) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Distribution by age group
age_distribution <- covid_patients %>%
  group_by(AgeGroup) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Visualize distribution by county
ggplot(county_distribution, aes(x = reorder(COUNTY, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Distribution of COVID Patients by County",
       x = "County",
       y = "Number of Patients")

# Visualize distribution by age group
ggplot(age_distribution, aes(x = AgeGroup, y = Count)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Distribution of COVID Patients by Age Group",
       x = "Age Group",
       y = "Number of Patients")

# Task 2: Common Conditions Analysis

# Identify top 10 most common conditions for COVID-19 patients
top_conditions <- covid_patients %>%
  count(DESCRIPTION) %>%
  arrange(desc(n)) %>%
  head(10)

# Filter the conditions to include only the top 10 conditions
covid_patients_top_conditions <- covid_patients %>%
  filter(DESCRIPTION %in% top_conditions$DESCRIPTION)

# Count conditions by gender
conditions_by_gender <- covid_patients_top_conditions %>%
  group_by(GENDER, DESCRIPTION) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(GENDER, desc(Count))

# Create a table to rank the top 10 conditions for male and female patients separately
conditions_table <- conditions_by_gender %>%
  spread(key = GENDER, value = Count, fill = 0)

# Print the table
print(conditions_table)

# Visualize the top conditions by gender
# Male patients
ggplot(conditions_table, aes(x = reorder(DESCRIPTION, -M), y = M)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Top 10 Conditions for Male COVID-19 Patients",
       x = "Condition",
       y = "Number of Patients")

# Female patients
ggplot(conditions_table, aes(x = reorder(DESCRIPTION, -F), y = F)) +
  geom_bar(stat = "identity", fill = "pink") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Top 10 Conditions for Female COVID-19 Patients",
       x = "Condition",
       y = "Number of Patients")

# Task 3: Analysis of Factors Influencing Hospitalization Rate

# Merge with encounter data to get encounter class
covid_encounters <- covid_patients %>%
  left_join(encounters, by = c("PATIENT" = "PATIENT"), relationship = "many-to-many")

# Filter for relevant encounter classes
relevant_encounters <- covid_encounters %>%
  filter(ENCOUNTERCLASS %in% c("ambulatory", "emergency", "inpatient", "urgent care"))

# Analyze hospitalization rate by age group and encounter class
relevant_encounters$AgeGroup <- cut(relevant_encounters$Age, 
                                    breaks = c(-Inf, 18, 35, 50, Inf), 
                                    labels = c("0-18", "19-35", "36-50", "51+"))

hospitalization_by_age <- relevant_encounters %>%
  group_by(AgeGroup, ENCOUNTERCLASS) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(desc(Count))

# Analyze hospitalization rate by gender and encounter class
hospitalization_by_gender <- relevant_encounters %>%
  group_by(GENDER, ENCOUNTERCLASS) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(desc(Count))

# Visualize hospitalization rate by age group
ggplot(hospitalization_by_age, aes(x = AgeGroup, y = Count, fill = ENCOUNTERCLASS)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Hospitalization Rate by Age Group for COVID-19 Patients",
       x = "Age Group",
       y = "Number of Encounters",
       fill = "Encounter Class")

# Visualize hospitalization rate by gender
ggplot(hospitalization_by_gender, aes(x = GENDER, y = Count, fill = ENCOUNTERCLASS)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Hospitalization Rate by Gender for COVID-19 Patients",
       x = "Gender",
       y = "Number of Encounters",
       fill = "Encounter Class")

# Task 4: Characteristics of Patients Who Recover vs. Those Who Don't

# Determine recovery status
covid_patients <- covid_patients %>%
  mutate(RecoveryStatus = ifelse(is.na(STOP), "Not Recovered", "Recovered"))

# Analyze recovery status by age group
recovery_by_age <- covid_patients %>%
  group_by(AgeGroup, RecoveryStatus) %>%
  summarise(Count = n(), .groups = 'drop')

# Analyze recovery status by gender
recovery_by_gender <- covid_patients %>%
  group_by(GENDER, RecoveryStatus) %>%
  summarise(Count = n(), .groups = 'drop')

# Analyze recovery status by zip code
recovery_by_zip <- covid_patients %>%
  filter(!is.na(ZIP)) %>%
  group_by(ZIP, RecoveryStatus) %>%
  summarise(Count = n(), .groups = 'drop')

# Analyze recovery status by timeline of diagnosis and recovery
covid_patients$StartDate <- ymd(covid_patients$START)
covid_patients$StopDate <- ymd(covid_patients$STOP)
covid_patients$RecoveryTime <- as.numeric(difftime(covid_patients$StopDate, covid_patients$StartDate, units = "days"))

recovery_by_timeline <- covid_patients %>%
  filter(RecoveryStatus == "Recovered") %>%
  summarise(AverageRecoveryTime = mean(RecoveryTime, na.rm = TRUE))

# Visualize recovery status by age group
ggplot(recovery_by_age, aes(x = AgeGroup, y = Count, fill = RecoveryStatus)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Recovery Status by Age Group for COVID-19 Patients",
       x = "Age Group",
       y = "Number of Patients",
       fill = "Recovery Status")

# Visualize recovery status by gender
ggplot(recovery_by_gender, aes(x = GENDER, y = Count, fill = RecoveryStatus)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Recovery Status by Gender for COVID-19 Patients",
       x = "Gender",
       y = "Number of Patients",
       fill = "Recovery Status")

# Visualize recovery status by zip code
ggplot(recovery_by_zip, aes(x = ZIP, y = Count, fill = RecoveryStatus)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Recovery Status by Zip Code for COVID-19 Patients",
       x = "Zip Code",
       y = "Number of Patients",
       fill = "Recovery Status") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Display average recovery time
print(recovery_by_timeline)
