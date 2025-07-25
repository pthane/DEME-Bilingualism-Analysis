library(here)
library(tidyverse)


# Load data
Participant_Data <- read_csv(here("CSV Files", "Aggregated Long Data.csv")) %>%
  mutate(
    Maternal_Ed = case_when(
      Maternal_Ed == 0.2 ~ "Missing data",
      Maternal_Ed == 0 ~ "Missing data",
      Maternal_Ed == 1 ~ "Primary School",
      Maternal_Ed == 2 ~ "High School",
      Maternal_Ed == 3 ~ "Some College",
      Maternal_Ed == 4 ~ "Professional Degree", 
      Maternal_Ed == 5 ~ "Bachelor's Degree",
      Maternal_Ed == 6 ~ "Graduate Degree",
      TRUE ~ as.character(Maternal_Ed)),
    Origin = case_when(
      Origin == 0 ~ "Missing data",
      Origin == 1 ~ "Mexico",
      Origin == 2 ~ "Puerto Rico",
      Origin == 3 ~ "Other",
      Origin == 4 ~ "United States",
      Origin == 5 ~ "Guatemala"))

unique(Participant_Data$Maternal_Ed)
unique(Participant_Data$Origin)


# Data by parental education level
Education <- Participant_Data %>%
  group_by(Lang_Group, Maternal_Ed, Code) %>%
  summarise(Rows_Per_Participant = n(), .groups = "drop") %>%
  group_by(Lang_Group, Maternal_Ed) %>%
  summarise(Count = n_distinct(Code), .groups = "drop") %>%
  ungroup()


# Data by origin
Origin <- Participant_Data %>%
  group_by(Lang_Group, Origin, Code) %>%
  summarise(Rows_Per_Participant = n(), .groups = "drop") %>%
  group_by(Lang_Group, Origin) %>%
  summarise(Count = n_distinct(Code), .groups = "drop") %>%
  ungroup()
