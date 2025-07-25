library(here)
library(tidyverse)



# Load and prepare data
## Load bilingual children
Bilingual <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Bilingual Data.csv")) %>% 
  mutate(Lang_Group = "Bilingual")

Monolingual <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Monolingual Data.csv")) %>% 
  mutate(Lang_Group = "Monolingual")


## Create group-specific datasets
Bilingual_DLD <- Bilingual %>% 
  filter(Status == 1,
         Lang_Group == "Bilingual") %>% 
  mutate(Status = "DLD",
         Group = "BL-DLD") %>% 
  select(Code, CA_Months, Status, Lang_Group, Group)

Bilingual_TD <- Bilingual %>% 
  filter(Status == 0,
         Lang_Group == "Bilingual") %>% 
  mutate(Status = "TD",
         Group = "BL-TD") %>% 
  select(Code, CA_Months, Status, Lang_Group, Group)

Monolingual_DLD <- Monolingual %>% 
  filter(Status == 1,
         Lang_Group == "Monolingual") %>% 
  mutate(Status = "DLD",
         Group = "ML-DLD") %>% 
  select(Code, CA_Months, Status, Lang_Group, Group)

Monolingual_TD <- Monolingual %>% 
  filter(Status == 0,
         Lang_Group == "Monolingual") %>% 
  mutate(Status = "TD",
         Group = "ML-TD") %>% 
  select(Code, CA_Months, Status, Lang_Group, Group)

Participant_List <- rbind(Bilingual_DLD, Bilingual_TD, Monolingual_DLD, Monolingual_TD)


# Create group averages
Group_Age_Averages <- Participant_List %>% 
  group_by(Group) %>% 
  summarize(Average = mean(CA_Months), SD = sd(CA_Months))


# Create overall average
Overall_Age_Average <- Participant_List %>% 
  summarize(Average = mean(CA_Months), SD = sd(CA_Months))

