library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(broom.mixed)
library(openxlsx)
library(here)


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
         Group = "BL-DLD",
         Participant_Count = 33) %>% 
  select(Code, CA_Months, Status, Lang_Group, Group, Participant_Count)

Bilingual_TD <- Bilingual %>% 
  filter(Status == 0,
         Lang_Group == "Bilingual") %>% 
  mutate(Status = "TD",
         Group = "BL-TD",
         Participant_Count = 33) %>% 
  select(Code, CA_Months, Status, Lang_Group, Group, Participant_Count)

Monolingual_DLD <- Monolingual %>% 
  filter(Status == 1,
         Lang_Group == "Monolingual") %>% 
  mutate(Status = "DLD",
         Group = "ML-DLD",
         Participant_Count = 25) %>% 
  select(Code, CA_Months, Status, Lang_Group, Group, Participant_Count)

Monolingual_TD <- Monolingual %>% 
  filter(Status == 0,
         Lang_Group == "Monolingual") %>% 
  mutate(Status = "TD",
         Group = "ML-TD",
         Participant_Count = 25) %>% 
  select(Code, CA_Months, Status, Lang_Group, Group, Participant_Count)

Participant_List <- rbind(Bilingual_DLD, Bilingual_TD, Monolingual_DLD, Monolingual_TD)


# Create summaries by structure
## Item #1
Verbal_Agreement_1 <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Verbal Agreement.csv")) %>% 
  select(Code, V1C, V1G, V1IF, V1M, V1T, V1N, V1P, V1Code, ...17) %>% 
  rename(Accuracy = V1C,
         Number_Accuracy = V1N,
         Person_Accuracy = V1P,
         Substitution = V1Code,
         Tense = V1T) %>% 
  inner_join(Participant_List, Verbal_Agreement_1, by = "Code") %>% 
  mutate(Gerund = coalesce(V1G, 0),
         Infinitive = coalesce(V1IF, 0),
         Stem = coalesce(...17, 0)) %>% 
  mutate(Nonfinite = Gerund + Infinitive) %>% 
  mutate(Item = "Verbal Agreement-1",
         Exp_Person = "First",
         Exp_Number = "Singular") %>% 
  select(Code, CA_Months, Group, Lang_Group, Status, Participant_Count, Accuracy, Number_Accuracy, Person_Accuracy, Nonfinite, Tense, Gerund, Infinitive, Stem, Item, Substitution, Exp_Person, Exp_Number)


## Item #3
Verbal_Agreement_3 <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Verbal Agreement.csv")) %>% 
  select(Code, V3C, V3G, V3IF, V3M, V3T, V3N, V3P, V3Code, ...53) %>% 
  rename(Accuracy = V3C,
         Number_Accuracy = V3N,
         Person_Accuracy = V3P,
         Substitution = V3Code,
         Tense = V3T) %>% 
  inner_join(Participant_List, Verbal_Agreement_3, by = "Code") %>% 
  mutate(Gerund = coalesce(V3G, 0),
         Infinitive = coalesce(V3IF, 0),
         Stem = coalesce(...53, 0)) %>% 
  mutate(Nonfinite = Gerund + Infinitive) %>% 
  mutate(Item = "Verbal Agreement-3",
         Exp_Person = "First",
         Exp_Number = "Plural") %>% 
  select(Code, CA_Months, Group, Lang_Group, Status, Participant_Count, Accuracy, Number_Accuracy, Person_Accuracy, Nonfinite, Tense, Gerund, Infinitive, Stem, Item, Substitution, Exp_Person, Exp_Number)


## Item #5
Verbal_Agreement_5 <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Verbal Agreement.csv")) %>% 
  select(Code, V5C, V5G, V5IF, V5M, V5T, V5N, V5P, V5Code, ...90) %>% 
  rename(Accuracy = V5C,
         Number_Accuracy = V5N,
         Person_Accuracy = V5P,
         Substitution = V5Code,
         Tense = V5T) %>% 
  inner_join(Participant_List, Verbal_Agreement_5, by = "Code") %>% 
  mutate(Gerund = coalesce(V5G, 0),
         Infinitive = coalesce(V5IF, 0),
         Stem = coalesce(...90, 0)) %>% 
  mutate(Nonfinite = Gerund + Infinitive) %>% 
  mutate(Item = "Verbal Agreement-5",
         Exp_Person = "First",
         Exp_Number = "Plural") %>% 
  select(Code, CA_Months, Group, Lang_Group, Status, Participant_Count, Accuracy, Number_Accuracy, Person_Accuracy, Nonfinite, Tense, Gerund, Infinitive, Stem, Item, Substitution, Exp_Person, Exp_Number)


## Item #7
Verbal_Agreement_7 <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Verbal Agreement.csv")) %>% 
  select(Code, V7C, V7G, V7IF, V7M, V7T, V7N, V7P, V7Code, ...127) %>% 
  rename(Accuracy = V7C,
         Number_Accuracy = V7N,
         Person_Accuracy = V7P,
         Substitution = V7Code,
         Tense = V7T) %>% 
  inner_join(Participant_List, Verbal_Agreement_7, by = "Code") %>% 
  mutate(Gerund = coalesce(V7G, 0),
         Infinitive = coalesce(V7IF, 0),
         Stem = coalesce(...127, 0)) %>% 
  mutate(Nonfinite = Gerund + Infinitive) %>% 
  mutate(Item = "Verbal Agreement-7",
         Exp_Person = "Third",
         Exp_Number = "Plural") %>% 
  select(Code, CA_Months, Group, Lang_Group, Status, Participant_Count, Accuracy, Number_Accuracy, Person_Accuracy, Nonfinite, Tense, Gerund, Infinitive, Stem, Item, Substitution, Exp_Person, Exp_Number)


## Item #8
Verbal_Agreement_8 <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Verbal Agreement.csv")) %>% 
  select(Code, V8C, V8G, V8IF, V8M, V8T, V8N, V8P, V8Code, ...144) %>% 
  rename(Accuracy = V8C,
         Number_Accuracy = V8N,
         Person_Accuracy = V8P,
         Substitution = V8Code,
         Tense = V8T) %>% 
  inner_join(Participant_List, Verbal_Agreement_8, by = "Code") %>% 
  mutate(Gerund = coalesce(V8G, 0),
         Infinitive = coalesce(V8IF, 0),
         Stem = coalesce(...144, 0)) %>% 
  mutate(Nonfinite = Gerund + Infinitive) %>% 
  mutate(Item = "Verbal Agreement-8",
         Exp_Person = "Second",
         Exp_Number = "Singular") %>% 
  select(Code, CA_Months, Group, Lang_Group, Status, Participant_Count, Accuracy, Number_Accuracy, Person_Accuracy, Nonfinite, Tense, Gerund, Infinitive, Stem, Item, Substitution, Exp_Person, Exp_Number)


## Item #9
Verbal_Agreement_9 <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Verbal Agreement.csv")) %>% 
  select(Code, V9C, `V9G/P`, V9IF, V9M, V9T, V9N, V9P, V9Code, ...161) %>% 
  rename(Accuracy = V9C,
         Number_Accuracy = V9N,
         Person_Accuracy = V9P,
         Substitution = V9Code,
         Tense = V9T) %>% 
  inner_join(Participant_List, Verbal_Agreement_9, by = "Code") %>% 
  mutate(Gerund = coalesce(`V9G/P`, 0),
         Infinitive = coalesce(V9IF, 0),
         Stem = coalesce(...161, 0)) %>% 
  mutate(Nonfinite = Gerund + Infinitive) %>% 
  mutate(Item = "Verbal Agreement-9",
         Exp_Person = "Third",
         Exp_Number = "Singular") %>% 
  select(Code, CA_Months, Group, Lang_Group, Status, Participant_Count, Accuracy, Number_Accuracy, Person_Accuracy, Nonfinite, Tense, Gerund, Infinitive, Stem, Item, Substitution, Exp_Person, Exp_Number)


## Item #10
Verbal_Agreement_10 <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Verbal Agreement.csv")) %>% 
  select(Code, V10C, V10G, V10IF, V10M, V10T, V10N, V10P, V10Code, ...177) %>% 
  rename(Accuracy = V10C,
         Number_Accuracy = V10N,
         Person_Accuracy = V10P,
         Substitution = V10Code,
         Tense = V10T) %>% 
  inner_join(Participant_List, Verbal_Agreement_10, by = "Code") %>% 
  mutate(Gerund = coalesce(V10G, 0),
         Infinitive = coalesce(V10IF, 0),
         Stem = coalesce(...177, 0)) %>% 
  mutate(Nonfinite = Gerund + Infinitive) %>% 
  mutate(Item = "Verbal Agreement-10",
         Exp_Person = "Third",
         Exp_Number = "Singular") %>% 
  select(Code, CA_Months, Group, Lang_Group, Status, Participant_Count, Accuracy, Number_Accuracy, Person_Accuracy, Nonfinite, Tense, Gerund, Infinitive, Stem, Item, Substitution, Exp_Person, Exp_Number)


# Join datasets
Verbal_Agreement_Long <- rbind(Verbal_Agreement_1, Verbal_Agreement_3, Verbal_Agreement_5, Verbal_Agreement_7, Verbal_Agreement_8, Verbal_Agreement_9, Verbal_Agreement_10)
Verbal_Agreement_Long$Lang_Group <- factor(Verbal_Agreement_Long$Lang_Group, levels = c("Monolingual", "Bilingual"))
Verbal_Agreement_Long$Status <- factor(Verbal_Agreement_Long$Status, levels = c("TD", "DLD"))
Verbal_Agreement_Long$Group <- factor(Verbal_Agreement_Long$Group, levels = c("ML-TD", "ML-DLD", "BL-TD", "BL-DLD"))


## Order levels
Verbal_Agreement_Long$Gerund <- as.numeric(Verbal_Agreement_Long$Gerund)
Verbal_Agreement_Long$Infinitive <- as.numeric(Verbal_Agreement_Long$Infinitive)
Verbal_Agreement_Long$Stem <- as.numeric(Verbal_Agreement_Long$Stem)
Verbal_Agreement_Long$Tense <- as.numeric(Verbal_Agreement_Long$Tense)
Verbal_Agreement_Long$Number_Accuracy <- as.numeric(Verbal_Agreement_Long$Number_Accuracy)
Verbal_Agreement_Long$Person_Accuracy <- as.numeric(Verbal_Agreement_Long$Person_Accuracy)
Verbal_Agreement_Long$Nonfinite <- as.numeric(Verbal_Agreement_Long$Nonfinite)


## Write CSV File
Verbal_Agreement_Final <- Verbal_Agreement_Long %>% 
  mutate(CA_Months_Ctd = CA_Months - mean(CA_Months, na.rm = TRUE)) %>% 
  mutate(Substitution = ifelse(Substitution == 1, 0, Substitution)) %>% 
  mutate(Substitution = ifelse(is.na(Substitution) | Substitution == 1, 0, Substitution)) %>% 
  write_csv(., here("JCL Verbal Manuscript", "JCL Verbal Agreement Analysis Data.csv"))
