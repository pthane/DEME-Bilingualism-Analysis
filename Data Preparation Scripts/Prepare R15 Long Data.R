library(here)
library(tidyverse)


# Prepare list of participants
## Load initial files
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


# Join
Participant_List <- rbind(Bilingual_DLD, Bilingual_TD, Monolingual_DLD, Monolingual_TD)


# Prepare Linguistic Data
## Load CSVs by structure
Articles <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Noun Phrase Agreement.csv")) %>% 
  select(Code, Version, ...31, ...64, ...96, ...129, ...162, ...195, ...294, ...359) %>% 
  rename(Article_1 = ...31, Article_2 = ...64, Article_3 = ...96, Article_4 = ...129, Article_5 = ...162, Article_6 = ...195, Article_10 = ...294, Article_12 = ...359) %>% 
  pivot_longer(., cols = c(Article_1, Article_2, Article_3, Article_4, Article_5, Article_6, Article_10, Article_12),
               names_to = "Item") %>% 
  rename(Accuracy = value) %>% 
  mutate(Structure = "Articles",
         Domain = "Nominal")

Clitics <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Clitics.csv")) %>% 
  select(Code, Version, Cl1C, Cl2C, Cl4C...245, Cl5C...230, Cl6C, Cl7C, Cl8C...215, Cl9C, Cl10C, Cl11C) %>% 
  rename(Clitic_1 = Cl1C, Clitic_2 = Cl2C, Clitic_4 = Cl4C...245, Clitic_5 = Cl5C...230, Clitic_6 = Cl6C, Clitic_7 = Cl7C, Clitic_8 = Cl8C...215, Clitic_9 = Cl9C, Clitic_10 = Cl10C, Clitic_11 = Cl11C) %>% 
  pivot_longer(., cols = c(Clitic_1, Clitic_2, Clitic_4, Clitic_5, Clitic_6, Clitic_7, Clitic_8, Clitic_9, Clitic_10, Clitic_11),
               names_to = "Item") %>% 
  rename(Accuracy = value) %>% 
  mutate(Structure = "Clitics",
         Domain = "Nominal")

Verbal_Agreement <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Verbal Agreement.csv")) %>% 
  select(Code, Version, V1C, V3C, V5C, V7C, V8C, V9C, V10C) %>%
  rename(Verbal_Agreement_1 = V1C, Verbal_Agreement_3 = V3C, Verbal_Agreement_5 = V5C, Verbal_Agreement_7 = V7C, Verbal_Agreement_8 = V8C, Verbal_Agreement_9 = V9C, Verbal_Agreement_10 = V10C) %>% 
  pivot_longer(., cols = c(Verbal_Agreement_1, Verbal_Agreement_3, Verbal_Agreement_5, Verbal_Agreement_7, Verbal_Agreement_8, Verbal_Agreement_9, Verbal_Agreement_10),
               names_to = "Item") %>% 
  rename(Accuracy = value) %>% 
  mutate(Structure = "Verbal Agreement",
         Domain = "Verbal")

Subjunctive <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Subjunctive.csv")) %>% 
  select(Code, Version, S1C, S2C, S3C, S4C, S5C) %>%
  rename(Subjunctive_1 = S1C, Subjunctive_2 = S2C, Subjunctive_3 = S3C, Subjunctive_4 = S4C, Subjunctive_5 = S5C) %>% 
  pivot_longer(., cols = c(Subjunctive_1, Subjunctive_2, Subjunctive_3, Subjunctive_4, Subjunctive_5),
               names_to = "Item") %>% 
  rename(Accuracy = value) %>% 
  mutate(Structure = "Subjunctive",
         Domain = "Verbal")


## Join CSVs
Joint_Structures <- rbind(Articles, Clitics, Subjunctive, Verbal_Agreement)
Accuracy <- left_join(Participant_List, Joint_Structures, by = "Code")


# Add participant background data
## Load background data
Background_Data <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Raw Performance Data.csv"))


## Bilingual data
Bilingual_Aggregate <- left_join(Accuracy, Background_Data, by = "Code") %>% 
  filter(Lang_Group == "Bilingual") %>% 
  mutate(Relative_Morphosyntax = BESA_S_s - BESA_E_s,
         Relative_PGU = PGU_Spanish - PGU_English,
         Relative_Words = Words_Spanish - Words_English,
         Home_Exposure = PQ_LANG_MOM_CH, PQ_LANG_DAD_CH) %>% 
  mutate(Relative_Morphosyntax_Std = (Relative_Morphosyntax - mean(Relative_Morphosyntax))/sd(Relative_Morphosyntax),
         Relative_PGU_Std = (Relative_PGU - mean(Relative_PGU))/sd(Relative_PGU),
         Home_Exposure_Std = (Home_Exposure - mean(Home_Exposure))/sd(Home_Exposure))

## Monolingual data
Monolingual_Aggregate <- left_join(Accuracy, Background_Data, by = "Code") %>% 
  filter(Lang_Group == "Monolingual")

Monolingual_Aggregate[, "Relative_Morphosyntax"] = NA
Monolingual_Aggregate[, "Relative_PGU"] = NA
Monolingual_Aggregate[, "Relative_Words"] = NA
Monolingual_Aggregate[, "Home_Exposure"] = NA
Monolingual_Aggregate[, "Relative_Morphosyntax_Std"] = NA
Monolingual_Aggregate[, "Relative_PGU_Std"] = NA
Monolingual_Aggregate[, "Home_Exposure_Std"] = NA


# Create joined dataset
Master <- rbind(Bilingual_Aggregate, Monolingual_Aggregate) %>% 
  mutate(CA_Months_Std = (CA_Months - mean(CA_Months))/sd(CA_Months)) %>% 
  mutate(CA_Months_Ctd = (CA_Months - mean(CA_Months))) %>% 
  write_csv(here("CSV Files", "Aggregated Long Data.csv"))


# Confirm all participants are included
Confirm_ML_TD <- Master %>% 
  filter(Group == "ML-TD")
unique(Confirm_ML_TD$Code)

Confirm_ML_DLD <- Master %>% 
  filter(Group == "ML-DLD")
unique(Confirm_ML_DLD$Code)

Confirm_BL_TD <- Master %>% 
  filter(Group == "BL-TD")
unique(Confirm_BL_TD$Code)

Confirm_BL_DLD <- Master %>% 
  filter(Group == "BL-DLD")
unique(Confirm_BL_DLD$Code)

unique(Master$Code)