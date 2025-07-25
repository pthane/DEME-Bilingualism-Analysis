library(tidyverse)
library(lmerTest)
library(lme4)
library(broom.mixed)
library(here)
library(performance)

options(scipen = 99)


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


# Item #1
Articles_1 <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Noun Phrase Agreement.csv")) %>% 
  select(Code, ...31, NP1N, NP1DO, `NP1Cli GD-D`, `NP1Cli ND-D`, `NPCliG+N D`, `NP1DN-GD`, `NP1DN-ND`, `NP1DNA GD-D`) %>% 
  inner_join(Participant_List, Articles_1, by = "Code") %>% 
  rename(Accuracy = ...31) %>% 
  mutate(Omission = coalesce(NP1N, 0) + coalesce(NP1DO, 0),
         Gender = coalesce(`NP1Cli GD-D`, 0) + coalesce(`NP1DN-GD`, 0),
         Number = coalesce(`NP1Cli ND-D`, 0) + coalesce(`NP1DN-ND`, 0),
         Double = coalesce(`NPCliG+N D`, 0) + coalesce(`NP1DNA GD-D`, 0)) %>% 
  mutate(Check_Score_1 = Accuracy + Omission + Gender + Number + Double) %>% 
  mutate(Substitution = Gender + Number + Double,
         Item = "Articles-1") %>% 
  mutate(Check_Score_2 = Accuracy + Omission + Substitution) %>% 
  select(Code, CA_Months, Status, Lang_Group, Group, Participant_Count, Item, Accuracy, Substitution, Omission, Gender, Number, Double, Check_Score_1, Check_Score_2)


# Item #2
Articles_2 <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Noun Phrase Agreement.csv")) %>% 
  select(Code, ...64, NP2N, NP2DO, `NP2Cli GD-D`, `NP2Cli ND-D`, `NP2CliG+N D`, `NP2DN-GD`, `NP2DN-ND`, `NP2DNA GD-D`) %>% 
  inner_join(Participant_List, Articles_, by = "Code") %>% 
  rename(Accuracy = ...64) %>% 
  mutate(Omission = coalesce(NP2N, 0) + coalesce(NP2DO, 0),
         Gender = coalesce(`NP2Cli GD-D`, 0) + coalesce(`NP2DN-GD`, 0),
         Number = coalesce(`NP2Cli ND-D`, 0) + coalesce(`NP2DN-ND`, 0),
         Double = coalesce(`NP2CliG+N D`, 0) + coalesce(`NP2DNA GD-D`, 0)) %>% 
  mutate(Check_Score_1 = Accuracy + Omission + Gender + Number + Double) %>% 
  mutate(Substitution = Gender + Number + Double,
         Item = "Articles-2") %>% 
  mutate(Check_Score_2 = Accuracy + Omission + Substitution) %>% 
  select(Code, CA_Months, Status, Lang_Group, Group, Participant_Count, Item, Accuracy, Substitution, Omission, Gender, Number, Double, Check_Score_1, Check_Score_2)


# Item #3
Articles_3 <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Noun Phrase Agreement.csv")) %>% 
  select(Code, ...96, NP3N, NP3DO, `NP3Cli GD-D`, `NP3Cli ND-D`, `NP3CliG+N D`, `NP3DN-GD`, `NP3DN-ND`, `NP3DNA GD-D`) %>% 
  inner_join(Participant_List, Articles_3, by = "Code") %>% 
  rename(Accuracy = ...96) %>% 
  mutate(Omission = coalesce(NP3N, 0) + coalesce(NP3DO, 0),
         Gender = coalesce(`NP3Cli GD-D`, 0) + coalesce(`NP3DN-GD`, 0),
         Number = coalesce(`NP3Cli ND-D`, 0) + coalesce(`NP3DN-ND`, 0),
         Double = coalesce(`NP3CliG+N D`, 0) + coalesce(`NP3DNA GD-D`, 0)) %>% 
  mutate(Check_Score_1 = Accuracy + Omission + Gender + Number + Double) %>% 
  mutate(Substitution = Gender + Number + Double,
         Item = "Articles-3") %>% 
  mutate(Check_Score_2 = Accuracy + Omission + Substitution) %>% 
  select(Code, CA_Months, Status, Lang_Group, Group, Participant_Count, Item, Accuracy, Substitution, Omission, Gender, Number, Double, Check_Score_1, Check_Score_2)



# Item #4
Articles_4 <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Noun Phrase Agreement.csv")) %>% 
  select(Code, ...129, NP4N, NP4DO, `NP4Cli GD-D`, `NP4Cli ND-D`, `NP4CliG+N D`, `NP4DN-GD`, `NP4DN-ND`, `NP4DNA GD-D`) %>% 
  inner_join(Participant_List, Articles_4, by = "Code") %>% 
  rename(Accuracy = ...129) %>% 
  mutate(Omission = coalesce(NP4N, 0) + coalesce(NP4DO, 0),
         Gender = coalesce(`NP4Cli GD-D`, 0) + coalesce(`NP4DN-GD`, 0),
         Number = coalesce(`NP4Cli ND-D`, 0) + coalesce(`NP4DN-ND`, 0),
         Double = coalesce(`NP4CliG+N D`, 0) + coalesce(`NP4DNA GD-D`, 0)) %>% 
  mutate(Check_Score_1 = Accuracy + Omission + Gender + Number + Double) %>% 
  mutate(Substitution = Gender + Number + Double,
         Item = "Articles-4") %>% 
  mutate(Check_Score_2 = Accuracy + Omission + Substitution) %>% 
  select(Code, CA_Months, Status, Lang_Group, Group, Participant_Count, Item, Accuracy, Substitution, Omission, Gender, Number, Double, Check_Score_1, Check_Score_2)


# Item #5
Articles_5 <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Noun Phrase Agreement.csv")) %>% 
  select(Code, ...162, NP5N, NP5DO, `NP5Cli GD-D`, `NP5Cli ND-D`, `NP5CliG+N D`, `NP5DN-GD`, `NP5DN-ND`, `NP5DNA GD-D`) %>% 
  inner_join(Participant_List, Articles_5, by = "Code") %>% 
  rename(Accuracy = ...162) %>% 
  mutate(Omission = coalesce(NP5N, 0) + coalesce(NP5DO, 0),
         Gender = coalesce(`NP5Cli GD-D`, 0) + coalesce(`NP5DN-GD`, 0),
         Number = coalesce(`NP5Cli ND-D`, 0) + coalesce(`NP5DN-ND`, 0),
         Double = coalesce(`NP5CliG+N D`, 0) + coalesce(`NP5DNA GD-D`, 0)) %>% 
  mutate(Check_Score_1 = Accuracy + Omission + Gender + Number + Double) %>% 
  mutate(Substitution = Gender + Number + Double,
         Item = "Articles-5") %>% 
  mutate(Check_Score_2 = Accuracy + Omission + Substitution) %>% 
  select(Code, CA_Months, Status, Lang_Group, Group, Participant_Count, Item, Accuracy, Substitution, Omission, Gender, Number, Double, Check_Score_1, Check_Score_2)


# Item #6
Articles_6 <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Noun Phrase Agreement.csv")) %>% 
  select(Code, ...195, NP6N, NP6DO, `NP6Cli GD-D`, `NP6Cli ND-D`, `NP6CliG+N D`, `NP6DN-GD`, `NP6DN-ND`, `NP6DNA GD-D`) %>% 
  inner_join(Participant_List, Articles_6, by = "Code") %>% 
  rename(Accuracy = ...195) %>% 
  mutate(Omission = coalesce(NP6N, 0) + coalesce(NP6DO, 0),
         Gender = coalesce(`NP6Cli GD-D`, 0) + coalesce(`NP6DN-GD`, 0),
         Number = coalesce(`NP6Cli ND-D`, 0) + coalesce(`NP6DN-ND`, 0),
         Double = coalesce(`NP6CliG+N D`, 0) + coalesce(`NP6DNA GD-D`, 0)) %>% 
  mutate(Check_Score_1 = Accuracy + Omission + Gender + Number + Double) %>% 
  mutate(Substitution = Gender + Number + Double,
         Item = "Articles-6") %>% 
  mutate(Check_Score_2 = Accuracy + Omission + Substitution) %>% 
  select(Code, CA_Months, Status, Lang_Group, Group, Participant_Count, Item, Accuracy, Substitution, Omission, Gender, Number, Double, Check_Score_1, Check_Score_2)


# Item #10
Articles_10 <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Noun Phrase Agreement.csv")) %>% 
  select(Code, ...294, NP10N, NP10DO, `NP10Cli GD-D`, `NP10Cli ND-D`, `NP10CliG+N D`, `NP10DN-GD`, `NP10DN-ND`, `NP10DNA GD-D`) %>% 
  inner_join(Participant_List, Articles_10, by = "Code") %>% 
  rename(Accuracy = ...294) %>%
  mutate(Omission = coalesce(NP10N, 0) + coalesce(NP10DO, 0),
         Gender = coalesce(`NP10Cli GD-D`, 0) + coalesce(`NP10DN-GD`, 0),
         Number = coalesce(`NP10Cli ND-D`, 0) + coalesce(`NP10DN-ND`, 0),
         Double = coalesce(`NP10CliG+N D`, 0) + coalesce(`NP10DNA GD-D`, 0)) %>% 
  mutate(Check_Score_1 = Accuracy + Omission + Gender + Number + Double) %>% 
  mutate(Substitution = Gender + Number + Double,
         Item = "Articles-10") %>% 
  mutate(Check_Score_2 = Accuracy + Omission + Substitution) %>% 
  select(Code, CA_Months, Status, Lang_Group, Group, Participant_Count, Item, Accuracy, Substitution, Omission, Gender, Number, Double, Check_Score_1, Check_Score_2)


# Item #12
Articles_12 <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Noun Phrase Agreement.csv")) %>% 
  select(Code, ...359, NP12N...341, NP12DO, `NP12Cli GD-D`, `NP12Cli ND-D`, `NP12CliG+N D`, `NP12DN-GD`, `NP12DN-ND`, `NP12DNA GD-D`) %>% 
  inner_join(Participant_List, Articles_12, by = "Code") %>% 
  rename(Accuracy = ...359) %>% 
  mutate(Omission = coalesce(NP12N...341, 0) + coalesce(NP12DO, 0),
         Gender = coalesce(`NP12Cli GD-D`, 0) + coalesce(`NP12DN-GD`, 0),
         Number = coalesce(`NP12Cli ND-D`, 0) + coalesce(`NP12DN-ND`, 0),
         Double = coalesce(`NP12CliG+N D`, 0) + coalesce(`NP12DNA GD-D`, 0)) %>% 
  mutate(Check_Score_1 = Accuracy + Omission + Gender + Number + Double) %>% 
  mutate(Substitution = Gender + Number + Double,
         Item = "Articles-12") %>% 
  mutate(Check_Score_2 = Accuracy + Omission + Substitution) %>% 
  select(Code, CA_Months, Status, Lang_Group, Group, Participant_Count, Item, Accuracy, Substitution, Omission, Gender, Number, Double, Check_Score_1, Check_Score_2)


# Prepare data structure
## Join dataframes together
Articles_Long <- rbind(Articles_1, Articles_2, Articles_3, Articles_4, Articles_5, Articles_6, Articles_10, Articles_12) %>% 
  write_csv(here("JCL Nominal Manuscript", "Results", "Articles Response Table.csv"))


## Order levels
Articles_Long$Omission <- as.numeric(Articles_Long$Omission)
Articles_Long$Substitution <- as.numeric(Articles_Long$Substitution)
Articles_Long$Gender <- as.numeric(Articles_Long$Gender)
Articles_Long$Number <- as.numeric(Articles_Long$Number)
Articles_Long$Double <- as.numeric(Articles_Long$Double)


## Articles
Articles_Non_Target_Responses <- Articles_Long %>% 
  filter(Accuracy == 0) %>% 
  filter(rowSums(select(., Substitution, Omission) == 1) == 1) %>% 
  mutate(Check_Score_3 = Substitution + Omission) %>% 
  write_csv(here("JCL Nominal Manuscript", "Results", "Non-Target Article Responses.csv"))

Articles_Non_Target_Responses$Status <- factor(Articles_Non_Target_Responses$Status, levels = c("TD", "DLD"))
Articles_Non_Target_Responses$Lang_Group <- factor(Articles_Non_Target_Responses$Lang_Group, levels = c("Monolingual", "Bilingual"))
Articles_Non_Target_Responses$Group <- factor(Articles_Non_Target_Responses$Group, levels = c("ML-TD", "ML-DLD", "BL-TD", "BL-DLD"))


# Create correlations
## TD vs. DLD
Articles_Status <- glmer(Substitution ~ Status +
                                         (1 | Code),
                                       family = "binomial",
                                       data = Articles_Non_Target_Responses)

summary(Articles_Status)

Articles_Status_Table <- tidy(Articles_Status, effects = "fixed") %>% 
  mutate(Set = "Articles",
         Contrast = "TD/DLD")


## Bilingual versus monolingual
Articles_Lang_Group <- glmer(Substitution ~ Lang_Group +
                           (1 | Code),
                         family = "binomial",
                         data = Articles_Non_Target_Responses)

summary(Articles_Lang_Group)

Articles_Lang_Group_Table <- tidy(Articles_Lang_Group, effects = "fixed") %>% 
  mutate(Set = "Articles",
         Contrast = "BL/ML")


## Join summary tables
Articles_Results_Table <- rbind(Articles_Status_Table, Articles_Lang_Group_Table) %>% 
  rename(Effect = term,
         Estimate = estimate,
         SE = std.error,
         p = p.value) %>% 
  select(Effect, Estimate, SE, p, Set, Contrast) %>% 
  mutate(Estimate = round(Estimate, 2),
         SE = round(SE, 2),
         p = round(p, 3)) %>% 
  write_csv(here("JCL Nominal Manuscript", "Results", "JCL Nominal Articles Pattern GLMMs.csv"))


# Prepare graph
## Adjust counts by participant
Articles_Adjusted_Counts <- Articles_Long %>% 
  group_by(Status, Lang_Group, Group, Participant_Count) %>% 
  summarize(Omission = sum(Omission),
            Gender = sum(Gender),
            Number = sum(Number),
            Double = sum(Double)) %>% 
  pivot_longer(cols = c(Omission, Gender, Number, Double), 
               names_to = "Type", 
               values_to = "Count") %>% 
  mutate(Count_Adjusted = Count/Participant_Count)

Articles_Adjusted_Counts$Status <- factor(Articles_Adjusted_Counts$Status, levels = c("DLD", "TD"))
Articles_Adjusted_Counts$Lang_Group <- factor(Articles_Adjusted_Counts$Lang_Group, levels = c("Bilingual", "Monolingual"))


## Create bar graph
Articles_Distribution_Plot <- Articles_Adjusted_Counts %>% 
  ggplot(aes(x = Type, y = Count_Adjusted, fill = Status)) + 
  facet_grid(rows = vars(Lang_Group)) +
  geom_bar(position = "dodge", color = "black", stat = "identity") +
  scale_fill_manual(values = c("#BF5700", "#F4EFE0")) +
  scale_y_continuous(breaks = seq(0, 5, 1),
                     limits = c(0, 5)) +
  geom_text(aes(label = round(Count_Adjusted, 2)),
            fontface = "bold",
            position = position_dodge(width = .9),
            vjust = -0.5,
            size = 3) +
  scale_x_discrete(labels = c("Gender substitution", "Number substitution", "Gender/number substitution", "Omission")) +
  labs(x = "Type of non-target response",
       y = "Non-target responses per participant",
       title = "Distribution of Non-Target Article Responses by Group",
       fill = "Ability") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"))

Articles_Distribution_Plot

ggsave(filename = here("JCL Nominal Manuscript", "Graphs", "JCL Nominal Figure 2.pdf"),
       plot = Articles_Distribution_Plot,
       device = "pdf",
       width = 9,
       height = 3,
       units = "in")
