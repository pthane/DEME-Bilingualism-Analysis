library(tidyverse)
library(lmerTest)
library(broom.mixed)
library(openxlsx)
library(lme4)
library(here)

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
Clitics_1 <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Clitics.csv")) %>% 
  select(Code, Cl1C, `Cl1GD-D`, `Cl1ND-D`, `Cl1ND-A`, CL1IN, CL1G, CL1R, Cl1NP, Cl1O, Cl1IP) %>% 
  inner_join(Participant_List, Clitics_1, by = "Code") %>% 
  rename(Accuracy = Cl1C) %>% 
  mutate(Gender = coalesce(`Cl1GD-D`, 0),
         Number = coalesce(`Cl1ND-D`, 0),
         Double = coalesce(`Cl1ND-A`, 0),
         Lexical_DP = coalesce(Cl1NP, 0),
         Other_Pronoun = coalesce(CL1R, 0) + coalesce(Cl1IP, 0)) %>%
  mutate(Omission = coalesce(CL1IN, 0) + coalesce(CL1G, 0) + coalesce(Cl1O, 0),
         Clitic_Substitution = Gender + Number + Double + Other_Pronoun,
         Item = "Clitics-1") %>% 
  select(Code, CA_Months, Status, Lang_Group, Group, Participant_Count, Item, Accuracy, Omission, Clitic_Substitution, Gender, Number, Double, Lexical_DP, Other_Pronoun)

# Item #2
Clitics_2 <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Clitics.csv")) %>% 
  select(Code, Cl2C, `Cl2GD-D`, `Cl2ND-D`, `Cl2ND-V`, CL2IN, CL2G, CL2R, Cl2NP, Cl2O, Cl2UN, Cl2IP) %>% 
  inner_join(Participant_List, Clitics_2, by = "Code") %>% 
  rename(Accuracy = Cl2C) %>% 
  mutate(Gender = coalesce(`Cl2GD-D`, 0),
         Number = coalesce(`Cl2ND-D`, 0),
         Double = coalesce(`Cl2ND-V`, 0),
         Lexical_DP = coalesce(Cl2NP, 0),
         Other_Pronoun = coalesce(CL2R, 0) + coalesce(Cl2IP, 0)) %>%
  mutate(Omission = coalesce(CL2IN, 0) + coalesce(CL2G, 0) + coalesce(Cl2O, 0),
         Clitic_Substitution = Gender + Number + Double + Other_Pronoun,
         Item = "Clitics-2") %>% 
  select(Code, CA_Months, Status, Lang_Group, Group, Participant_Count, Item, Accuracy, Omission, Clitic_Substitution, Gender, Number, Double, Lexical_DP, Other_Pronoun)


# Item #4
Clitics_4 <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Clitics.csv")) %>% 
  select(Code, Cl4C...245, `Cl4GD-D...249`, `Cl4ND-D...250`, `Cl4ND-V...251`, CL4IN...252, CL4G...253, CL4R...254, Cl4NP...255, Cl4O...256, Cl4UN...257, Cl4IP...258) %>% 
  inner_join(Participant_List, Clitics_4, by = "Code") %>% 
  rename(Accuracy = Cl4C...245) %>% 
  mutate(Gender = coalesce(`Cl4GD-D...249`, 0),
         Number = coalesce(`Cl4ND-D...250`, 0),
         Double = coalesce(`Cl4ND-V...251`, 0),
         Lexical_DP = coalesce(Cl4NP...255, 0),
         Other_Pronoun = coalesce(CL4R...254, 0) + coalesce(Cl4IP...258, 0)) %>%
  mutate(Omission = coalesce(CL4IN...252, 0) + coalesce(CL4G...253, 0) + coalesce(Cl4O...256, 0),
         Clitic_Substitution = Gender + Number + Double + Other_Pronoun,
         Item = "Clitics-4") %>% 
  select(Code, CA_Months, Status, Lang_Group, Group, Participant_Count, Item, Accuracy, Omission, Clitic_Substitution, Gender, Number, Double, Lexical_DP, Other_Pronoun)


# Item #5
Clitics_5 <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Clitics.csv")) %>% 
  select(Code, Cl5C...230, `Cl5GD-D...234`, `Cl5ND-D...235`, `Cl5ND-V...236`, CL5IN...237, CL5G...238, CL5R...239, Cl5NP...240, Cl5O...241, Cl5UN...242, Cl5IP...243) %>% 
  inner_join(Participant_List, Clitics_5, by = "Code") %>% 
  rename(Accuracy = Cl5C...230) %>% 
  mutate(Gender = coalesce(`Cl5GD-D...234`, 0),
         Number = coalesce(`Cl5ND-D...235`, 0),
         Double = coalesce(`Cl5ND-V...236`, 0),
         Lexical_DP = coalesce(Cl5NP...240, 0),
         Other_Pronoun = coalesce(CL5R...239, 0) + coalesce(Cl5IP...243, 0)) %>%
  mutate(Omission = coalesce(CL5IN...237, 0) + coalesce(CL5G...238, 0) + coalesce(Cl5O...241, 0),
         Clitic_Substitution = Gender + Number + Double + Other_Pronoun,
         Item = "Clitics-5") %>% 
  select(Code, CA_Months, Status, Lang_Group, Group, Participant_Count, Item, Accuracy, Omission, Clitic_Substitution, Gender, Number, Double, Lexical_DP, Other_Pronoun)


# Item #6
Clitics_6 <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Clitics.csv")) %>% 
  select(Code, Cl6C, `Cl6GD-D`, `Cl6ND-D`, `Cl6ND-V`, CL6IN, CL6G, CL6R, Cl6NP, Cl6O, Cl6UN, Cl6IP) %>% 
  inner_join(Participant_List, Clitics_6, by = "Code") %>% 
  rename(Accuracy = Cl6C) %>% 
  mutate(Gender = coalesce(`Cl6GD-D`, 0),
         Number = coalesce(`Cl6ND-D`, 0),
         Double = coalesce(`Cl6ND-V`, 0),
         Lexical_DP = coalesce(Cl6NP, 0),
         Other_Pronoun = coalesce(CL6R, 0) + coalesce(Cl6IP, 0)) %>%
  mutate(Omission = coalesce(CL6IN, 0) + coalesce(CL6G, 0) + coalesce(Cl6O, 0),
         Clitic_Substitution = Gender + Number + Double + Other_Pronoun,
         Item = "Clitics-6") %>% 
  select(Code, CA_Months, Status, Lang_Group, Group, Participant_Count, Item, Accuracy, Omission, Clitic_Substitution, Gender, Number, Double, Lexical_DP, Other_Pronoun)


# Item #7
Clitics_7 <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Clitics.csv")) %>% 
  select(Code, Cl7C, `Cl7GD-D`, `Cl7ND-D`, `Cl7ND-V`, CL7IN, CL7G, CL7R, Cl7NP, Cl7O, Cl7UN, Cl7IP) %>% 
  inner_join(Participant_List, Clitics_7, by = "Code") %>% 
  rename(Accuracy = Cl7C) %>% 
  mutate(Gender = coalesce(`Cl7GD-D`, 0),
         Number = coalesce(`Cl7ND-D`, 0),
         Double = coalesce(`Cl7ND-V`, 0),
         Lexical_DP = coalesce(Cl7NP, 0),
         Other_Pronoun = coalesce(CL7R, 0) + coalesce(Cl7IP, 0)) %>%
  mutate(Omission = coalesce(CL7IN, 0) + coalesce(CL7G, 0) + coalesce(Cl7O, 0),
         Clitic_Substitution = Gender + Number + Double + Other_Pronoun,
         Item = "Clitics-7") %>% 
  select(Code, CA_Months, Status, Lang_Group, Group, Participant_Count, Item, Accuracy, Omission, Clitic_Substitution, Gender, Number, Double, Lexical_DP, Other_Pronoun)



# Item #8
Clitics_8 <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Clitics.csv")) %>% 
  select(Code, Cl8C...215, `Cl8GD-D...219`, `Cl8ND-D...220`, `Cl8ND-V...221`, CL8IN...222, CL8G...223, CL8R...224, Cl8NP...225, Cl8O...226, Cl8UN...227, Cl8IP...228) %>% 
  inner_join(Participant_List, Clitics_8, by = "Code") %>% 
  rename(Accuracy = Cl8C...215) %>% 
  mutate(Gender = coalesce(`Cl8GD-D...219`, 0),
         Number = coalesce(`Cl8ND-D...220`, 0),
         Double = coalesce(`Cl8ND-V...221`, 0),
         Lexical_DP = coalesce(Cl8NP...225, 0),
         Other_Pronoun = coalesce(CL8R...224, 0) + coalesce(Cl8IP...228, 0)) %>%
  mutate(Omission = coalesce(CL8IN...222, 0) + coalesce(CL8G...223, 0) + coalesce(Cl8O...226, 0),
         Clitic_Substitution = Gender + Number + Double + Other_Pronoun,
         Item = "Clitics-8") %>% 
  select(Code, CA_Months, Status, Lang_Group, Group, Participant_Count, Item, Accuracy, Omission, Clitic_Substitution, Gender, Number, Double, Lexical_DP, Other_Pronoun)


# Item #9
Clitics_9 <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Clitics.csv")) %>% 
  select(Code, Cl9C, `Cl9GD-D`, `Cl9ND-D`, `Cl9ND-V`, CL9IN, CL9G, CL9R, Cl9NP, Cl9O, Cl9UN, Cl9IP) %>% 
  inner_join(Participant_List, Clitics_9, by = "Code") %>% 
  rename(Accuracy = Cl9C) %>% 
  mutate(Gender = coalesce(`Cl9GD-D`, 0),
         Number = coalesce(`Cl9ND-D`, 0),
         Double = coalesce(`Cl9ND-V`, 0),
         Lexical_DP = coalesce(Cl9NP, 0),
         Other_Pronoun = coalesce(CL9R, 0) + coalesce(Cl9IP, 0)) %>%
  mutate(Omission = coalesce(CL9IN, 0) + coalesce(CL9G, 0) + coalesce(Cl9O, 0),
         Clitic_Substitution = Gender + Number + Double + Other_Pronoun,
         Item = "Clitics-9") %>% 
  select(Code, CA_Months, Status, Lang_Group, Group, Participant_Count, Item, Accuracy, Omission, Clitic_Substitution, Gender, Number, Double, Lexical_DP, Other_Pronoun)


# Item #10
Clitics_10 <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Clitics.csv")) %>% 
  select(Code, Cl10C, `Cl10GD-D`, `Cl10ND-D`, `Cl10ND-V`, CL10IN, CL10G, CL10R, Cl10NP, Cl10O, Cl10UN, Cl10IP) %>% 
  inner_join(Participant_List, Clitics_10, by = "Code") %>% 
  rename(Accuracy = Cl10C) %>% 
  mutate(Gender = coalesce(`Cl10GD-D`, 0),
         Number = coalesce(`Cl10ND-D`, 0),
         Double = coalesce(`Cl10ND-V`, 0),
         Lexical_DP = coalesce(Cl10NP, 0),
         Other_Pronoun = coalesce(CL10R, 0) + coalesce(Cl10IP, 0)) %>%
  mutate(Omission = coalesce(CL10IN, 0) + coalesce(CL10G, 0) + coalesce(Cl10O, 0),
         Clitic_Substitution = Gender + Number + Double + Other_Pronoun,
         Item = "Clitics-10") %>% 
  select(Code, CA_Months, Status, Lang_Group, Group, Participant_Count, Item, Accuracy, Omission, Clitic_Substitution, Gender, Number, Double, Lexical_DP, Other_Pronoun)


# Item #11
Clitics_11 <- read_csv(here("CSV Files", "Anonymized Files", "Anonymized Clitics.csv")) %>% 
  select(Code, Cl11C, `Cl11GD-D`, `Cl11ND-D`, `Cl11ND-V`, CL11IN, CL11G, CL11R, Cl11NP, Cl11O, Cl11UN, Cl11IP) %>% 
  inner_join(Participant_List, Clitics_11, by = "Code") %>% 
  rename(Accuracy = Cl11C) %>% 
  mutate(Gender = coalesce(`Cl11GD-D`, 0),
         Number = coalesce(`Cl11ND-D`, 0),
         Double = coalesce(`Cl11ND-V`, 0),
         Lexical_DP = coalesce(Cl11NP, 0),
         Other_Pronoun = coalesce(CL11R, 0) + coalesce(Cl11IP, 0)) %>%
  mutate(Omission = coalesce(CL11IN, 0) + coalesce(CL11G, 0) + coalesce(Cl11O, 0),
         Clitic_Substitution = Gender + Number + Double + Other_Pronoun,
         Item = "Clitics-11") %>% 
  select(Code, CA_Months, Status, Lang_Group, Group, Participant_Count, Item, Accuracy, Omission, Clitic_Substitution, Gender, Number, Double, Lexical_DP, Other_Pronoun)


# Prepare data structure
## Join dataframes together
Clitics_Long <- rbind(Clitics_1, Clitics_2, Clitics_4, Clitics_5, Clitics_6, Clitics_7, Clitics_8, Clitics_9, Clitics_10, Clitics_11) %>% 
  write_csv(here("JCL Nominal Manuscript", "Results", "Clitics Response Table.csv"))
Clitics_Long$Lang_Group <- factor(Clitics_Long$Lang_Group, levels = c("Monolingual", "Bilingual"))
Clitics_Long$Status <- factor(Clitics_Long$Status, levels = c("TD", "DLD"))
Clitics_Long$Group <- factor(Clitics_Long$Group, levels = c("ML-TD", "ML-DLD", "BL-TD", "BL-DLD"))


## Order levels
Clitics_Long$Omission <- as.numeric(Clitics_Long$Omission)
Clitics_Long$Clitic_Substitution <- as.numeric(Clitics_Long$Clitic_Substitution)
Clitics_Long$Gender <- as.numeric(Clitics_Long$Gender)
Clitics_Long$Number <- as.numeric(Clitics_Long$Number)
Clitics_Long$Double <- as.numeric(Clitics_Long$Double)
Clitics_Long$Lexical_DP <- as.numeric(Clitics_Long$Lexical_DP)
Clitics_Long$Other_Pronoun <- as.numeric(Clitics_Long$Other_Pronoun)


## Create submissions and omissions datasets
Clitics_Non_Target_Responses <- Clitics_Long %>% 
  filter(Accuracy == 0) %>% 
  filter(rowSums(select(., Clitic_Substitution, Omission) == 1) == 1) %>% 
  mutate(Code_Check = Clitic_Substitution + Omission) %>% 
  write_csv(here("JCL Nominal Manuscript", "Results", "Non-Target Clitics Responses.csv"))


## TD vs. DLD
Clitics_Status <- glmer(Clitic_Substitution ~ Status +
                          (1 | Code),
                        family = "binomial",
                        data = Clitics_Non_Target_Responses)

summary(Clitics_Status)

Clitics_Status_Table <- tidy(Clitics_Status, effects = "fixed") %>% 
  mutate(Set = "Clitics",
         Contrast = "TD/DLD")


## Bilingual versus monolingual
Clitics_Lang_Group <- glmer(Clitic_Substitution ~ Lang_Group +
                              (1 | Code),
                            family = "binomial",
                            data = Clitics_Non_Target_Responses)

summary(Clitics_Lang_Group)

Clitics_Lang_Group_Table <- tidy(Clitics_Lang_Group, effects = "fixed") %>% 
  mutate(Set = "Clitics",
         Contrast = "BL/ML")


## Join summary tables
Clitics_Results_Table <- rbind(Clitics_Status_Table, Clitics_Lang_Group_Table) %>% 
  rename(Effect = term,
         Estimate = estimate,
         SE = std.error,
         p = p.value) %>% 
  select(Effect, Estimate, SE, p, Set, Contrast) %>% 
  mutate(Estimate = round(Estimate, 2),
         SE = round(SE, 2),
         p = round(p, 3)) %>% 
  write_csv(here("JCL Nominal Manuscript", "Results", "JCL Nominal Clitics Pattern GLMMs.csv"))


# Prepare graph
## Adjust counts by participant
Clitics_Adjusted_Counts <- Clitics_Long %>% 
  group_by(Status, Lang_Group, Group, Participant_Count) %>% 
  summarize(Omission = sum(Omission),
            Gender = sum(Gender),
            Number = sum(Number),
            Double = sum(Double),
            Other_Pronoun = sum(Other_Pronoun),
            Lexical_DP = sum(Lexical_DP)) %>% 
  pivot_longer(cols = c(Omission, Gender, Number, Double, Other_Pronoun, Lexical_DP), 
               names_to = "Type", 
               values_to = "Count") %>% 
  mutate(Count_Adjusted = Count/Participant_Count)

Clitics_Adjusted_Counts$Status <- factor(Clitics_Adjusted_Counts$Status, levels = c("DLD", "TD"))
Clitics_Adjusted_Counts$Lang_Group <- factor(Clitics_Adjusted_Counts$Lang_Group, levels = c("Bilingual", "Monolingual"))
Clitics_Adjusted_Counts$Type <- factor(Clitics_Adjusted_Counts$Type,
                                       levels = c("Gender", "Number", "Double", "Other_Pronoun", "Lexical_DP", "Omission"))


## Create bar graph
Clitics_Distribution_Plot <- Clitics_Adjusted_Counts %>% 
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
  scale_x_discrete(labels = c("GEN substitution", "NUM substitution", "G/N substitution", "Se, le, les", "Noun phrase", "Omission")) +
  labs(x = "Type of non-target response",
       y = "Non-target responses per participant",
       title = "Distribution of Non-Target Clitic Responses by Group",
       fill = "Ability") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"))

Clitics_Distribution_Plot

ggsave(filename = here("JCL Nominal Manuscript", "Graphs", "JCL Nominal Figure 3.pdf"),
       plot = Clitics_Distribution_Plot,
       device = "pdf",
       width = 9,
       height = 3,
       units = "in")


## Check for participant count
ML_TD <- Clitics_Long %>% 
  filter(Group == "ML-TD")
unique(ML_TD$Code)

ML_DLD <- Clitics_Long %>% 
  filter(Group == "ML-DLD")
unique(ML_DLD$Code)

BL_TD <- Clitics_Long %>% 
  filter(Group == "BL-TD")
unique(BL_TD$Code)

BL_DLD <- Clitics_Long %>% 
  filter(Group == "BL-DLD")
unique(BL_DLD$Code)

