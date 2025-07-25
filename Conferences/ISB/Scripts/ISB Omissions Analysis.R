library(here)
library(tidyverse)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(openxlsx)

options(scipen = 99)


# Load and modify data
## Articles data
Articles_Data <- read_csv(here("JCL Nominal Manuscript", "Results", "Non-Target Article Responses.csv"))

Articles_Data$Lang_Group <- factor(Articles_Data$Lang_Group, levels = c("Monolingual", "Bilingual"))
Articles_Data$Status <- factor(Articles_Data$Status, levels = c("TD", "DLD"))
Articles_Data$Group <- factor(Articles_Data$Group, levels = c("ML-TD", "ML-DLD", "BL-TD", "BL-DLD"))


## Clitics data
Clitics_Data <- read_csv(here("JCL Nominal Manuscript", "Results", "Non-Target Clitics Responses.csv"))

Clitics_Data$Lang_Group <- factor(Clitics_Data$Lang_Group, levels = c("Monolingual", "Bilingual"))
Clitics_Data$Status <- factor(Clitics_Data$Status, levels = c("TD", "DLD"))
Clitics_Data$Group <- factor(Clitics_Data$Group, levels = c("ML-TD", "ML-DLD", "BL-TD", "BL-DLD"))


## Verbal agreement data
Verbal_Data <- read_csv(here("JCL Verbal Manuscript", "JCL Verbal Agreement Analysis Data.csv")) %>% 
  filter(Accuracy == 0) %>% 
  rename(Person = Person_Accuracy,
         Number = Number_Accuracy) %>%
  mutate(Person = ifelse(Person == 1, 0, 1),
         Number = ifelse(Number == 1, 0, 1)) %>% 
  mutate(Nonfinite = Gerund + Infinitive) %>%
  mutate(Nonfinite = ifelse(Substitution == "3PS", 1, Nonfinite)) %>%
  mutate(Sub = ifelse(Substitution %in% c("3PP", "1PS", "1PP", "2PP", "2PS"), 1, NA)) %>%
  mutate(Sub = ifelse(is.na(Sub), 0, Sub)) %>% 
  select(!Substitution) %>% 
  rename(Substitution = Sub) %>% 
  mutate(Check_Code = Nonfinite + Substitution) %>% 
  filter(Check_Code == 1)

Verbal_Data$Status <- factor(Verbal_Data$Status, levels = c("TD", "DLD"))
Verbal_Data$Lang_Group <- factor(Verbal_Data$Lang_Group, levels = c("Monolingual", "Bilingual"))


# Articles correlations
## Language ability
Articles_Language_Ability <- glmer(Omission ~ Status +
                                     (1 | Code),
                                   family = "binomial",
                                   data = Articles_Data)

summary(Articles_Language_Ability)

Articles_Language_Ability_Table <- tidy(Articles_Language_Ability, effects = "fixed") %>% 
  bind_rows(tidy(Articles_Language_Ability, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 5)) %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Effect_type = effect, Variable = term, Estimate = estimate, SE = std.error, Z = statistic, p = p.value) %>% 
  select(!c(group, Effect_type, Z)) %>% 
  mutate(Model = "Language ability",
         p = format(round(p, 3), nsmall = 3))


## Bilingualism
Articles_Bilingualism <- glmer(Omission ~ Lang_Group +
                                 (1 | Code),
                               family = "binomial",
                               data = Articles_Data)

summary(Articles_Bilingualism)

Articles_Bilingualism_Table <- tidy(Articles_Bilingualism, effects = "fixed") %>% 
  bind_rows(tidy(Articles_Bilingualism, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 5)) %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Effect_type = effect, Variable = term, Estimate = estimate, SE = std.error, Z = statistic, p = p.value) %>% 
  select(!c(group, Effect_type, Z)) %>% 
  mutate(Model = "Bilingualism",
         p = format(round(p, 3), nsmall = 3))


## Join summary tables
Combined_Articles_Table <- rbind(Articles_Language_Ability_Table, Articles_Bilingualism_Table) %>% 
  mutate(Structure = "Article omissions")


# Clitics correlations
## Language ability
Clitics_Language_Ability <- glmer(Omission ~ Status +
                          (1 | Code),
                        family = "binomial",
                        data = Clitics_Data)

summary(Clitics_Language_Ability)

Clitics_Language_Ability_Table <- tidy(Clitics_Language_Ability, effects = "fixed") %>% 
  bind_rows(tidy(Clitics_Language_Ability, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 5)) %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Effect_type = effect, Variable = term, Estimate = estimate, SE = std.error, Z = statistic, p = p.value) %>% 
  select(!c(group, Effect_type, Z)) %>% 
  mutate(Model = "Language ability",
         p = format(round(p, 3), nsmall = 3))


## Bilingualism
Clitics_Bilingualism <- glmer(Omission ~ Lang_Group +
                                (1 | Code),
                              family = "binomial",
                              data = Clitics_Data)

summary(Clitics_Bilingualism)

Clitics_Bilingualism_Table <- tidy(Clitics_Bilingualism, effects = "fixed") %>% 
  bind_rows(tidy(Clitics_Bilingualism, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 5)) %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Effect_type = effect, Variable = term, Estimate = estimate, SE = std.error, Z = statistic, p = p.value) %>% 
  select(!c(group, Effect_type, Z)) %>% 
  mutate(Model = "Bilingualism",
         p = format(round(p, 3), nsmall = 3))


## Join summary tables
Combined_Clitics_Table <- rbind(Clitics_Language_Ability_Table, Clitics_Bilingualism_Table) %>% 
  mutate(Structure = "Clitic omissions")


# Verbal correlations
## Language ability
Nonfinite_Language_Ability <- glmer(Nonfinite ~ Status +
                                      (1 | Code),
                                    data = Verbal_Data,
                                    family = "binomial")

summary(Nonfinite_Language_Ability)


Nonfinite_Language_Ability_Table <- tidy(Nonfinite_Language_Ability, effects = "fixed") %>%
  bind_rows(tidy(Nonfinite_Language_Ability, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 5)) %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Effect_type = effect, Variable = term, Estimate = estimate, SE = std.error, Z = statistic, p = p.value) %>% 
  select(!c(group, Effect_type, Z)) %>% 
  mutate(Model = "Language ability",
         p = format(round(p, 3), nsmall = 3))


## Bilingualism
Nonfinite_Bilingualism <- glmer(Nonfinite ~ Lang_Group +
                                  (1 | Code),
                                data = Verbal_Data,
                                family = "binomial")

summary(Nonfinite_Bilingualism)

Nonfinite_Bilingualism_Table <- tidy(Nonfinite_Bilingualism, effects = "fixed") %>%
  bind_rows(tidy(Nonfinite_Bilingualism, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 5)) %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Effect_type = effect, Variable = term, Estimate = estimate, SE = std.error, Z = statistic, p = p.value) %>% 
  select(!c(group, Effect_type, Z)) %>% 
  mutate(Model = "Bilingualism",
         p = format(round(p, 3), nsmall = 3))


## Combined table
Combined_Verbs_Table <- rbind(Nonfinite_Language_Ability_Table, Nonfinite_Bilingualism_Table) %>% 
  mutate(Structure = "Non-finite verbs")


# Create master table
Final_Table <- rbind(Combined_Articles_Table, Combined_Clitics_Table, Combined_Verbs_Table) %>% 
  write_csv(here("Conferences", "ISB", "Results", "ISB Qualitative Analysis.csv"))

