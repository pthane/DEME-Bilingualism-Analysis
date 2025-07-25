library(tidyverse)
library(here)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(openxlsx)

options(scipen = 99)



# Load data
Overall <- read_csv(here("JCL Verbal Manuscript", "JCL Verbal Agreement Analysis Data.csv")) %>% 
  filter(Accuracy == 0) %>% 
  filter(!is.na(Substitution)) %>%
  mutate(Substitution = case_when(Substitution == "3PS" ~ 1, TRUE ~ 0))

Overall$Status <- factor(Overall$Status, levels = c("DLD", "TD"))
Overall$Lang_Group <- factor(Overall$Lang_Group, levels = c("Monolingual", "Bilingual"))


# Correlations
## DLD vs. TD
Third_Status <- glmer(Substitution ~ Status +
                            (1 | Code),
                          family = "binomial",
                          data = Overall)

summary(Third_Status)

Third_Status_Table <- tidy(Third_Status, effects = "fixed") %>% 
  mutate(Contrast = "Language ability")


## Bilingualism
Third_Bilingualism <- glmer(Substitution ~ Lang_Group +
                        (1 | Code),
                      family = "binomial",
                      data = Overall)

summary(Third_Bilingualism)

Third_Bilingualism_Table <- tidy(Third_Bilingualism, effects = "fixed") %>% 
  mutate(Contrast = "Bilingualism")


# Joint table
Clitics_Results_Table <- rbind(Third_Status_Table, Third_Bilingualism_Table) %>% 
  rename(Effect = term,
         Estimate = estimate,
         SE = std.error,
         p = p.value) %>% 
  select(Effect, Estimate, SE, p, Contrast) %>% 
  mutate(Estimate = round(Estimate, 2),
         SE = round(SE, 2),
         p = round(p, 5)) %>% 
  write_csv(here("Conferences", "ISB", "Results", "ISB Third Person Singular Results.csv"))
