library(tidyverse)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(openxlsx)

options(scipen = 99)


# Prepare master data
## Load CSV files
Overall <- read_csv("./CSV Files/Aggregated Long Data.csv")
Overall$Status <- factor(Overall$Status, levels = c("TD", "DLD"))
Overall$Lang_Group <- factor(Overall$Lang_Group, levels = c("Monolingual", "Bilingual"))


# Overall correlations
## Create reference levels
Overall_ML_TD <- Overall
Overall_ML_TD$Group <- factor(Overall_ML_TD$Group, levels = c("ML-TD", "ML-DLD", "BL-TD", "BL-DLD"))

Overall_ML_DLD <- Overall
Overall_ML_DLD$Group <- factor(Overall_ML_DLD$Group, levels = c("ML-DLD", "ML-TD", "BL-TD", "BL-DLD"))

Overall_BL_TD <- Overall
Overall_BL_TD$Group <- factor(Overall_BL_TD$Group, levels = c("BL-TD", "ML-TD", "ML-DLD", "BL-DLD"))

Overall_BL_DLD <- Overall
Overall_BL_DLD$Group <- factor(Overall_BL_DLD$Group, levels = c("BL-DLD", "ML-TD", "ML-DLD", "BL-TD"))


# Generate correlation for overall data
## ML-TD as reference level
Overall_ML_TD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                     (1 | Code) + (1 | Item),
                                   data = Overall_ML_TD,
                                   family = "binomial")

Overall_ML_TD_Correlation_Table <- tidy(Overall_ML_TD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Overall_ML_TD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 3),
         Referencia ="ML-DT") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Variable = term, Estimado =  estimate, Error_est = std.error, Z = statistic, Group = group, p = p.value)


## ML-DLD as reference level
Overall_ML_DLD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                      (1 | Code) + (1 | Item),
                                    data = Overall_ML_DLD,
                                    family = "binomial")

Overall_ML_DLD_Correlation_Table <- tidy(Overall_ML_DLD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Overall_ML_DLD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 3),
         Referencia ="ML-TDL") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Variable = term, Estimado =  estimate, Error_est = std.error, Z = statistic, Group = group, p = p.value)


## BL-TD as reference level
Overall_BL_TD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                     (1 | Code) + (1 | Item),
                                   data = Overall_BL_TD,
                                   family = "binomial")

Overall_BL_TD_Correlation_Table <- tidy(Overall_BL_TD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Overall_BL_TD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 3),
         Referencia ="BL-TD") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Variable = term, Estimado =  estimate, Error_est = std.error, Z = statistic, Group = group, p = p.value)


## BL-DLD as reference level
Overall_BL_DLD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                      (1 | Code) + (1 | Item),
                                    data = Overall_BL_DLD,
                                    family = "binomial")

Overall_BL_DLD_Correlation_Table <- tidy(Overall_BL_DLD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Overall_BL_DLD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 3),
         Referencia ="BL-TDL") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Variable = term, Estimado =  estimate, Error_est = std.error, Z = statistic, Group = group, p = p.value)


## Join tables
Overall_Output <- rbind(Overall_ML_TD_Correlation_Table, Overall_ML_DLD_Correlation_Table, Overall_BL_TD_Correlation_Table, Overall_BL_DLD_Correlation_Table) %>% 
  mutate(Juego = "Combinado") %>% 
  select(!c("Group"))


# Create article correlations
## Create datasets
Articles <- Overall %>% 
  filter(Structure == "Articles")

Articles_ML_TD <- Articles
Articles_ML_TD$Group <- factor(Articles_ML_TD$Group, levels = c("ML-TD", "ML-DLD", "BL-TD", "BL-DLD"))

Articles_ML_DLD <- Articles
Articles_ML_DLD$Group <- factor(Articles_ML_DLD$Group, levels = c("ML-DLD", "ML-TD", "BL-TD", "BL-DLD"))

Articles_BL_TD <- Articles
Articles_BL_TD$Group <- factor(Articles_BL_TD$Group, levels = c("BL-TD", "ML-TD", "ML-DLD", "BL-DLD"))

Articles_BL_DLD <- Articles
Articles_BL_DLD$Group <- factor(Articles_BL_DLD$Group, levels = c("BL-DLD", "ML-TD", "ML-DLD", "BL-TD"))



## ML-TD as reference level
Articles_ML_TD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                      (1 | Code) + (1 | Item),
                                    data = Articles_ML_TD,
                                    family = "binomial")

Articles_ML_TD_Correlation_Table <- tidy(Articles_ML_TD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Articles_ML_TD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 3),
         Referencia ="ML-DT") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Variable = term, Estimado =  estimate, Error_est = std.error, Z = statistic, Group = group, p = p.value)


## ML-DLD as reference level
Articles_ML_DLD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                       (1 | Code) + (1 | Item),
                                     data = Articles_ML_DLD,
                                     family = "binomial")

Articles_ML_DLD_Correlation_Table <- tidy(Articles_ML_DLD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Articles_ML_DLD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 3),
         Referencia ="ML-TDL") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Variable = term, Estimado =  estimate, Error_est = std.error, Z = statistic, Group = group, p = p.value)


## BL-TD as reference level
Articles_BL_TD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                      (1 | Code) + (1 | Item),
                                    data = Articles_BL_TD,
                                    family = "binomial")

Articles_BL_TD_Correlation_Table <- tidy(Articles_BL_TD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Articles_BL_TD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 3),
         Referencia ="BL-DT") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Variable = term, Estimado =  estimate, Error_est = std.error, Z = statistic, Group = group, p = p.value)


## BL-DLD as reference level
Articles_BL_DLD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                       (1 | Code) + (1 | Item),
                                     data = Articles_BL_DLD,
                                     family = "binomial")

Articles_BL_DLD_Correlation_Table <- tidy(Articles_BL_DLD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Articles_BL_DLD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 3),
         Referencia ="BL-TDL") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Variable = term, Estimado =  estimate, Error_est = std.error, Z = statistic, Group = group, p = p.value)


## Join tables
Articles_Output <- rbind(Articles_ML_TD_Correlation_Table, Articles_ML_DLD_Correlation_Table, Articles_BL_TD_Correlation_Table, Articles_BL_DLD_Correlation_Table) %>% 
  mutate(Juego = "Articulos") %>% 
  select(!c("Group"))


# Create clitic correlations
## Create datasets
Clitics <- Overall %>% 
  filter(Structure == "Clitics")

Clitics_ML_TD <- Clitics
Clitics_ML_TD$Group <- factor(Clitics_ML_TD$Group, levels = c("ML-TD", "ML-DLD", "BL-TD", "BL-DLD"))

Clitics_ML_DLD <- Clitics
Clitics_ML_DLD$Group <- factor(Clitics_ML_DLD$Group, levels = c("ML-DLD", "ML-TD", "BL-TD", "BL-DLD"))

Clitics_BL_TD <- Clitics
Clitics_BL_TD$Group <- factor(Clitics_BL_TD$Group, levels = c("BL-TD", "ML-TD", "ML-DLD", "BL-DLD"))

Clitics_BL_DLD <- Clitics
Clitics_BL_DLD$Group <- factor(Clitics_BL_DLD$Group, levels = c("BL-DLD", "ML-TD", "ML-DLD", "BL-TD"))



## ML-TD as reference level
Clitics_ML_TD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                     (1 | Code) + (1 | Item),
                                   data = Clitics_ML_TD,
                                   family = "binomial")

Clitics_ML_TD_Correlation_Table <- tidy(Clitics_ML_TD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Clitics_ML_TD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 3),
         Referencia ="ML-DT") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Variable = term, Estimado =  estimate, Error_est = std.error, Z = statistic, Group = group, p = p.value)


## ML-DLD as reference level
Clitics_ML_DLD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                      (1 | Code) + (1 | Item),
                                    data = Clitics_ML_DLD,
                                    family = "binomial")

Clitics_ML_DLD_Correlation_Table <- tidy(Clitics_ML_DLD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Clitics_ML_DLD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 3),
         Referencia ="ML-TDL") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Variable = term, Estimado =  estimate, Error_est = std.error, Z = statistic, Group = group, p = p.value)


## BL-TD as reference level
Clitics_BL_TD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                     (1 | Code) + (1 | Item),
                                   data = Clitics_BL_TD,
                                   family = "binomial")

Clitics_BL_TD_Correlation_Table <- tidy(Clitics_BL_TD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Clitics_BL_TD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 3),
         Referencia ="BL-DT") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Variable = term, Estimado =  estimate, Error_est = std.error, Z = statistic, Group = group, p = p.value)


## BL-DLD as reference level
Clitics_BL_DLD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                      (1 | Code) + (1 | Item),
                                    data = Clitics_BL_DLD,
                                    family = "binomial")

Clitics_BL_DLD_Correlation_Table <- tidy(Clitics_BL_DLD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Clitics_BL_DLD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 3),
         Referencia ="BL-TDL") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Variable = term, Estimado =  estimate, Error_est = std.error, Z = statistic, Group = group, p = p.value)


## Join tables
Clitics_Output <- rbind(Clitics_ML_TD_Correlation_Table, Clitics_ML_DLD_Correlation_Table, Clitics_BL_TD_Correlation_Table, Clitics_BL_DLD_Correlation_Table) %>% 
  mutate(Juego = "Cliticos") %>% 
  select(!c("Group"))


# Create article correlations
## Create datasets
Verbal_Agreement <- Overall %>% 
  filter(Structure == "Verbal Agreement")

Verbal_Agreement_ML_TD <- Verbal_Agreement
Verbal_Agreement_ML_TD$Group <- factor(Verbal_Agreement_ML_TD$Group, levels = c("ML-TD", "ML-DLD", "BL-TD", "BL-DLD"))

Verbal_Agreement_ML_DLD <- Verbal_Agreement
Verbal_Agreement_ML_DLD$Group <- factor(Verbal_Agreement_ML_DLD$Group, levels = c("ML-DLD", "ML-TD", "BL-TD", "BL-DLD"))

Verbal_Agreement_BL_TD <- Verbal_Agreement
Verbal_Agreement_BL_TD$Group <- factor(Verbal_Agreement_BL_TD$Group, levels = c("BL-TD", "ML-TD", "ML-DLD", "BL-DLD"))

Verbal_Agreement_BL_DLD <- Verbal_Agreement
Verbal_Agreement_BL_DLD$Group <- factor(Verbal_Agreement_BL_DLD$Group, levels = c("BL-DLD", "ML-TD", "ML-DLD", "BL-TD"))



## ML-TD as reference level
Verbal_Agreement_ML_TD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                              (1 | Code) + (1 | Item),
                                            data = Verbal_Agreement_ML_TD,
                                            family = "binomial")

Verbal_Agreement_ML_TD_Correlation_Table <- tidy(Verbal_Agreement_ML_TD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Verbal_Agreement_ML_TD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 3),
         Referencia ="ML-DT") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Variable = term, Estimado =  estimate, Error_est = std.error, Z = statistic, Group = group, p = p.value)


## ML-DLD as reference level
Verbal_Agreement_ML_DLD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                               (1 | Code) + (1 | Item),
                                             data = Verbal_Agreement_ML_DLD,
                                             family = "binomial")

Verbal_Agreement_ML_DLD_Correlation_Table <- tidy(Verbal_Agreement_ML_DLD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Verbal_Agreement_ML_DLD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 3),
         Referencia ="ML-TDL") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Variable = term, Estimado =  estimate, Error_est = std.error, Z = statistic, Group = group, p = p.value)


## BL-TD as reference level
Verbal_Agreement_BL_TD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                              (1 | Code) + (1 | Item),
                                            data = Verbal_Agreement_BL_TD,
                                            family = "binomial")

Verbal_Agreement_BL_TD_Correlation_Table <- tidy(Verbal_Agreement_BL_TD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Verbal_Agreement_BL_TD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 3),
         Referencia ="BL-DT") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Variable = term, Estimado =  estimate, Error_est = std.error, Z = statistic, Group = group, p = p.value)


## BL-DLD as reference level
Verbal_Agreement_BL_DLD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                               (1 | Code) + (1 | Item),
                                             data = Verbal_Agreement_BL_DLD,
                                             family = "binomial")

Verbal_Agreement_BL_DLD_Correlation_Table <- tidy(Verbal_Agreement_BL_DLD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Verbal_Agreement_BL_DLD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 3),
         Referencia ="BL-TDL") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Variable = term, Estimado =  estimate, Error_est = std.error, Z = statistic, Group = group, p = p.value)


## Join tables
Verbal_Agreement_Output <- rbind(Verbal_Agreement_ML_TD_Correlation_Table, Verbal_Agreement_ML_DLD_Correlation_Table, Verbal_Agreement_BL_TD_Correlation_Table, Verbal_Agreement_BL_DLD_Correlation_Table) %>% 
  mutate(Juego = "Concordancia verbal") %>% 
  select(!c("Group"))


# Create clitic correlations
## Create datasets
Subjunctive <- Overall %>% 
  filter(Structure == "Subjunctive")

Subjunctive_ML_TD <- Subjunctive
Subjunctive_ML_TD$Group <- factor(Subjunctive_ML_TD$Group, levels = c("ML-TD", "ML-DLD", "BL-TD", "BL-DLD"))

Subjunctive_ML_DLD <- Subjunctive
Subjunctive_ML_DLD$Group <- factor(Subjunctive_ML_DLD$Group, levels = c("ML-DLD", "ML-TD", "BL-TD", "BL-DLD"))

Subjunctive_BL_TD <- Subjunctive
Subjunctive_BL_TD$Group <- factor(Subjunctive_BL_TD$Group, levels = c("BL-TD", "ML-TD", "ML-DLD", "BL-DLD"))

Subjunctive_BL_DLD <- Subjunctive
Subjunctive_BL_DLD$Group <- factor(Subjunctive_BL_DLD$Group, levels = c("BL-DLD", "ML-TD", "ML-DLD", "BL-TD"))



## ML-TD as reference level
Subjunctive_ML_TD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                         (1 | Code) + (1 | Item),
                                       data = Subjunctive_ML_TD,
                                       family = "binomial")

Subjunctive_ML_TD_Correlation_Table <- tidy(Subjunctive_ML_TD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Subjunctive_ML_TD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 3),
         Referencia ="ML-DT") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Variable = term, Estimado =  estimate, Error_est = std.error, Z = statistic, Group = group, p = p.value)


## ML-DLD as reference level
Subjunctive_ML_DLD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                          (1 | Code) + (1 | Item),
                                        data = Subjunctive_ML_DLD,
                                        family = "binomial")

Subjunctive_ML_DLD_Correlation_Table <- tidy(Subjunctive_ML_DLD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Subjunctive_ML_DLD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 3),
         Referencia ="ML-TDL") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Variable = term, Estimado =  estimate, Error_est = std.error, Z = statistic, Group = group, p = p.value)


## BL-TD as reference level
Subjunctive_BL_TD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                         (1 | Code) + (1 | Item),
                                       data = Subjunctive_BL_TD,
                                       family = "binomial")

Subjunctive_BL_TD_Correlation_Table <- tidy(Subjunctive_BL_TD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Subjunctive_BL_TD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 3),
         Referencia ="BL-DT") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Variable = term, Estimado =  estimate, Error_est = std.error, Z = statistic, Group = group, p = p.value)


## BL-DLD as reference level
Subjunctive_BL_DLD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                          (1 | Code) + (1 | Item),
                                        data = Subjunctive_BL_DLD,
                                        family = "binomial")

Subjunctive_BL_DLD_Correlation_Table <- tidy(Subjunctive_BL_DLD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Subjunctive_BL_DLD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 3),
         Referencia ="BL-TDL") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Variable = term, Estimado =  estimate, Error_est = std.error, Z = statistic, Group = group, p = p.value)


## Join tables
Subjunctive_Output <- rbind(Subjunctive_ML_TD_Correlation_Table, Subjunctive_ML_DLD_Correlation_Table, Subjunctive_BL_TD_Correlation_Table, Subjunctive_BL_DLD_Correlation_Table) %>% 
  mutate(Juego = "Subjuntivo") %>% 
  select(!c("Group"))


# Join all tables
Joint_Table <- rbind(Overall_Output, Articles_Output, Clitics_Output, Verbal_Agreement_Output, Subjunctive_Output) %>% 
  select(-Z, -effect) %>% 
  write_csv(here("Conferences", "SIUS", "Results", "SIUS Group Correlations.csv"))
