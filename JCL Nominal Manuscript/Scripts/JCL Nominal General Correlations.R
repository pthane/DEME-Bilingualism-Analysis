library(tidyverse)
library(here)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(openxlsx)

options(scipen = 99)


# Prepare master data
## Load CSV files
Articles <- read_csv(here("CSV Files", "Aggregated Long Data.csv")) %>% 
  filter(Structure == "Articles")

Clitics <- read_csv(here("CSV Files", "Aggregated Long Data.csv")) %>% 
  filter(Structure == "Clitics")

Overall <- rbind(Articles, Clitics)
Overall$Status <- factor(Overall$Status, levels = c("TD", "DLD"))
Overall$Lang_Group <- factor(Overall$Lang_Group, levels = c("Monolingual", "Bilingual"))


## Prepare ML-TD reference level
Overall_ML_TD <- Overall
Overall_ML_TD$Group <- factor(Overall_ML_TD$Group, levels = c("ML-TD", "ML-DLD", "BL-TD", "BL-DLD"))


## Prepare ML-DLD reference level
Overall_ML_DLD <- Overall
Overall_ML_DLD$Group <- factor(Overall_ML_DLD$Group, levels = c("ML-DLD", "ML-TD", "BL-TD", "BL-DLD"))


## Prepare BL-TD reference level
Overall_BL_TD <- Overall
Overall_BL_TD$Group <- factor(Overall_BL_TD$Group, levels = c("BL-TD", "ML-TD", "ML-DLD", "BL-DLD"))


## Prepare BL-DLD reference level
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
  mutate(across(where(is.numeric), round, digits = 10),
         Reference = "ML-TD") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Effect_type = effect, Variable = term, Estimate = estimate, SE = std.error, Z = statistic, Group = group, p = p.value)


## ML-DLD as reference level
Overall_ML_DLD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                      (1 | Code) + (1 | Item),
                                    data = Overall_ML_DLD,
                                    family = "binomial")

Overall_ML_DLD_Correlation_Table <- tidy(Overall_ML_DLD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Overall_ML_DLD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 10),
         Reference = "ML-DLD") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Effect_type = effect, Variable = term, Estimate = estimate, SE = std.error, Z = statistic, Group = group, p = p.value)


## BL-TD as reference level
Overall_BL_TD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                     (1 | Code) + (1 | Item),
                                   data = Overall_BL_TD,
                                   family = "binomial")

Overall_BL_TD_Correlation_Table <- tidy(Overall_BL_TD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Overall_BL_TD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 10),
         Reference = "BL-TD") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Effect_type = effect, Variable = term, Estimate = estimate, SE = std.error, Z = statistic, Group = group, p = p.value)


## BL-DLD as reference level
Overall_BL_DLD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                      (1 | Code) + (1 | Item),
                                    data = Overall_BL_DLD,
                                    family = "binomial")

Overall_BL_DLD_Correlation_Table <- tidy(Overall_BL_DLD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Overall_BL_DLD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 10),
         Reference = "BL-DLD") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Effect_type = effect, Variable = term, Estimate = estimate, SE = std.error, Z = statistic, Group = group, p = p.value)


## Join tables
Overall_Output <- rbind(Overall_ML_TD_Correlation_Table, Overall_ML_DLD_Correlation_Table, Overall_BL_TD_Correlation_Table, Overall_BL_DLD_Correlation_Table) %>% 
  mutate(Data = "Overall") %>% 
  select(!c("Group", "Effect_type"))


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
  mutate(across(where(is.numeric), round, digits = 10),
         Reference = "ML-TD") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Effect_type = effect, Variable = term, Estimate = estimate, SE = std.error, Z = statistic, Group = group, p = p.value)


## ML-DLD as reference level
Articles_ML_DLD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                       (1 | Code) + (1 | Item),
                                     data = Articles_ML_DLD,
                                     family = "binomial")

Articles_ML_DLD_Correlation_Table <- tidy(Articles_ML_DLD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Articles_ML_DLD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 10),
         Reference = "ML-DLD") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Effect_type = effect, Variable = term, Estimate = estimate, SE = std.error, Z = statistic, Group = group, p = p.value)


## BL-TD as reference level
Articles_BL_TD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                      (1 | Code) + (1 | Item),
                                    data = Articles_BL_TD,
                                    family = "binomial")

Articles_BL_TD_Correlation_Table <- tidy(Articles_BL_TD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Articles_BL_TD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 10),
         Reference = "BL-TD") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Effect_type = effect, Variable = term, Estimate = estimate, SE = std.error, Z = statistic, Group = group, p = p.value)


## BL-DLD as reference level
Articles_BL_DLD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                       (1 | Code) + (1 | Item),
                                     data = Articles_BL_DLD,
                                     family = "binomial")

Articles_BL_DLD_Correlation_Table <- tidy(Articles_BL_DLD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Articles_BL_DLD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 10),
         Reference = "BL-DLD") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Effect_type = effect, Variable = term, Estimate = estimate, SE = std.error, Z = statistic, Group = group, p = p.value)


## Join tables
Articles_Output <- rbind(Articles_ML_TD_Correlation_Table, Articles_ML_DLD_Correlation_Table, Articles_BL_TD_Correlation_Table, Articles_BL_DLD_Correlation_Table) %>% 
  mutate(Data = "Articles") %>% 
  select(!c("Group", "Effect_type"))


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
  mutate(across(where(is.numeric), round, digits = 10),
         Reference = "ML-TD") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Effect_type = effect, Variable = term, Estimate = estimate, SE = std.error, Z = statistic, Group = group, p = p.value)


## ML-DLD as reference level
Clitics_ML_DLD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                      (1 | Code) + (1 | Item),
                                    data = Clitics_ML_DLD,
                                    family = "binomial")

Clitics_ML_DLD_Correlation_Table <- tidy(Clitics_ML_DLD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Clitics_ML_DLD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 10),
         Reference = "ML-DLD") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Effect_type = effect, Variable = term, Estimate = estimate, SE = std.error, Z = statistic, Group = group, p = p.value)


## BL-TD as reference level
Clitics_BL_TD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                     (1 | Code) + (1 | Item),
                                   data = Clitics_BL_TD,
                                   family = "binomial")

Clitics_BL_TD_Correlation_Table <- tidy(Clitics_BL_TD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Clitics_BL_TD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 10),
         Reference = "BL-TD") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Effect_type = effect, Variable = term, Estimate = estimate, SE = std.error, Z = statistic, Group = group, p = p.value)


## BL-DLD as reference level
Clitics_BL_DLD_Correlation <- glmer(Accuracy ~ 1 + Group + CA_Months_Ctd +
                                      (1 | Code) + (1 | Item),
                                    data = Clitics_BL_DLD,
                                    family = "binomial")

Clitics_BL_DLD_Correlation_Table <- tidy(Clitics_BL_DLD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Clitics_BL_DLD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 10),
         Reference = "BL-DLD") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Effect_type = effect, Variable = term, Estimate = estimate, SE = std.error, Z = statistic, Group = group, p = p.value)


## Join tables
Clitics_Output <- rbind(Clitics_ML_TD_Correlation_Table, Clitics_ML_DLD_Correlation_Table, Clitics_BL_TD_Correlation_Table, Clitics_BL_DLD_Correlation_Table) %>% 
  mutate(Data = "Clitics") %>% 
  select(!c("Group", "Effect_type"))


# Create master table
## Join columns
Correlations_Table <- rbind(Overall_Output, Articles_Output, Clitics_Output)
Correlations_Table$Data <- factor(Correlations_Table$Data, levels = c("Overall", "Articles", "Clitics"))


## Apply formatting for Excel sheet
Correlations_Table$Italicize <- Correlations_Table$p < 0.05
Correlations_Table$p <- formatC(Correlations_Table$p, format = "f", digits = 10)

Master_Table <- createWorkbook()
addWorksheet(Master_Table, "Output")
writeData(Master_Table, "Output", Correlations_Table)

Italic_Style <- createStyle(textDecoration = "italic")
Number_Columns <- ncol(Correlations_Table)


## If-then statements
for (i in 1:nrow(Correlations_Table)) {
  if (Correlations_Table$Italicize[i]) {
    addStyle(
      Master_Table,
      sheet = "Output",
      style = Italic_Style,
      rows = i + 1,
      cols = 1:Number_Columns,
      gridExpand = TRUE,
      stack = TRUE)}}


## Save CSV
saveWorkbook(Master_Table, (here("JCL Nominal Manuscript", "Results", "JCL Nominal GLMM Results.xlsx")), overwrite = TRUE)
