library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(broom.mixed)
library(openxlsx)
library(here)

options(scipen = 99)


# Prepare master data
## Load CSV files
Overall <- read_csv(here("JCL Verbal Manuscript", "JCL Verbal Agreement Analysis Data.csv"))
Overall$Status <- factor(Overall$Status, levels = c("TD", "DLD"))
Overall$Lang_Group <- factor(Overall$Lang_Group, levels = c("Monolingual", "Bilingual"))


# Check averages
Averages <- Overall %>% 
  group_by(Status, Lang_Group) %>% 
  summarize(Mean = (mean(Accuracy, na.rm = TRUE)*100),
            SD = (sd(Accuracy, na.rm = TRUE)*100))


## Prepare ML-TD reference level
ML_TD <- Overall
ML_TD$Group <- factor(ML_TD$Group, levels = c("ML-TD", "ML-DLD", "BL-TD", "BL-DLD"))
ML_TD$Exp_Person <- factor(ML_TD$Exp_Person, levels = c("Third", "First", "Second"))
ML_TD$Exp_Number <- factor(ML_TD$Exp_Number, levels = c("Plural", "Singular"))


## Prepare ML-DLD reference level
ML_DLD <- Overall
ML_DLD$Group <- factor(ML_DLD$Group, levels = c("ML-DLD", "ML-TD", "BL-TD", "BL-DLD"))
ML_DLD$Exp_Person <- factor(ML_DLD$Exp_Person, levels = c("Third", "First", "Second"))
ML_DLD$Exp_Number <- factor(ML_DLD$Exp_Number, levels = c("Plural", "Singular"))


## Prepare BL-TD reference level
BL_TD <- Overall
BL_TD$Group <- factor(BL_TD$Group, levels = c("BL-TD", "ML-TD", "ML-DLD", "BL-DLD"))
BL_TD$Exp_Person <- factor(BL_TD$Exp_Person, levels = c("Third", "First", "Second"))
BL_TD$Exp_Number <- factor(BL_TD$Exp_Number, levels = c("Plural", "Singular"))


## Prepare BL-DLD reference level
BL_DLD <- Overall
BL_DLD$Group <- factor(BL_DLD$Group, levels = c("BL-DLD", "ML-TD", "ML-DLD", "BL-TD"))
BL_DLD$Exp_Person <- factor(BL_DLD$Exp_Person, levels = c("Third", "First", "Second"))
BL_DLD$Exp_Number <- factor(BL_DLD$Exp_Number, levels = c("Plural", "Singular"))


# Generate correlation for overall data
## ML-TD as reference level
ML_TD_Correlation <- glmer(Accuracy ~ Group + CA_Months_Ctd +
                                     (1 | Code) + (1 | Item),
                                   data = ML_TD,
                                   family = "binomial",
                                   control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(ML_TD_Correlation)

ML_TD_Correlation_Table <- tidy(ML_TD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(ML_TD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 10),
         Reference = "ML-TD") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Effect_type = effect, Variable = term, Estimate = estimate, SE = std.error, Z = statistic, Group = group, p = p.value)


## ML-DLD as reference level
ML_DLD_Correlation <- glmer(Accuracy ~ Group + CA_Months_Ctd +
                                      (1 | Code) + (1 | Item),
                                    data = ML_DLD,
                                    family = "binomial")

summary(ML_DLD_Correlation)

ML_DLD_Correlation_Table <- tidy(ML_DLD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(ML_DLD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 10),
         Reference = "ML-DLD") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Effect_type = effect, Variable = term, Estimate = estimate, SE = std.error, Z = statistic, Group = group, p = p.value)


## BL-TD as reference level
BL_TD_Correlation <- glmer(Accuracy ~ Group + CA_Months_Ctd +
                                     (1 | Code) + (1 | Item),
                                   data = BL_TD,
                                   family = "binomial")

summary(BL_TD_Correlation)

BL_TD_Correlation_Table <- tidy(BL_TD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(BL_TD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 10),
         Reference = "BL-TD") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Effect_type = effect, Variable = term, Estimate = estimate, SE = std.error, Z = statistic, Group = group, p = p.value)


## BL-DLD as reference level
BL_DLD_Correlation <- glmer(Accuracy ~ Group + CA_Months_Ctd +
                                      (1 | Code) + (1 | Item),
                                    data = BL_DLD,
                                    family = "binomial")

summary(BL_DLD_Correlation)

BL_DLD_Correlation_Table <- tidy(BL_DLD_Correlation, effects = "fixed") %>%
  bind_rows(tidy(BL_DLD_Correlation, effects = "ran_pars")) %>%
  mutate(across(where(is.numeric), round, digits = 10),
         Reference = "BL-DLD") %>% 
  filter(!effect == "ran_pars") %>% 
  rename(Effect_type = effect, Variable = term, Estimate = estimate, SE = std.error, Z = statistic, Group = group, p = p.value)


# Create statistical table
## Join tables
Correlations_Table <- rbind(ML_TD_Correlation_Table, ML_DLD_Correlation_Table, BL_TD_Correlation_Table, BL_DLD_Correlation_Table) %>% 
  mutate(Data = "Overall") %>% 
  select(!c("Group", "Effect_type"))


## Apply formatting for Excel sheet
Correlations_Table$Italicize <- Correlations_Table$p < 0.05
Correlations_Table$p <- formatC(Correlations_Table$p, format = "f", digits = 10)

Master_Table <- createWorkbook()
addWorksheet(Master_Table, "Correlations_Table")
writeData(Master_Table, "Correlations_Table", Correlations_Table)

Italic_Style <- createStyle(textDecoration = "italic")
Number_Columns <- ncol(Correlations_Table)


## If-then statements
for (i in 1:nrow(Correlations_Table)) {
  if (Correlations_Table$Italicize[i]) {
    addStyle(
      Master_Table,
      sheet = "Correlations_Table",
      style = Italic_Style,
      rows = i + 1,
      cols = 1:Number_Columns,
      gridExpand = TRUE,
      stack = TRUE)}}

## Order columns
Correlations_Table$Data <- factor(Correlations_Table$Data, levels = c("Overall", "Verbal Agreement", "Subjunctive"))


## Save CSV
saveWorkbook(Master_Table, (here("JCL Verbal Manuscript", "Results", "JCL Verbal Overall GLMM Results.xlsx")), overwrite = TRUE)
