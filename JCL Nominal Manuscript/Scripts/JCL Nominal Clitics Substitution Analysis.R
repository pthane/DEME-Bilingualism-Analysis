library(here)
library(tidyverse)
library(lme4)
library(lmerTest)
library(broom.mixed)

options(scipen = 99)


# Load data
Clitics_Overall <- read_csv(here("CSV Files", "Aggregated Long Data.csv")) %>% 
  filter(Structure == "Clitics")
Clitics_Overall$Status <- factor(Clitics_Overall$Status, levels = c("TD", "DLD"))
Clitics_Overall$Lang_Group <- factor(Clitics_Overall$Lang_Group, levels = c("Monolingual", "Bilingual"))


# Prepare items
Clitics_1 <- Clitics_Overall %>% 
  filter(Item == "Clitic_1") %>% 
  mutate(Exp_Gender = "Masculine",
         Exp_Number = "Singular")

Clitics_2 <- Clitics_Overall %>% 
  filter(Item == "Clitic_2") %>% 
  mutate(Exp_Gender = "Feminine",
         Exp_Number = "Singular")

Clitics_4 <- Clitics_Overall %>% 
  filter(Item == "Clitic_4") %>% 
  mutate(Exp_Gender = "Masculine",
         Exp_Number = "Plural")

Clitics_5 <- Clitics_Overall %>% 
  filter(Item == "Clitic_5") %>% 
  mutate(Exp_Gender = "Feminine",
         Exp_Number = "Plural")

Clitics_6 <- Clitics_Overall %>% 
  filter(Item == "Clitic_6") %>% 
  mutate(Exp_Gender = "Masculine",
         Exp_Number = "Plural")

Clitics_7 <- Clitics_Overall %>% 
  filter(Item == "Clitic_7") %>% 
  mutate(Exp_Gender = "Masculine",
         Exp_Number = "Singular")

Clitics_8 <- Clitics_Overall %>% 
  filter(Item == "Clitic_8") %>% 
  mutate(Exp_Gender = "Feminine",
         Exp_Number = "Plural")

Clitics_9 <- Clitics_Overall %>% 
  filter(Item == "Clitic_9") %>% 
  mutate(Exp_Gender = "Feminine",
         Exp_Number = "Singular")

Clitics_10 <- Clitics_Overall %>% 
  filter(Item == "Clitic_10") %>% 
  mutate(Exp_Gender = "Masculine",
         Exp_Number = "Singular")

Clitics_11 <- Clitics_Overall %>% 
  filter(Item == "Clitic_11") %>% 
  mutate(Exp_Gender = "Feminine",
         Exp_Number = "Singular")


## Join numbers
Clitics_Master <- rbind(Clitics_1, Clitics_2, Clitics_4, Clitics_5, Clitics_6, Clitics_7, Clitics_8, Clitics_9, Clitics_10, Clitics_11)
Clitics_Master$Group <- factor(Clitics_Master$Group, levels = c("ML-TD", "ML-DLD", "BL-TD", "BL-DLD"))


# Generate correlations
## Gender
Clitics_Gender_Correlation <- glmer(Accuracy ~ Exp_Gender + Lang_Group + Status + Exp_Gender:Lang_Group + Exp_Gender:Status +
                               (1 | Code) + (1 | Item),
                             family = "binomial",
                             data = Clitics_Master)

summary(Clitics_Gender_Correlation)

Clitics_Gender_Table <- tidy(Clitics_Gender_Correlation, effects = "fixed") %>% 
  mutate(Model = "Clitic Gender") %>% 
  mutate(across(where(is.numeric), round, digits = 3)) %>% 
  filter(!effect == "ran_pars") %>% 
  select(!c(effect, statistic)) %>% 
  rename(Variable = term, Estimate = estimate, SE = std.error, p = p.value)


## Number
Clitics_Number_Correlation <- glmer(Accuracy ~ Exp_Number + Lang_Group + Status + Exp_Number:Lang_Group + Exp_Number:Status +
                                      (1 | Code) + (1 | Item),
                                    family = "binomial",
                                    data = Clitics_Master)

summary(Clitics_Number_Correlation)

Clitics_Number_Table <- tidy(Clitics_Number_Correlation, effects = "fixed") %>% 
  mutate(Model = "Clitic Number") %>% 
  mutate(across(where(is.numeric), round, digits = 3)) %>% 
  filter(!effect == "ran_pars") %>% 
  select(!c(effect, statistic)) %>% 
  rename(Variable = term, Estimate = estimate, SE = std.error, p = p.value)


## Merge correlations
Clitics_Combined_Table <- rbind(Clitics_Gender_Table, Clitics_Number_Table) %>% 
  write_csv(here("JCL Nominal Manuscript", "Results", "JCL Nominal Clitics Gender and Number Results.csv"))


# Create plot
## Create averages
Clitics_Plot_Averages <- Clitics_Master %>% 
  group_by(Code, Status, Lang_Group, Exp_Gender, Exp_Number) %>%
  summarize(Total_Accuracy = sum(Accuracy, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy)),
            Ratio = Total_Accuracy/Total_Responses) %>% 
  mutate(Ratio = (Ratio*100))

Clitics_Plot_Averages_Mean <- Clitics_Plot_Averages %>% 
  group_by(Status, Lang_Group, Exp_Gender, Exp_Number) %>% 
  summarize(Ratio_Mean = mean(Ratio), .groups = "drop")

Joint_Clitics_Plot <- left_join(Clitics_Plot_Averages, Clitics_Plot_Averages_Mean, by = c("Lang_Group", "Status", "Exp_Gender", "Exp_Number"))

Joint_Clitics_Plot$Lang_Group <- factor(Joint_Clitics_Plot$Lang_Group, levels = c("Monolingual", "Bilingual"))
Joint_Clitics_Plot$Status <- factor(Joint_Clitics_Plot$Status, levels = c("TD", "DLD"))
Joint_Clitics_Plot$Exp_Gender <- factor(Joint_Clitics_Plot$Exp_Gender, levels = c("Masculine", "Feminine"))
Joint_Clitics_Plot$Exp_Number <- factor(Joint_Clitics_Plot$Exp_Number, levels = c("Singular", "Plural"))


## Generate plot without averages
Clitics_Boxplot_Gender_Number <- Joint_Clitics_Plot %>% 
  ggplot(aes(x = Exp_Gender, y = Ratio, fill = Exp_Number)) +
  geom_boxplot() +
  facet_grid(vars(Status), vars(Lang_Group)) +
  scale_fill_manual(values = c("#BF5700", "#F4EFE0")) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  labs(x = "Structure", y = "Percentage of target responses", title = "Clitic Production by Expected Gender and Number", fill = "Number") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"))

Clitics_Boxplot_Gender_Number

ggsave(filename = here("JCL Nominal Manuscript", "Graphs", "JCL Nominal Figure 5.pdf"),
       plot = Clitics_Boxplot_Gender_Number,
       device = "pdf",
       width = 6.5,
       height = 3.5,
       units = "in")


# Calculate percentages
Clitics_Percentages_Table <- Clitics_Master %>% 
  group_by(Status, Lang_Group, Exp_Gender, Exp_Number) %>% 
  summarize(Mean = mean(Accuracy, na.rm = TRUE)) %>% 
  mutate(Percentage = (Mean*100))
