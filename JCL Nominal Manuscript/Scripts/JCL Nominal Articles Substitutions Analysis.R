library(here)
library(tidyverse)
library(lme4)
library(lmerTest)

options(scipen = 99)


# Load data
Articles_Overall <- read_csv(here("CSV Files", "Aggregated Long Data.csv")) %>% 
  filter(Structure == "Articles")
Articles_Overall$Status <- factor(Articles_Overall$Status, levels = c("TD", "DLD"))
Articles_Overall$Lang_Group <- factor(Articles_Overall$Lang_Group, levels = c("Monolingual", "Bilingual"))


# Prepare items
Articles_1 <- Articles_Overall %>% 
  filter(Item == "Article_1") %>% 
  mutate(Exp_Gender = "Feminine",
         Exp_Number = "Singular",
         Exp_Definiteness = "Definite")

Articles_2 <- Articles_Overall %>% 
  filter(Item == "Article_2") %>% 
  mutate(Exp_Gender = "Masculine",
         Exp_Number = "Plural",
         Exp_Definiteness = "Definite")

Articles_3 <- Articles_Overall %>% 
  filter(Item == "Article_3") %>% 
  mutate(Exp_Gender = "Masculine",
         Exp_Number = "Plural",
         Exp_Definiteness = "Definite")

Articles_4 <- Articles_Overall %>% 
  filter(Item == "Article_4") %>% 
  mutate(Exp_Gender = "Feminine",
         Exp_Number = "Plural",
         Exp_Definiteness = "Definite")

Articles_5 <- Articles_Overall %>% 
  filter(Item == "Article_5") %>% 
  mutate(Exp_Gender = "Feminine",
         Exp_Number = "Plural",
         Exp_Definiteness = "Definite")

Articles_6 <- Articles_Overall %>% 
  filter(Item == "Article_6") %>% 
  mutate(Exp_Gender = "Masculine",
         Exp_Number = "Singular",
         Exp_Definiteness = "Definite")

Articles_10 <- Articles_Overall %>% 
  filter(Item == "Article_10") %>% 
  mutate(Exp_Gender = "Masculine",
         Exp_Number = "Singular",
         Exp_Definiteness = "Indefinite")

Articles_12 <- Articles_Overall %>% 
  filter(Item == "Article_12") %>% 
  mutate(Exp_Gender = "Masculine",
         Exp_Number = "Singular",
         Exp_Definiteness = "Indefinite")


## Join numbers
Articles_Master <- rbind(Articles_1, Articles_2, Articles_3, Articles_4, Articles_5, Articles_6, Articles_10, Articles_12)
Articles_Master$Group <- factor(Articles_Master$Group, levels = c("ML-TD", "ML-DLD", "BL-TD", "BL-DLD"))
Articles_Master$Lang_Group <- factor(Articles_Master$Lang_Group, levels = c("Monolingual", "Bilingual"))
Articles_Master$Status <- factor(Articles_Master$Status, levels = c("TD", "DLD"))


# Generate correlations
## Gender
Articles_Gender_Correlation <- glmer(Accuracy ~ + Exp_Gender + Lang_Group + Status + Exp_Gender:Lang_Group + Exp_Gender:Status +
                               (1 | Code) + (1 | Item),
                             family = "binomial",
                             data = Articles_Master)

summary(Articles_Gender_Correlation)

Articles_Gender_Table <- tidy(Articles_Gender_Correlation, effects = "fixed") %>% 
  mutate(Model = "Article Gender") %>% 
  mutate(across(where(is.numeric), round, digits = 3)) %>% 
  filter(!effect == "ran_pars") %>% 
  select(!c(effect, statistic)) %>% 
  rename(Variable = term, Estimate = estimate, SE = std.error, p = p.value)


## Number
Articles_Number_Correlation <- glmer(Accuracy ~ + Exp_Number + Lang_Group + Status + Exp_Number:Lang_Group + Exp_Number:Status +
                                      (1 | Code) + (1 | Item),
                                    family = "binomial",
                                    data = Articles_Master)

summary(Articles_Number_Correlation)

Articles_Number_Table <- tidy(Articles_Number_Correlation, effects = "fixed") %>% 
  mutate(Model = "Article Number") %>% 
  mutate(across(where(is.numeric), round, digits = 3)) %>% 
  filter(!effect == "ran_pars") %>% 
  select(!c(effect, statistic)) %>% 
  rename(Variable = term, Estimate = estimate, SE = std.error, p = p.value)


## Merge correlations
Articles_Combined_Table <- rbind(Articles_Gender_Table, Articles_Number_Table) %>% 
  write_csv(here("JCL Nominal Manuscript", "Results", "JCL Nominal Articles Gender and Number Results.csv"))



## Definiteness
Articles_Definiteness_Correlation <- glmer(Accuracy ~ + Exp_Definiteness + Lang_Group + Status + Exp_Definiteness:Lang_Group + Exp_Definiteness:Status +
                                       (1 | Code) + (1 | Item),
                                     family = "binomial",
                                     data = Articles_Master)

summary(Articles_Definiteness_Correlation)



# Create plot
## Create averages
Articles_Plot_Averages <- Articles_Master %>% 
  group_by(Code, Status, Lang_Group, Exp_Gender, Exp_Number) %>%
  summarize(Total_Accuracy = sum(Accuracy, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy)),
            Ratio = Total_Accuracy/Total_Responses) %>% 
  mutate(Ratio = (Ratio*100))

Articles_Plot_Averages_Mean <- Articles_Plot_Averages %>% 
  group_by(Status, Lang_Group, Exp_Gender, Exp_Number) %>% 
  summarize(Ratio_Mean = mean(Ratio), .groups = "drop")

Joint_Articles_Plot <- left_join(Articles_Plot_Averages, Articles_Plot_Averages_Mean, by = c("Lang_Group", "Status", "Exp_Gender", "Exp_Number"))

Joint_Articles_Plot$Lang_Group <- factor(Joint_Articles_Plot$Lang_Group, levels = c("Monolingual", "Bilingual"))
Joint_Articles_Plot$Status <- factor(Joint_Articles_Plot$Status, levels = c("TD", "DLD"))
Joint_Articles_Plot$Exp_Gender <- factor(Joint_Articles_Plot$Exp_Gender, levels = c("Masculine", "Feminine"))
Joint_Articles_Plot$Exp_Number <- factor(Joint_Articles_Plot$Exp_Number, levels = c("Singular", "Plural"))


## Generate plot without averages
Articles_Boxplot_Gender_Number <- Joint_Articles_Plot %>% 
  ggplot(aes(x = Exp_Gender, y = Ratio, fill = Exp_Number)) +
  geom_boxplot() +
  facet_grid(vars(Status), vars(Lang_Group)) +
  scale_fill_manual(values = c("#BF5700", "#F4EFE0")) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  labs(x = "Structure", y = "Percentage of target responses", title = "Article Production by Expected Gender and Number", fill = "Number") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"))

Articles_Boxplot_Gender_Number

ggsave(filename = here("JCL Nominal Manuscript", "Graphs", "JCL Nominal Figure 4.pdf"),
       plot = Articles_Boxplot_Gender_Number,
       device = "pdf",
       width = 6.5,
       height = 3.5,
       units = "in")


# Calculate percentages
Articles_Percentages_Table <- Articles_Master %>% 
  group_by(Group) %>% 
  summarize(Mean = mean(Accuracy, na.rm = TRUE), SD = sd(Accuracy, na.r = TRUE)) %>% 
  mutate(Mean_Percentage = (Mean*100),
         SD_Percentage = (SD*100))

Articles_Percentages_Table_Gender_Number <- Articles_Master %>% 
  group_by(Status, Lang_Group, Exp_Gender, Exp_Number) %>% 
  summarize(Mean = mean(Accuracy, na.rm = TRUE), SD = sd(Accuracy, na.r = TRUE)) %>% 
  mutate(Mean_Percentage = (Mean*100),
         SD_Percentage = (SD*100))
