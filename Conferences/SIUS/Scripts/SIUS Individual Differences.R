library(here)
library(tidyverse)
library(lme4)
library(lmerTest)

options(scipen = 99)


# Prepare data
## Load CSV
Data <- read_csv(here("CSV Files", "Aggregated Long Data.csv")) %>% 
  filter(Lang_Group == "Bilingual")
Data <- Data[!is.na(Data$Structure),]
Data$Group <- factor(Data$Group, levels = c("BL-TD", "BL-DLD"))
Data$Structure <- factor(Data$Structure, c("Adjectives", "Articles", "Clitics", "Verbal Agreement", "Subjunctive"))


## Prepare averages by structures
Averages <- Data %>% 
  group_by(Code, Status, Structure) %>%
  summarize(Total_Accuracy = sum(Accuracy, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy)),
            Ratio = Total_Accuracy/Total_Responses) %>% 
  mutate(Ratio = (Ratio*100))


## Prepare individual variables
Load_Variables <- read_csv(here("CSV Files", "Aggregated Long Data.csv")) %>% 
  filter(Lang_Group == "Bilingual") %>% 
  filter(Item == "Article_1") %>% 
  select(Code, Relative_Morphosyntax)

Master <- left_join(Averages, Load_Variables, by = "Code")
Master$Structure <- factor(Master$Structure, c("Adjectives", "Articles", "Clitics", "Verbal Agreement", "Subjunctive"))


# Generate relative PGU plot
Morphosyntactic_Dominance <- Master %>% 
  ggplot(aes(x = Relative_Morphosyntax, y = Ratio, color = Status)) +
  facet_wrap(vars(Structure), 
             labeller = labeller(Structure = c("Articles" = "Artículos",
                                               "Clitics" = "Clíticos", 
                                               "Verbal Agreement" = "Concordancia verbal",
                                               "Subjunctive" = "Subjuntivo"))) +
  geom_jitter(mapping = aes(color = Status)) +
  geom_smooth(method = glm) +
  scale_x_continuous(breaks = seq(-20, 60, 20), 
                     limits = c(-20, 60)) +
  scale_y_continuous(breaks = seq(0, 100, 20), 
                     limits = c(-4, 104)) +
  scale_color_discrete(labels = c("DLD" = "TDL", "TD" = "DT")) +
  labs(x = "Gramaticalidad relativa entre español e inglés", 
       y = "Porcentaje de respuestas anticipadas", 
       title = "Diferencias individuales en bilingües por gramaticalidad relativa", 
       color = "Trastorno") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))

Morphosyntactic_Dominance

ggsave(filename = here("Conferences", "SIUS", "Graphs", "Morphosyntax.pdf"),
       plot = Morphosyntactic_Dominance,
       device = "pdf",
       width = 8,
       height = 6,
       units = "in")


# Run correlation
Individual_Differences_Correlation <- glmer(Accuracy ~ Group * Relative_Morphosyntax +
                                  (1 | Code) + (1 | Item),
                                data = Data,
                                family = "binomial")

summary(Individual_Differences_Correlation)

Individual_Differences_Table <- tidy(Individual_Differences_Correlation, effects = "fixed") %>%
  bind_rows(tidy(Individual_Differences_Correlation, effects = "ran_pars")) %>% 
  mutate(across(where(is.numeric), round, digits = 3)) %>% 
  filter(effect != "ran_pars") %>%
  select(-effect, -statistic) %>%
  rename(Variable = term, Estimado = estimate, Error_est = std.error, p = p.value) %>% 
  write_csv(here("Conferences", "SIUS", "Results", "SIUS Individual Differences GLMM.csv"))


  