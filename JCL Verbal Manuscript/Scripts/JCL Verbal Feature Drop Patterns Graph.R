library(here)
library(tidyverse)


# Load and prepare data
Overall <- read_csv(here("JCL Verbal Manuscript", "JCL Verbal Agreement Analysis Data.csv")) %>% 
  filter(Accuracy == 0) %>% 
  mutate(Exp_Inflection = paste(Exp_Person, Exp_Number, sep = " Person ")) %>% 
  mutate(Exp_Inflection = case_when(
    Exp_Inflection == "First Person Singular"  ~ "Expected 1PS",
    Exp_Inflection == "First Person Plural"    ~ "Expected 1PP",
    Exp_Inflection == "Second Person Singular" ~ "Expected 2PS",
    Exp_Inflection == "Third Person Singular"  ~ "Expected 3PS",
    Exp_Inflection == "Third Person Plural"    ~ "Expected 3PP",
    TRUE ~ Exp_Inflection))

Nonfinite <- Overall %>% 
  filter(Nonfinite == 1) %>% 
  mutate(Substitution = case_when(Infinitive == 1 ~ "Infinitive",
                                  Gerund == 1 ~ "Gerund"))

Substitution <- Overall %>%
  filter(!Substitution == 0)

Master <- rbind(Nonfinite, Substitution) %>% 
  rename(Non_Target = Substitution)


# Create graphs by person and number
Master_Table <- Master %>% 
  group_by(Exp_Inflection, Group, Non_Target, Participant_Count) %>%
  summarize(Count = n(), .groups = "drop") %>%
  mutate(Count_Adjusted = Count/Participant_Count)

Master_Table$Group <- factor(Master_Table$Group, levels = c("ML-TD", "ML-DLD", "BL-TD", "BL-DLD"))
Master_Table$Exp_Inflection <- factor(Master_Table$Exp_Inflection, levels = c("Expected 1PS", "Expected 1PP", "Expected 2PS", "Expected 3PS", "Expected 3PP"))
Master_Table$Non_Target <- factor(Master_Table$Non_Target, levels = c("1PS", "1PP", "2PS", "3PS", "3PP", "Infinitive", "Gerund"))


## Graph by count
Master_Table_Graph_Count <- Master_Table %>% 
  ggplot(aes(x = Group, y = Count_Adjusted, fill = Non_Target)) + 
  geom_bar(position = "dodge", color = "black", stat = "identity") +
  facet_grid(rows = vars(Exp_Inflection)) +
  geom_text(aes(label = paste0(round(Count_Adjusted, 2))),
            position = position_dodge(width = .9),
            vjust = -0.5,
            size = 2.5,
            fontface = "bold") +
  scale_y_continuous(breaks = seq(0, 1, 0.2),
                     limits = c(0, 1)) +
  labs(x = "Group",
       y = "Number of substitutions per participant",
       fill = "Form produced",
       title = "Substitutions by Group and Expected Inflection") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))

Master_Table_Graph_Count

ggsave(filename = here("JCL Verbal Manuscript", "Graphs", "JCL Verbal Figure 2.pdf"),
       plot = Master_Table_Graph_Count,
       device = "pdf",
       width = 6.5,
       height = 5.5,
       units = "in")


## Table of percentages by structure
Structure_Breakdown <- Master_Table %>% 
  group_by(Group, Non_Target) %>% 
  summarize(Total = sum(Count)) %>%
  mutate(Percentage = (Total / sum(Total)) * 100) %>%
  ungroup() %>%
  add_row(Non_Target = "Grand Total", 
          Total = sum(.$Total), 
          Percentage = 100) %>% 
  write_csv(here("JCL Verbal Manuscript", "Results", "JCL Verbal Non-Target Responses Distribution.csv"))

