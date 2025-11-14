library(here)
library(tidyverse)
library(patchwork)


# Load data
Data <- read_csv(here("CSV Files", "Aggregated Long Data.csv")) %>% 
  drop_na(Structure) %>% 
  filter(!Structure == "Subjunctive")
Data$Group <- factor(Data$Group, levels = c("ML-TD", "ML-DLD", "BL-TD", "BL-DLD"))
Data$Structure <- factor(Data$Structure, levels = c("Articles", "Clitics", "Verbal Agreement", "Subjunctive"))



# Prepare averages by structures
Master_Bar <- Data %>% 
  group_by(Group, Structure) %>%
  summarize(Total_Accuracy = sum(Accuracy, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy)),
            Ratio = Total_Accuracy/Total_Responses) %>% 
  mutate(Ratio = (Ratio*100))

Master_Box <- Data %>% 
  group_by(Code, Group, Structure) %>%
  summarize(Total_Accuracy = sum(Accuracy, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy)),
            Ratio = Total_Accuracy/Total_Responses) %>% 
  mutate(Ratio = (Ratio*100))


# Summary of SDs for bar graph
Master_Bar_Summary <- Master_Box %>%
  group_by(Structure, Group) %>%
  summarise(Average = mean(Ratio, na.rm = TRUE), 
            SD = sd(Ratio, na.rm = TRUE), 
            .groups = "drop")


# Create bar graph
Bar_Graph <- Master_Bar_Summary %>% 
  ggplot(aes(x = Structure, y = Average, fill = Group)) + 
  geom_bar(position = "dodge", color = "black", stat = "identity") +
  scale_fill_manual(values = c("#FFFD78", "#76D6FF", "#FF7E79", "#009193")) +
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 102)) +
  geom_text(aes(label = paste0(round(Average), "\n(", round(SD), ")")),
            position = position_dodge(width = .9),
            vjust = 0.5,
            size = 2.5,
            fontface = "bold") +
  labs(x = "Structure",
       y = "Percentage target responses",
       fill = "Group",
       title = "Average (SE) Production by Structure and Group") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"))

Bar_Graph

ggsave(filename = here("Conferences", "HLS 2025", "Graphs", "HLS 2025 Bar Graph.pdf"),
       plot = Bar_Graph,
       device = "pdf",
       width = 7.5,
       height = 2.5,
       units = "in")


# Generate boxplot
Boxplot <- Master_Box %>% 
  ggplot(aes(x = Structure, y = Ratio, fill = Group)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FFFD78", "#76D6FF", "#FF7E79", "#009193")) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  labs(x = "Structure",
       y = "Percentage target responses",
       fill = "Group",
       title = "Distribution of Production by Structure and Group") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"))

Boxplot

ggsave(filename = here("Conferences", "HLS 2025", "Graphs", "HLS 2025 Boxplot.pdf"),
       plot = Boxplot,
       device = "pdf",
       width = 7.5,
       height = 2.5,
       units = "in")
