library(tidyverse)
library(here)
library(patchwork)


# Load data
Data <- read_csv(here("CSV Files", "Aggregated Long Data.csv")) %>% 
  drop_na(Structure) %>% 
  filter(Structure == "Verbal Agreement")
Data$Group <- factor(Data$Group, levels = c("ML-TD", "ML-DLD", "BL-TD", "BL-DLD"))


# Prepare averages by structures
## Bar graph by individual
Bar_Graph_Data <- Data %>% 
  group_by(Code, Group, Structure) %>%
  summarize(Total_Accuracy = sum(Accuracy, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy)),
            Ratio = Total_Accuracy/Total_Responses) %>% 
  mutate(Ratio = (Ratio*100)) %>% 
  group_by(Group, Structure) %>%
  summarise(Average = mean(Ratio, na.rm = TRUE), 
            SD = sd(Ratio, na.rm = TRUE), 
            .groups = "drop")

Bar_Graph_Data$Structure <- factor(Bar_Graph_Data$Structure, levels = c("Verbal Agreement", "Subjunctive"))


## Boxplot by participant
Boxplot_Data <- Data %>% 
  group_by(Code, Group, Structure) %>%
  summarize(Total_Accuracy = sum(Accuracy, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy)),
            Ratio = Total_Accuracy/Total_Responses) %>% 
  mutate(Ratio = (Ratio*100))

Boxplot_Data$Structure <- factor(Boxplot_Data$Structure, levels = c("Verbal Agreement", "Subjunctive"))


# Generate bar graph
Bar_Graph <- Bar_Graph_Data %>% 
  ggplot(aes(x = Group, y = Average, fill = Group)) + 
  geom_bar(position = "dodge", color = "black", stat = "identity") +
  scale_fill_manual(values = c("#FFFD78", "#76D6FF", "#FF7E79", "#009193")) +
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 102)) +
  geom_text(aes(label = paste0(round(Average), "\n(", round(SD), ")")),
            position = position_dodge(width = .9),
            vjust = 0.5,
            size = 3,
            fontface = "bold") +
  labs(x = "Average (SD) by group", y = "Percentage of target responses", fill = "Group") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none",
        strip.text = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"))

Bar_Graph


# Generate boxplot
Boxplot <- Boxplot_Data %>% 
  ggplot(aes(x = Group, y = Ratio, fill = Group)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("#FFFD78", "#76D6FF", "#FF7E79", "#009193")) +
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) +
  labs(x = "Distribution by group") +
  guides(fill = "none") +
  theme(
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    strip.text.x = element_text(face = "bold"),
    axis.title.y = element_blank())


Boxplot


# Join plots
Group_Plot <- (Bar_Graph + Boxplot) + 
  plot_annotation(title = "Statistical Summary of Responses by Group and Structure") & 
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

Group_Plot

ggsave(filename = here("JCL Verbal Manuscript", "Graphs", "JCL Verbal Figure 1.pdf"),
       plot = Group_Plot,
       device = "pdf",
       width = 6.5,
       height = 3,
       units = "in")

