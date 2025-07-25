library(here)
library(tidyverse)
library(patchwork)


# Load data
Data <- read_csv(here("CSV Files", "Aggregated Long Data.csv")) %>% 
  drop_na(Structure) %>% 
  filter(Structure == c("Articles", "Clitics"))
Data$Group <- factor(Data$Group, levels = c("ML-TD", "ML-DLD", "BL-TD", "BL-DLD"))


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
  scale_fill_manual(values = c("#FFFD78", "#76D6FF", "#FF7E79", "#009193")) +
  geom_bar(position = "dodge", color = "black", stat = "identity") +
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 102)) +
  geom_text(aes(label = paste0(round(Average), "\n(", round(SD), ")")),
            position = position_dodge(width = .9),
            vjust = 0.5,
            size = 2.75,
            fontface = "bold") +
  labs(x = "Average (SD) by structure", y = "Percentage of target responses", fill = "Group") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none",
        strip.text = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"))

Bar_Graph


# Generate boxplot
Boxplot <- Master_Box %>% 
  ggplot(aes(x = Structure, y = Ratio, fill = Group)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FFFD78", "#76D6FF", "#FF7E79", "#009193")) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  labs(x = "Distribution by structure") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"),
        axis.title.y = element_blank())

Boxplot


# Create joint plot
Group_Plot <- (Bar_Graph + Boxplot) + 
  plot_annotation(title = "Statistical Summary of Responses by Group and Structure") & 
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

Group_Plot

ggsave(filename = here("JCL Nominal Manuscript", "Graphs", "JCL Nominal Figure 1.pdf"),
       plot = Group_Plot,
       device = "pdf",
       width = 6.5,
       height = 3,
       units = "in")

