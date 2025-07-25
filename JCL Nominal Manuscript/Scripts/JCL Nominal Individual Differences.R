library(tidyverse)
library(here)

options(scipen = 99)


# Prepare dataframes by structure
Articles <- read_csv(here("CSV Files", "Aggregated Long Data.csv")) %>% 
  filter(Structure == c("Articles")) %>% 
  filter(!is.na(Accuracy)) %>%
  group_by(Code, Group, CA_Months, Structure) %>%
  summarize(Total_Target = sum(Accuracy, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy)),
            Ratio = Total_Target/Total_Responses) %>% 
  mutate(Ratio = Ratio*100,
         Structure = "Articles")

Clitics <- read_csv(here("CSV Files", "Aggregated Long Data.csv")) %>% 
  filter(Structure == c("Clitics")) %>% 
  filter(!is.na(Accuracy)) %>%
  group_by(Code, Group, CA_Months, Structure) %>%
  summarize(Total_Target = sum(Accuracy, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy)),
            Ratio = Total_Target/Total_Responses) %>% 
  mutate(Ratio = Ratio*100,
         Structure = "Clitics")


# Join dataframes
Joint <- rbind(Articles, Clitics)
Joint$Group <- factor(Joint$Group, levels = c("ML-TD", "ML-DLD", "BL-TD", "BL-DLD"))


# Create graph
## Without group space tracings
Ind_Differences <- Joint %>%
  ggplot(aes(x = CA_Months, y = Ratio, fill = Group)) +
  geom_jitter(shape = 21, color = "black", width = 0.5, height = 0, size = 2) +
  scale_fill_manual(values = c("#FFFD78", "#76D6FF", "#FF7E79", "#009193")) +
  facet_grid(rows = vars(Structure)) +
  scale_x_continuous(breaks = seq(48, 84, 6), expand = expansion(mult = 0.05)) +
  scale_y_continuous(breaks = seq(0, 100, 20), expand = expansion(mult = 0.05)) +
  labs(x = "Age in months",
       y = "Percentage of target responses",
       title = "Individual Rates of Article and Clitic Production by Age",
       fill = "Group") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 10))

Ind_Differences


## With group spaces
Ind_Differences_Circles <- Joint %>%
  ggplot(aes(x = CA_Months, y = Ratio)) +
  facet_grid(rows = vars(Structure)) +
  geom_jitter(aes(fill = Group), shape = 21, color = "black", width = 0.5, height = 0, size = 2.5) +
  stat_ellipse(aes(group = Group, color = Group),
               type = "norm",
               level = 0.95,
               size = 0.6,
               fill = NA,
               linetype = "solid") +
  scale_fill_manual(values = c("#FFFD78", "#76D6FF", "#FF7E79", "#009193")) +
  scale_color_manual(values = c("#FFFD78", "#76D6FF", "#FF7E79", "#009193")) +
  scale_x_continuous(breaks = seq(48, 84, 6), expand = expansion(mult = 0.05)) +
  scale_y_continuous(breaks = seq(0, 100, 20), expand = expansion(mult = 0.05)) +
  labs(x = "Age in months",
       y = "Percentage of target responses",
       title = "Individual Rates of Target Verbal Agreement by Age",
       fill = "Group",
       color = "Group") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 10))

Ind_Differences_Circles

ggsave(filename = here("JCL Nominal Manuscript", "Graphs", "JCL Nominal Figure 6.pdf"),
       plot = Ind_Differences_Circles,
       device = "pdf",
       width = 6.5,
       height = 5,
       units = "in")
