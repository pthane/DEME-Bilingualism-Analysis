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

Agreement <- read_csv(here("CSV Files", "Aggregated Long Data.csv")) %>% 
  filter(Structure == c("Verbal Agreement")) %>% 
  filter(!is.na(Accuracy)) %>%
  group_by(Code, Group, CA_Months, Structure) %>%
  summarize(Total_Target = sum(Accuracy, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy)),
            Ratio = Total_Target/Total_Responses) %>% 
  mutate(Ratio = Ratio*100)


# Join dataframes
Joint <- rbind(Articles, Clitics, Agreement)
Joint$Group <- factor(Joint$Group, levels = c("ML-TD", "ML-DLD", "BL-TD", "BL-DLD"))


# Create graph
## With group spaces
Ind_Differences_Circles <- Joint %>%
  ggplot(aes(x = CA_Months, y = Ratio)) +
  facet_grid(cols = vars(Structure)) +
  geom_jitter(aes(fill = Group),
              shape = 21, color = "black",
              width = 0.5, height = 0, size = 2.5) +
  stat_ellipse(aes(group = Group),
               type = "norm",
               level = 0.95,
               size = 1.25,
               color = "black",
               alpha = 0.25) +
  stat_ellipse(aes(group = Group, color = Group),
               type = "norm",
               level = 0.95,
               size = 0.6,
               linetype = "solid") +
  scale_fill_manual(values = c("#FFFD78", "#76D6FF", "#FF7E79", "#009193")) +
  scale_color_manual(values = c("#FFFD78", "#76D6FF", "#FF7E79", "#009193")) +
  scale_x_continuous(breaks = seq(48, 84, 6), expand = expansion(mult = 0.05)) +
  scale_y_continuous(breaks = seq(0, 100, 20), expand = expansion(mult = 0.05)) +
  labs(x = "Age in months",
       y = "Percentage of target responses",
       title = "Individual Rates of Target Inflection by Age",
       fill = "Group",
       color = "Group") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 10))

Ind_Differences_Circles

ggsave(filename = here("Conferences", "HLS 2025", "Graphs", "HLS 2025 Individual Differences.pdf"),
       plot = Ind_Differences_Circles,
       device = "pdf",
       width = 7,
       height = 4.5,
       units = "in")
