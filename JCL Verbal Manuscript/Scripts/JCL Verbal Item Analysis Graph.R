library(tidyverse)
library(here)
library(patchwork)


# Load data
Data <- read_csv(here("JCL Verbal Manuscript", "JCL Verbal Agreement Analysis Data.csv")) %>%
  mutate(Item_num = as.numeric(str_extract(Item, "(?<=Verbal Agreement-)\\d+")),
    Short_Code = case_when(
      Exp_Person == "First" & Exp_Number == "Singular" ~ "1PS",
      Exp_Person == "First" & Exp_Number == "Plural" ~ "1PP",
      Exp_Person == "Second" & Exp_Number == "Singular" ~ "2PS",
      Exp_Person == "Third" & Exp_Number == "Singular" ~ "3PS",
      Exp_Person == "Third" & Exp_Number == "Plural" ~ "3PP",
      TRUE ~ NA_character_)) %>%
  filter(!is.na(Item_num), !is.na(Short_Code)) %>%
  mutate(Item = paste0(Item_num, " (", Short_Code, ")"),
         Item = factor(Item, levels = unique(Item[order(Item_num)]))) %>%
  select(-Item_num)
Data$Group <- factor(Data$Group, levels = c("ML-TD", "ML-DLD", "BL-TD", "BL-DLD"))


# Prepare averages by structures
## Bar graph by individual
Bar_Graph_Data <- Data %>% 
  group_by(Code, Group, Item) %>%
  summarize(Total_Accuracy = sum(Accuracy, na.rm = TRUE),
            Total_Responses = sum(!is.na(Accuracy)),
            Ratio = Total_Accuracy/Total_Responses) %>% 
  mutate(Ratio = (Ratio*100)) %>% 
  group_by(Group, Item) %>%
  summarise(Average = mean(Ratio, na.rm = TRUE), 
            SD = sd(Ratio, na.rm = TRUE), 
            .groups = "drop")


# Generate bar graph
Item_Analysis <- Bar_Graph_Data %>% 
  ggplot(aes(x = Item, y = Average, fill = Group)) + 
  geom_bar(position = "dodge", color = "black", stat = "identity") +
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 102)) +
  geom_text(aes(label = paste0(round(Average), "\n(", round(SD), ")")),
            position = position_dodge(width = .9),
            vjust = 0.5,
            size = 2.5,
            fontface = "bold") +
  labs(x = "Item", y = "Percentage of target responses", fill = "Group", title = "Production of Target-Like Verbal Agreement by Group and Item") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.text.x = element_text(face = "bold"))

Item_Analysis

ggsave(filename = here("JCL Verbal Manuscript", "Graphs", "JCL Verbal Item Analysis.pdf"),
       plot = Item_Analysis,
       device = "pdf",
       width = 10,
       height = 3.5,
       units = "in")

