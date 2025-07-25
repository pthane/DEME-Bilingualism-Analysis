library(tidyverse)
library(here)
library(lme4)
library(lmerTest)
library(broom.mixed)

options(scipen = 99)


# Load CSV files
Clitics <- read_csv("./CSV Files/Aggregated Long Data.csv") %>% 
  filter(Structure == "Clitics") %>% 
  filter(Status == "DLD")


# Prepare items
Clitics_1 <- Clitics %>% 
  filter(Item == "Clitic_1") %>% 
  mutate(Exp_Gender = "Masculine",
         Exp_Number = "Singular")

Clitics_2 <- Clitics %>% 
  filter(Item == "Clitic_2") %>% 
  mutate(Exp_Gender = "Feminine",
         Exp_Number = "Singular")

Clitics_4 <- Clitics %>% 
  filter(Item == "Clitic_4") %>% 
  mutate(Exp_Gender = "Masculine",
         Exp_Number = "Plural")

Clitics_5 <- Clitics %>% 
  filter(Item == "Clitic_5") %>% 
  mutate(Exp_Gender = "Feminine",
         Exp_Number = "Plural")

Clitics_6 <- Clitics %>% 
  filter(Item == "Clitic_6") %>% 
  mutate(Exp_Gender = "Masculine",
         Exp_Number = "Plural")

Clitics_7 <- Clitics %>% 
  filter(Item == "Clitic_7") %>% 
  mutate(Exp_Gender = "Masculine",
         Exp_Number = "Singular")

Clitics_8 <- Clitics %>% 
  filter(Item == "Clitic_8") %>% 
  mutate(Exp_Gender = "Feminine",
         Exp_Number = "Plural")

Clitics_9 <- Clitics %>% 
  filter(Item == "Clitic_9") %>% 
  mutate(Exp_Gender = "Feminine",
         Exp_Number = "Singular")

Clitics_10 <- Clitics %>% 
  filter(Item == "Clitic_10") %>% 
  mutate(Exp_Gender = "Masculine",
         Exp_Number = "Singular")

Clitics_11 <- Clitics %>% 
  filter(Item == "Clitic_11") %>% 
  mutate(Exp_Gender = "Feminine",
         Exp_Number = "Singular")


## Join numbers
Clitics_Joint <- rbind(Clitics_1, Clitics_2, Clitics_4, Clitics_5, Clitics_6, Clitics_7, Clitics_8, Clitics_9, Clitics_10, Clitics_11)


# Create correlations
DLD_Correlation <- glmer(Accuracy ~ Exp_Gender +
                                (1 | Code) + (1 | Item),
                              family = "binomial",
                              data = Clitics_Joint)

summary(DLD_Correlation)


# Create table
Clitics_Results_Table <- tidy(DLD_Correlation, effects = "fixed") %>% 
  rename(Effect = term,
         Estimate = estimate,
         SE = std.error,
         p = p.value) %>% 
  select(Effect, Estimate, SE, p) %>% 
  mutate(Estimate = round(Estimate, 2),
         SE = round(SE, 2),
         p = round(p, 3)) %>% 
  write_csv(here("JCL Nominal Manuscript", "Results", "JCL Nominal Clitics Post-Hoc Analysis.csv"))
