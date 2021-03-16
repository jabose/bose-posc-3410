# Title: DAL 5 Part 2
# Author: Jacob A. Bose
# Email: jabose@clemson.edu
# Date: 14 March 2021

# Purpose: To learn how to do chi-square tests on the GSS file.

# Set Up ####
# Libraries
library(tidyverse)

# Data
load("gss_df")

# Data Wrangling ####
analysis_df <- gss_df %>% 
  filter(YEAR == 2018 & !is.na(NEXTGEN) & !is.na(INTSPACE) & (NEXTGEN == "Agree" | NEXTGEN == "Strongly agree" | NEXTGEN == "Strongly disagree" | NEXTGEN == "Disagree") & (INTSPACE == "Moderately interested" | INTSPACE == "Very interested" | INTSPACE == "Not at all interested")) %>% 
  select(INTSPACE, NEXTGEN, WTSSALL) %>% 
  group_by(NEXTGEN, INTSPACE) %>% 
  summarise(count = sum(WTSSALL))

analysis_df <- analysis_df %>% 
  mutate(NEXTGEN = str_remove_all(NEXTGEN, "Strongly"),
         NEXTGEN = str_to_lower(NEXTGEN),
         NEXTGEN = str_trim(NEXTGEN, side = c("both"))) %>% 
  group_by(NEXTGEN, INTSPACE) %>% 
  summarise(count = sum(count))

analysis_df <- analysis_df %>% 
  mutate(INTSPACE = factor(INTSPACE))

frequency_table_df <- analysis_df %>% 
  spread(INTSPACE, count)

view(frequency_table_df)
