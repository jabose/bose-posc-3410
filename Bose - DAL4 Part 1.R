# Title: DAL4 Part 1 
# Author: Jacob A Bose
# Author's Email: jabose@clemson.edu
# Date: 14 Feb 2021

# Set Up####
# Libraries
library(tidyverse)

# Data 
# Load your Data and assign to gss_df ####
gss_df

# Script ####

# Question A ####
# View the data and read the code book to answer the following question. What do the columns and rows represent. Assign you answer as a text vector the below object. 

AnswerA <- "The columns represent the questions asked and the respondants' answers. The rows represent each respondant."

# Question B ####
# What is the start and end year present in the data? Code as a NUMERIC vector and assign to AnswerB. Plot how many observations per year there are. Assign your code to AnswerB_plot
AnswerB. <- c(1972, 2018)

AnswerB_plot <- gss_df %>% 
  group_by(YEAR) %>% 
  count() %>% 
  ggplot(aes(x=YEAR, y=n))+
  geom_bar(stat = "identity")
  
  # Question C ####
# Is every question asked each year? Use the code book or the GSS Data Explorer (link posted on Canvas) to answer this question. Code your answer as a LOGICAL vector and assign to AnswerC. 

AnswerC <- c(FALSE)
  
  # Question D ####
# What are the names of all the variables in the data frame? Assign your answer to col.names. 
col.names<- c("X1", "YEAR", "BALPOS", "BALNEG", "SCISTUDY", "SCITEXT", "COLSCI", "COLSCINM", "INTSCI", "INTSPACE", "SCIENTGO", "SCIENTHE", "SCIENTOD", "SCIENTBE", "REALRINC", "BALLOT", "SCIBNFTS", "ADVFRONT", "TOOFAST", "ID_", "AGE", "EDUC", "SEX", "RACE", "RINCOME", "PARTYID", "NATSPAC", "NATEDUC", "GOD", "NEWSFROM", "SCIFROM", "SEEKSCI", "NEXTGEN", "WTSSALL")
  
# Question 1 ####
# Our ultimate goal is to understand what causes people to support federal funding for science. Our starting point for this inquiry should be to explore the dataset by developing a baseline understanding of the respondents and the sample. The most basic disaggregator in our society is gender. Make a plot of how many men and women respondents there in the sample overall. 

# When you make a plot that graphs number of respondents by sex, what do you expect to see. What ratio do you expect to see between men and women? Write your answer as a text vector and assign to Answer1_expectation. 

# Create Answer1_expectation
Answer1_expectation <- "I expect to see more women than men responding because I feel like women are more likely to answer surveys than men."

# Create Answer1_plot
Answer1_plot<- gss_df %>% 
  group_by(SEX) %>% 
  count() %>% 
  rename(Number = n, Sex = SEX) %>% 
  ggplot(aes(x = Sex, y=Number)) +
  geom_bar(stat="identity")

# Use a text vector to explain what is peculiar about the result that you see. 
Answer1 <- "There are almost 10,000 more female respondants than male."


# Question 2 ####
# Repeat the steps we used in Question 1 for SEX to explore RACE 

# Create Answer2 Expectation 
Answer2_expectation <- "I expect there to be more white respondants than black."

# Create Answer2_plot
Answer2_plot<- gss_df %>% 
  group_by(RACE) %>% 
  count() %>% 
  rename(Number = n, Race = RACE) %>% 
  ggplot(aes(x = Race, y=Number)) +
  geom_bar(stat="identity")

# Use a text vector to explain what is peculiar about the result that you see. 
Answer2 <- "There are significantly more white respondants than black or other, more than 40,000 more."

# Question 3 ####
# When using GSS data, it is vital that we compare weighted percentages. In this question we will learn more about sampling weights. 

# View the structure of the WTSSALL variable. What is peculiar about the values of this variable. Describe your answer in a text vector and assign to Question3_part1. 
str("WTSSALL")

Question3_part1 <- "The values are numbers, but it is a character vector."

# Find the description wtssall in the code book. What is this variable and why is it in the data? Describe your answer in a text vector and assign to Question3_part2.

Question3_part2 <- "This variable tells us the weights of the other variables in the survey. The survey must oversample some populations to get an accurate representation of the national population."

# Important Takeaway #### 
# Our takeaway should be that we will need to compare the proportions. The GSS Team has surveyed a representative sample of Americans, which required oversampling some demographic groups to make sure there was enough data. The goal of this survey is NOT to take a Census of how many Americans in each demographic there are; rather, its goal is to understand the views and opinions Americans of each demographic group. 

# Therefore, analyzing data in the GSS will require us to find what percentage of a demographic group hold a certain opinion.

# Question 4 - Pointing out Ballot Difference ####
# Further, each respondent is also given a different ballot, which includes a different set of questions. You can read more about this in the Codebook or online. But we need to be aware that for each question in each year, we may have to filter to keep only the results for specific ballots. 

# How many different values are there for `BALLOT`?
Question4A <- unique(gss_df$`BALLOT`)

# Let's say we want to understand which Ballots asked a specific question `ADVFRONT` (look up what it means in the codebook) in a specific year (2018), now let's use tidyverse to find this answer. 
gss_df %>% 
  filter(YEAR == 2018) %>% 
  group_by(`BALLOT`,`ADVFRONT`) %>% 
  count() %>% 
  View()

# Code a list of text vectors with the Ballots that asked this question. 
Question4B <- c("Ballot a", "Ballot b")

# As you can see, the GSS lends itself to cross-sectional study but makes time-series study difficult because we have to develop a data frame for each question and variable we want to study and then combine these data frames together. We will learn how to do this next week. 

# Question 4 ####
# Here IS how we want to compare data in the GSS within 1 year. We will use the 2018 data. 

# 1) How many respondents were male and female (fill in the blanks)
gss_df %>% 
  filter(YEAR == 2018 & (BALLOT == "Ballot a" | BALLOT == "Ballot b")) %>% 
  group_by(SEX) %>% 
  count()

# 2A) Now create a dataframe of Male respondents who answered specific Ballots. 
Male_ab_df <-gss_df %>% 
  filter(YEAR == 2018 & SEX == "Male" & (BALLOT == "Ballot a" | BALLOT == "Ballot b"))

# 2B) Now create a dataframe of Female respondents who answered specific Ballots. 
Female_ab_df <-gss_df %>% 
  filter(YEAR == 2018 & SEX == "Female" & (BALLOT == "Ballot a" | BALLOT == "Ballot b"))

# 3A) Group_by SEX and ADVFRONT, summarize data to calculate proportion of men who responded each value
Male_ADVFRONT_df<- Male_ab_df %>% 
  group_by(SEX, ADVFRONT) %>%
  select(SEX, ADVFRONT) %>% 
  count() %>% 
  summarise(prop = n / 691 * 100 )

# 3B) Group_by SEX and ADVFRONT, summarize data to calculate proportion of women who responded each value
Female_ADVFRONT_df <- Female_ab_df %>% 
  group_by(SEX, ADVFRONT) %>%
  select(SEX, ADVFRONT) %>% 
  count() %>% 
  summarise(prop = n / 868 * 100 )

# 4) Combine these dataframes using the bind_rows function 
ADVFRONT_by_SEX_df <-bind_rows(Male_ADVFRONT_df, Female_ADVFRONT_df)

# 5A) Create ggplot bar graph  
ADVFRONT_by_SEX_df %>% 
  ggplot(aes(x = SEX, y = prop, fill= ADVFRONT)) +
  geom_bar(stat="identity")

# 5B) Filter out "Not Applicable" & "No answer" and make a ggplot. 
ADVFRONT_by_SEX_df %>% 
  filter(!is.na("Not applicable") & !is.na("No answer")) %>% 
  ggplot(aes(x=SEX, y= prop, fill= ADVFRONT)) +
  geom_bar(stat="identity")

# 5C) What are some aspects of this visualization that are wonky? Describe in detail what you want to change about this graph in the future. Assing your answer to Answer5C 
Answer5C <- "I would rearrange the categories of answers so it flows from strongly agree to agree to don't know to disagree to strongly disagree or vice versa. The current setup of the visualization is hard to read and understand how much of the population generally agrees or disagrees."

# Copyright (c) Jacob Bose, 2021
