### Problem Set 3 Regression Model ###
# Author: Ziyue Yang				 #
# Date: October 19, 2020			 #
# Student Number: 1004804759		 #
# Email: ziyue.yang@mail.utoronto.ca #
######################################

set.seed(4759)
library(tidyverse)

# Read the cleaned csv
df <- read_csv("output/gss-time-stress.csv")

# Removing "Not stated" and "Don't know" entries
for (i in colnames(df)) {
  df <- filter(df, df[i] != "Not stated")
  df <- filter(df, df[i] != "Don't know")
}

# Adding dummy variables for response
df <- df %>% mutate(mental_dummy = ifelse(
  (mental_health == '... excellent?' | mental_health == '... very good?'), 1, 0))

income_intervals <- c("No income", "Less than $5,000",
   "$5,000 to $9,999", "$10,000 to $14,999",
   "$15,000 to $19,999", "$20,000 to $29,999",
   "$30,000 to $39,999", "$40,000 to $49,999",
   "$50,000 to $59,999", "$60,000 to $79,999",
   "$80,000 to $99,999", "$100,000 or more")

df <- df %>% mutate(uniform_income = case_when(annual_income == income_intervals[1] ~ 0,
                              annual_income == income_intervals[2] ~ runif(1, 1, 5000),
                              annual_income == income_intervals[3] ~ runif(1, 5000, 9999),
                              annual_income == income_intervals[4] ~ runif(1, 10000, 14999),
                              annual_income == income_intervals[5] ~ runif(1, 15000, 19999),
                              annual_income == income_intervals[6] ~ runif(1, 20000, 29999),
                              annual_income == income_intervals[7] ~ runif(1, 30000, 39999),
                              annual_income == income_intervals[8] ~ runif(1, 40000, 49999),
                              annual_income == income_intervals[9] ~ runif(1, 50000, 59999),
                              annual_income == income_intervals[10] ~ runif(1, 60000, 79999),
                              annual_income == income_intervals[11] ~ runif(1, 80000, 99999),
                              annual_income == income_intervals[12] ~ 100000,
                              ))

logit_model <- glm(mental_dummy ~ uniform_income + age_10 + marital + gender, data = df, family = "binomial")
summary(logit_model)
