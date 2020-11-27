# Analyze election results

# Set up

library(tidyverse)
library(stringr)
raw_data <- read.csv("https://raw.githubusercontent.com/alex/nyt-2020-election-scraper/master/all-state-changes.csv")
# Basic data frame exploration
num_cols <- ncol(raw_data)
num_rows <- nrow(raw_data)
num_states <- length(unique(raw_data$state))
num_timestamps <- length(unique(raw_data$timestamp))

# Number of time stamps varies for each state
timestamps_by_state <- raw_data %>% 
  group_by(state) %>% 
  count()

# Formatting: split out state name from electoral votes
data <- raw_data %>% 
  separate(state, into = c("state", "ev"), " \\(") %>% 
  mutate(ev = parse_number(ev))

# How many reported time stamps exist for each state


# When did Biden take the lead in Georgia
ga_lead_time <- data %>% 
  filter(state == "Georgia (EV: 16)", leading_candidate == "Biden")

# What is the difference in votes in each state


# How do total votes change over time (by candidate)


