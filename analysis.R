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

# Important asDate function: 
# If date is in this format: 11/23/2020
# You can change it to readable form by passing the data like this:
# as.Date("11/23/2020", format = "%m/%d/%Y")

# Formatting: split out state name from electoral votes
# Add biden and trump vote columns
data <- raw_data %>% 
  separate(state, into = c("state", "ev"), " \\(") %>% 
  mutate(ev = parse_number(ev)) %>% 
  mutate(biden_votes = 
           if_else(leading_candidate_name == "Biden", # condition
                   leading_candidate_votes,   # if true 
                   trailing_candidate_votes   # if false
                  ),
            trump_votes = total_votes_count - biden_votes
)

# Quick check!!! very useful
data %>% 
  mutate(total_check = trump_votes + biden_votes,
         done_correctly = if_else(total_check == total_votes_count, 1, 0)) %>% 
  summarise(total_correct = (done_correctly))

# How many reported time stamps exist for each state


# When did Biden take the lead in Georgia
ga_lead_time <- data %>% 
  filter(state == "Georgia", leading_candidate_name == "Biden") %>% 
  filter(timestamp == min(timestamp)) %>% 
  pull(timestamp)
  
  
# Earliest time in each state that biden is ahead?
biden_lead_time <- data %>% 
  group_by(state) %>% 
  filter(leading_candidate_name == "Biden") %>% 
  filter(timestamp == min(timestamp)) %>% 
  select(state, timestamp)

# What is the difference in votes in each state
# at the most recent time stamp
vote_diff <- data %>%
  group_by(state) %>% 
  filter(timestamp == max(timestamp)) %>% 
  mutate(vote_diff = biden_votes - trump_votes,
         pct_diff = vote_diff / total_votes_count)

vote_diff_plot <- ggplot(vote_diff) +
  geom_col(aes(x = vote_diff, 
                         y = reorder(state, vote_diff),
                         fill = leading_candidate_name)) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Vote Difference" ,y = "State", fill = "Candidate", 
       title = "Vote Difference at most recent time stamp")

# How do total votes change over time (by candidate)


