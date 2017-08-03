#Trying to check out the association between spam and the length of an email

# Load packages
library(openintro)
library(ggplot2)
library(dplyr)

# Compute summary statistics
email %>%
  group_by(spam) %>%
  summarize(median(num_char),
            IQR(num_char))

# Create plot
email %>%
  mutate(log_num_char = log(num_char)) %>%
  ggplot(aes(x = spam, y = log_num_char)) +
  geom_boxplot()