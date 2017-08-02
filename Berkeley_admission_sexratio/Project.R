#Number of males and females admitted(Use the ucb_admit dataset)
# Load packages
library(dplyr)
library(tidyr)

# Count number of male and female applicants admitted
ucb_counts <- ucb_admit %>%
  count(Gender, Admit)

# View result
ucb_counts

# Spread the output across columns
ucb_counts %>%
  spread(Admit, n)