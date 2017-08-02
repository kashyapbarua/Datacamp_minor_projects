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

#Proportion of males admitted overall
ucb_admit %>%
  # Table of counts of admission status and gender
  count(Gender, Admit) %>%
  # Spread output across columns based on admission status
  spread(Admit, n) %>%
  # Create new variable
  mutate(Perc_Admit = Admitted / (Admitted + Rejected))

#Proportion of males admitted for each department

# Table of counts of admission status and gender for each department
admit_by_dept <- ucb_admit %>%
  count(Dept, Gender, Admit) %>%
  spread(Admit, n)

# View result
admit_by_dept

# Percentage of those admitted to each department
admit_by_dept %>%
  mutate(Perc_Admit = Admitted / (Admitted + Rejected))
