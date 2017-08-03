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

#NOTE: The median length of not-spam emails is greater than that of spam emails.

#Relationship between exclaimation mark and whether or not a message is spam.

# Compute center and spread for exclaim_mess by spam
email %>%
  group_by(spam) %>%
  summarize(median(exclaim_mess),
            IQR(exclaim_mess))

# Create plot for spam and exclaim_mess
email %>%
  mutate(log_exclaim_mess = log(exclaim_mess + .01)) %>%
  ggplot(aes(x = log_exclaim_mess)) +
  geom_histogram() +
  facet_wrap(~ spam)

# Alternative plot: side-by-side box plots
email %>%
  mutate(log_exclaim_mess = log(exclaim_mess + .01)) %>%
  ggplot(aes(x = 1, y = log_exclaim_mess)) +
  geom_boxplot() +
  facet_wrap(~ spam)

# Alternative plot: Overlaid density plots
email %>%
  mutate(log_exclaim_mess = log(exclaim_mess + .01)) %>%
  ggplot(aes(x = log_exclaim_mess, fill = spam)) +
  geom_density(alpha = 0.3)

#Image content and it's association with spam

# Create plot of proportion of spam by image
email %>%
  mutate(has_image = image > 0) %>%
  ggplot(aes(x = has_image, fill = spam)) +
  geom_bar(position = "fill")

#NOTE: An email without an image is more likely to be non-spam than spam.

#do attached images count as attached files in this dataset?
# Test if images count as attachments
sum(email$image > email$attach)

#Answering a question using piping example.
#"Within non-spam emails, is the typical length of emails shorter for those that were sent to multiple people?"
email %>%
  filter(spam == "not-spam") %>%
  group_by(to_multiple) %>%
  summarize(median(num_char))
#Creating chain for both the questions below.
#1.For emails containing the word "dollar", does the typical spam email contain a greater number of occurrences of the word than the typical non-spam email? Create a summary statistic that answers this question.
#2.If you encounter an email with greater than 10 occurrences of the word "dollar", is it more likely to be spam or not-spam? Create a barchart that answers this question.
# Question 1
email %>%
  filter(dollar > 0) %>%
  group_by(spam) %>%
  summarize(median(dollar))

# Question 2
email %>%
  filter(dollar > 10) %>%
  ggplot(aes(x = spam)) +
  geom_bar()

#For illustrating relationships between categorical variables, you've seen

#1. Faceted barcharts
#2. Side-by-side barcharts
#3. Stacked and normalized barcharts.

#Checking the association with the number variable of whether being spam or not.

# Reorder levels
email$number <- factor(email$number, levels = c("none", "small", "big"))

# Construct plot of number
ggplot(email, aes(x = number)) +
  geom_bar() +
  facet_wrap(~ spam)
#NOTE: Given that an email contains no number, it is more likely to be spam.