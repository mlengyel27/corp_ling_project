library(here)
library(tidyverse)
library(mclm)
library(dplyr)
library(tidytext)
library(wordcloud)
library(scales)
library(ggplot2)

# importing tidy objects
tidy_trump_corpus <- readRDS(here("corpora", "tidy_Trump_2016.rds"))
tidy_mccain_corpus <- readRDS(here("corpora", "tidy_McCain_2008.rds"))
tidy_romney_corpus <- readRDS(here("corpora", "tidy_Romney_2012.rds"))
tidy_rpp2008_corpus <- readRDS(here("corpora", "tidy_RPP_2008.rds"))
tidy_rpp2012_corpus <- readRDS(here("corpora", "tidy_RPP_2012.rds"))
tidy_rpp2016_corpus <- readRDS(here("corpora", "tidy_RPP_2016.rds"))

# Count word frequencies for each tidy corpus
count_word_frequencies <- function(tidy_corpus, exclude_word, top_n_words = 100) {
  tidy_corpus %>%
    unnest_tokens(word, text) %>%
    filter(word != exclude_word) %>%
    count(word, sort = TRUE) %>%
    top_n(top_n_words, wt = n)
}

# Count word frequencies for each tidy corpus without proportion column
trump_word_freq <- count_word_frequencies(tidy_trump_corpus, "trump", top_n_words = 200000)
mccain_word_freq <- count_word_frequencies(tidy_mccain_corpus, "mccain", top_n_words = 200000)
romney_word_freq <- count_word_frequencies(tidy_romney_corpus, "romney", top_n_words = 200000)
rpp2008_word_freq <- count_word_frequencies(tidy_rpp2008_corpus, "", top_n_words = 2000)
rpp2012_word_freq <- count_word_frequencies(tidy_rpp2012_corpus, "", top_n_words = 2000)
rpp2016_word_freq <- count_word_frequencies(tidy_rpp2016_corpus, "", top_n_words = 2000)


# Write CSV files with proportional frequencies
write_csv(trump_word_freq, here("corpora", "trump_word_freq.csv"))
write_csv(mccain_word_freq, here("corpora", "mccain_word_freq.csv"))
write_csv(romney_word_freq, here("corpora", "romney_word_freq.csv"))
write_csv(rpp2008_word_freq, here("corpora", "rpp2008_word_freq.csv"))
write_csv(rpp2012_word_freq, here("corpora", "rpp2012_word_freq.csv"))
write_csv(rpp2016_word_freq, here("corpora", "rpp2016_word_freq.csv"))


#2016
trump <- trump_word_freq %>% 
  mutate(author = "trump")

rpp2016 <- rpp2016_word_freq %>%
  mutate(author = "rpp2016")

freqs_2016_election <- bind_rows(rpp2016, trump) 

comparison_df <- freqs_2016_election %>%
  add_count(author, wt = n, name = "total_word") %>% 
  mutate(proportion = n / total_word) %>% 
  select(-total_word, -n) %>% 
  pivot_wider(names_from = author, values_from = proportion, 
              values_fill = list(proportion = 0)) %>%
  mutate(other = "trump") %>% 
  rename(rpp2016 = rpp2016, proportion = trump)


comparison_df %>%
  filter(proportion > 1 / 1e4) %>%
  ggplot(aes(x = proportion, y = `rpp2016`)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(aes(color = abs(`rpp2016` - proportion)), 
              alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) + 
  scale_x_log10(labels = label_percent()) +  
  scale_y_log10(labels = label_percent()) +  #
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") + 
  facet_wrap(~ other) +
  guides(color = FALSE)

# 2008
mccain <- mccain_word_freq %>% 
  mutate(author = "mccain")

rpp2008 <- rpp2008_word_freq %>%
  mutate(author = "rpp2008")

freqs_2008_election <- bind_rows(rpp2008, mccain) 

comparison_df_2008 <- freqs_2008_election %>%
  add_count(author, wt = n, name = "total_word") %>% 
  mutate(proportion = n / total_word) %>% 
  select(-total_word, -n) %>% 
  pivot_wider(names_from = author, values_from = proportion, 
              values_fill = list(proportion = 0)) %>%
  mutate(other = "mccain") %>% 
  rename(rpp2008 = rpp2008, proportion = mccain)

# 2012
romney <- romney_word_freq %>% 
  mutate(author = "romney")

rpp2012 <- rpp2012_word_freq %>%
  mutate(author = "rpp2012")

freqs_2012_election <- bind_rows(rpp2012, romney) 

comparison_df_2012 <- freqs_2012_election %>%
  add_count(author, wt = n, name = "total_word") %>% 
  mutate(proportion = n / total_word) %>% 
  select(-total_word, -n) %>% 
  pivot_wider(names_from = author, values_from = proportion, 
              values_fill = list(proportion = 0)) %>%
  mutate(other = "romney") %>% 
  rename(rpp2012 = rpp2012, proportion = romney)

# Plotting for 2008 election
comparison_df_2008 %>%
  filter(proportion > 1 / 1e4) %>%
  ggplot(aes(x = proportion, y = rpp2008)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(aes(color = abs(rpp2008 - proportion)), 
              alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) + 
  scale_x_log10(labels = label_percent()) +  
  scale_y_log10(labels = label_percent()) + 
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") + 
  facet_wrap(~ other) +
  guides(color = FALSE)

# Plotting for 2012 election
comparison_df_2012 %>%
  filter(proportion > 1 / 1e4) %>%
  ggplot(aes(x = proportion, y = rpp2012)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(aes(color = abs(rpp2012 - proportion)), 
              alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) + 
  scale_x_log10(labels = label_percent()) +  
  scale_y_log10(labels = label_percent()) + 
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") + 
  facet_wrap(~ other) +
  guides(color = FALSE)





