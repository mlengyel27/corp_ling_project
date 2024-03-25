library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readr)
library(tidytext)

# function to count bigrams of a word in a given corpus
bigram_counter <- function(targetcorpus, targetword) {
  bigrams <- targetcorpus %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) 
  
  separated_bigrams <- bigrams %>%
    separate(bigram, into = c("word1", "word2"), sep = " ")
  
  united_bigrams <- separated_bigrams %>%
    unite(bigram, c(word1, word2), sep = " ", remove = FALSE) 
  
  bigram_counts <- united_bigrams %>% count(bigram, sort = TRUE)
  
  targetword_bigrams <- united_bigrams %>%
    filter(word1 == targetword | word2 == targetword) %>%
    unite(bigram, c(word1, word2), sep = " ", remove = FALSE) %>%  
    count(bigram = str_c(word1, word2, sep = " "), sort = TRUE) %>%
    mutate(bigram = str_replace(bigram, targetword, ""))
  
  return(targetword_bigrams)
}

# running all 6 corpora through the function and visualizing the results on a word cloud
# RPP2016 and Trump for "foreign"
rpp_2016_result <- bigram_counter(tidy_rpp2016_corpus, "foreign")
trump_result <- bigram_counter(tidy_trump_corpus, "foreign")

wordcloud(words = rpp_2016_result$bigram, freq = rpp_2016_result$n, min.freq = 1, 
          random.order = FALSE, max.words = 70,scale = c(3, 0.5),
           colors = brewer.pal(8, "Dark2"))
wordcloud(words = trump_result$bigram, freq = trump_result$n, min.freq = 1, 
          random.order = FALSE, max.words = 70,scale = c(3, 0.5),
          colors = brewer.pal(8, "Dark2"))

#RPP2012 and Romney for "economic"
rpp_2012_result <- bigram_counter(tidy_rpp2012_corpus, "economic")
romney_result <- bigram_counter(tidy_romney_corpus, "economic")

wordcloud(words = rpp_2012_result$bigram, freq = rpp_2012_result$n, min.freq = 1, 
          random.order = FALSE, max.words = 70,scale = c(3, 0.5),
          colors = brewer.pal(8, "Dark2"))
wordcloud(words = romney_result$bigram, freq = romney_result$n, min.freq = 1, 
          random.order = FALSE, max.words = 70,scale = c(3, 0.5),
          colors = brewer.pal(8, "Dark2"))

#RPP2008 and McCain for "tax"
rpp_2008_result <- bigram_counter(tidy_rpp2008_corpus, "tax")
mccain_result <- bigram_counter(tidy_mccain_corpus, "tax")

wordcloud(words = rpp_2008_result$bigram, freq = rpp_2008_result$n, min.freq = 1, 
          random.order = FALSE, max.words = 70,scale = c(3, 0.5),
          colors = brewer.pal(8, "Dark2"))
wordcloud(words = mccain_result$bigram, freq = mccain_result$n, min.freq = 1, 
          random.order = FALSE, max.words = 70,scale = c(3, 0.5),
          colors = brewer.pal(8, "Dark2"))



