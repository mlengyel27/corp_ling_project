library(tm)
library(SnowballC)
library(here)
library(tidyverse)
library(mclm)
library(dplyr)
library(tidytext)

# Create corpus building and cleaning function
corpuscreator <- function(folder, filename) {
  file_path <- here(folder, filename)
  text <- readLines(file_path, warn = FALSE)
  source <- VectorSource(text)
  corpus <- VCorpus(source)
  
  # Cleaning
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("[^A-Za-z0-9]", " ", x))) 
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("\\b[A-Za-z]\\b", " ", x))) 
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stripWhitespace)
  
  return(corpus)
}

# Trump
trump_corpus <- corpuscreator("data", "Trump_unified.txt")
saveRDS(trump_corpus, here("corpora", "Trump_2016.rds"))


# McCain
mccain_corpus <- corpuscreator("data", "McCain_unified.txt") 
saveRDS(mccain_corpus, here("corpora", "McCain_2008.rds"))

# Romney
romney_corpus <- corpuscreator("data", "Romney_unified.txt") 
saveRDS(romney_corpus, here("corpora", "Romney_2012.rds"))

# RPP2008
rpp2008_corpus <- corpuscreator("data", "2008_Republican_Party_Platform.txt") 
saveRDS(rpp2008_corpus, here("corpora", "RPP_2008.rds"))

# RPP2012
rpp2012_corpus <- corpuscreator("data", "2012_Republican_Party_Platform.txt") 
saveRDS(rpp2012_corpus, here("corpora", "RPP_2012.rds"))

# RPP2016
rpp2016_corpus <- corpuscreator("data", "2016_Republican_Party_Platform.txt") 
saveRDS(rpp2016_corpus, here("corpora", "RPP_2016.rds"))

token_counter <- function(corpus) {
  sum(sapply(corpus, function(doc) length(unlist(strsplit(as.character(doc), "\\W+")))))
}


# counting the tokens in each
cat("Trump Corpus:", token_counter(trump_corpus),"\n")
cat("McCain Corpus:", token_counter(mccain_corpus),"\n")
cat("Romney Corpus:", token_counter(romney_corpus),"\n")
cat("RPP2008 Corpus:", token_counter(rpp2008_corpus),"\n")
cat("RPP2012 Corpus:", token_counter(rpp2012_corpus),"\n")
cat("RPP2016 Corpus:", token_counter(rpp2016_corpus),"\n")


# Tidying each corpus
tidy_trump_corpus <- tidy(trump_corpus)
tidy_mccain_corpus <- tidy(mccain_corpus)
tidy_romney_corpus <- tidy(romney_corpus)
tidy_rpp2008_corpus <- tidy(rpp2008_corpus)
tidy_rpp2012_corpus <- tidy(rpp2012_corpus)
tidy_rpp2016_corpus <- tidy(rpp2016_corpus)

# Saving tidy corpus objects
saveRDS(tidy_trump_corpus, here("corpora", "tidy_Trump_2016.rds"))
saveRDS(tidy_mccain_corpus, here("corpora", "tidy_McCain_2008.rds"))
saveRDS(tidy_romney_corpus, here("corpora", "tidy_Romney_2012.rds"))
saveRDS(tidy_rpp2008_corpus, here("corpora", "tidy_RPP_2008.rds"))
saveRDS(tidy_rpp2012_corpus, here("corpora", "tidy_RPP_2012.rds"))
saveRDS(tidy_rpp2016_corpus, here("corpora", "tidy_RPP_2016.rds"))

