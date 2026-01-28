#Homework Assignment: Sentiment Analysis and TF-IDF
#This code computes sentiment twice for the same texts. 
#Sentiment is computed using 1) raw word counts and 2) TF-IDF weighted words

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(forcats)
library(tibble)
library(scales)

file_a <- "texts/A07594__Circle_of_Commerce.txt"
file_b <- "texts/B14801__Free_Trade.txt"

text_a <- read_file(file_a)
text_b <- read_file(file_b)

#combine into a tibble for tidytext workflows
texts <- tibble(
  doc_title = c("Text A", "Text B"),
  text = c(text_a, text_b)
)
texts

#start with tidytext's build-in stopword list
data("stop_words")

#add project-specific stopwords
custom_stopwords <- tibble(
  word = c(
    "vnto", "haue", "doo", "hath", "bee", "ye", "thee"
  )
)

all_stopwords <- bind_rows(stop_words, custom_stopwords) %>%
  distinct(word)

#STEP 1: tokenize and clean the text
word_counts <- texts %>%
  unnest_tokens(word, text) %>% #automatically remove punctuation and lowercases
  mutate(word = str_to_lower(word)) %>%
  anti_join(all_stopwords, by = "word") %>%
  count(doc_title, word, sort = TRUE)

#STEP 2: Join tokens to the Bing sentiment dictionary
bing <- word_counts %>%
  inner_join(get_sentiments("bing"))

#STEP 3: Compute raw sentiment totals
raw_sentiment <- bing %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  group_by(doc_title) %>%
  summarise(
    total_pos = sum(positive),
    total_neg = sum(negative),
    net_sentiment = total_pos - total_neg
  )
raw_sentiment

#STEP 4: Compute TF-IDF for words in each doc
tf_idf <- word_counts %>%
  #count how many times each word appears in each doc
  pivot_wider(names_from = doc_title, values_from = n, values_fill = 0)
tf_idf