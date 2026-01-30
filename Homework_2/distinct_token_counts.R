#This code filters distinct words in each doc
#This is not part of the homework assignment, but I calculated
#the number of positive vs negative distinct words are summed for each text
#in order to answer the analysis

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(forcats)
library(tibble)

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
    "vnto","haue","doo","hath","bee","ye","thee","hee","shall","hast","doe",
    "beene","thereof","thus"
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
#word_counts is a tidy table with the columns: doc_title, word, and n
#words in word_counts are lowercased 
#stopwords have been removed
word_counts

#calculate total number of tokens in each doc (after lowercasing and stopword removal)
doc_lenghts <- word_counts %>%
  group_by(doc_title) %>%
  summarise(total_words = sum(n))
doc_lenghts

#STEP 2: Join tokens to the Bing sentiment dictionary
bing <- word_counts %>%
  inner_join(get_sentiments("bing"))
bing

#filter OUT words that appear across both texts
unique_words <- bing %>%
  group_by(word) %>%
  filter(n_distinct(doc_title) == 1) %>%
  ungroup()
unique_words

#Of the distinct words in each text, calculate the number of words with positive sentiment vs negative sentiment
unique_cts <- unique_words %>%
  count(doc_title, sentiment, name = "n") %>%
  pivot_wider(names_from = sentiment, values_from = n)
unique_cts
