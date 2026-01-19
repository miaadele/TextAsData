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

# Read the raw text files into R
text_a <- read_file(file_a)
text_b <- read_file(file_b)

#combine into a tibble for tidytext workflows
texts <- tibble(
  doc_title = c("Text A", "Text B"),
  text = c(text_a, text_b)
)
texts

#split text into tokens and count the number of word tokens, no lowercasing
word_counts <- texts %>%
  unnest_tokens(word, text, to_lower = FALSE) %>%
  count(doc_title, name = "n_tokens")
word_counts

a_token_ct <- word_counts %>%
  filter(doc_title == "Text A") %>%
  pull(n_tokens)
a_token_ct #num of word tokens in Text A before stopword removal, no lowercasing

b_token_ct <- word_counts %>%
  filter(doc_title == "Text B") %>%
  pull(n_tokens)
b_token_ct #num of word tokens in Text B before stopword removal, no lowercasing

#split text into tokens and count the number of word tokens, with lowercasing
word_counts_lower <- texts %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_to_lower(word)) %>%
  group_by(doc_title) %>%
  summarise(n_unique_tokens = n_distinct(word)) %>%
  ungroup()
word_counts_lower

a_token_ct_lower <- word_counts_lower %>%
  filter(doc_title == "Text A") %>%
  pull(n_unique_tokens)
a_token_ct_lower #num of UNIQUE word tokens in Text A before stopword removal, with lowercasing

b_token_ct_lower <- word_counts_lower %>%
  filter(doc_title == "Text B") %>%
  pull(n_unique_tokens)
b_token_ct_lower #num of UNIQUE word tokens in Text B before stopword removal, with lowercasing

#STEP 1
#create a diagnostics table before stopword removal
corpus_diagnostics <- tibble(
  doc_title = c("Text A", "Text B"),
  n_chars = c(nchar(text_a),nchar(text_b)), #number of chars in each doc
  n_word_tokens = c(a_token_ct, b_token_ct), #number of word tokens before stopword removal
  n_word_types = c(a_token_ct_lower, b_token_ct_lower) #number of unique word types after lowercasing, before stopword removal
)
corpus_diagnostics