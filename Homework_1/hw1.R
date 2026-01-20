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
word_counts_case <- texts %>%
  unnest_tokens(word, text, to_lower = FALSE) %>%
  count(doc_title, name = "n_tokens")
word_counts_case

#num of word tokens in Text A before stopword removal, no lowercasing
a_token_ct <- word_counts_case %>%
  filter(doc_title == "Text A") %>%
  pull(n_tokens)
a_token_ct 

#num of word tokens in Text B before stopword removal, no lowercasing
b_token_ct <- word_counts_case %>%
  filter(doc_title == "Text B") %>%
  pull(n_tokens)
b_token_ct 

#split text into tokens and count the number of word tokens, with lowercasing
word_counts_lower <- texts %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_to_lower(word)) %>%
  group_by(doc_title) %>%
  summarise(n_unique_tokens = n_distinct(word)) %>%
  ungroup()
word_counts_lower

#num of UNIQUE word tokens in Text A before stopword removal, with lowercasing
a_token_ct_lower <- word_counts_lower %>%
  filter(doc_title == "Text A") %>%
  pull(n_unique_tokens)
a_token_ct_lower 

#num of UNIQUE word tokens in Text B before stopword removal, with lowercasing
b_token_ct_lower <- word_counts_lower %>%
  filter(doc_title == "Text B") %>%
  pull(n_unique_tokens)
b_token_ct_lower 

#create a diagnostics table before stopword removal
corpus_diagnostics <- tibble(
  doc_title = c("Text A", "Text B"),
  n_chars = c(nchar(text_a),nchar(text_b)), #number of chars in each doc
  n_word_tokens = c(a_token_ct, b_token_ct), #number of word tokens before stopword removal
  n_word_types = c(a_token_ct_lower, b_token_ct_lower) #number of unique word types after lowercasing, before stopword removal
)
corpus_diagnostics

#stopword removal
data("stop_words") #tidytext's built-in stopword list
#project-specific stopwords
custom_stopwords <- tibble(
  word = c(
    "vnto", "haue", "doo", "hath", "bee", "ye", "thee"
  )
)
all_stopwords <- bind_rows(stop_words, custom_stopwords) %>%
  distinct(word)
all_stopwords %>% slice(1:10)

#count which words appear most frequently in each document, 
#after removing standard and custom stopwords
word_cts_no_stopwords <- texts %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_to_lower(word)) %>%
  anti_join(all_stopwords, by = "word") %>%
  count(doc_title, word, sort = TRUE)
word_cts_no_stopwords

#calculate total number of words (after stopword removal) in each doc
doc_lengths <-word_cts_no_stopwords %>%
  group_by(doc_title) %>%
  summarise(total_words = sum(n))
doc_lengths

#join word_cts_no_stopwords with doc_lengths
word_counts_normalized <-
  word_cts_no_stopwords %>%
  left_join(doc_lengths)
word_counts_normalized

#add a new column to word_counts_normalized to
#show each word's frequency as a proportion of the total words in its doc
word_counts_normalized <- word_counts_normalized %>%
  mutate(relative_freq = n/total_words)
word_counts_normalized

#word frequency visualization using relative frequencies
#plot the top 20 words
plot_n_words <- 20
word_comp_tbl <- word_counts_normalized %>%
  pivot_wider(
    names_from = doc_title,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(max_n = pmax(`Text A`, `Text B`)) %>%
  arrange(desc(max_n))

word_plot_data <- word_comp_tbl %>%
  slice_head(n = plot_n_words) %>%
  pivot_longer(
    cols = c(`Text A`, `Text B`),
    names_to = "doc_title",
    values_to = "n"
  ) %>%
  mutate(word = fct_reorder(word, n, .fun = max))

ggplot(word_plot_data, aes(x = relative_freq, y = word)) + 
  geom_col() +
  facet_wrap(~ doc_title, scales = "free_x") +
  labs(
    title = "Most relatively frequent words (stopwords removed)",
    subtitle = paste0(
      "Top ", plot_n_words,
      " words by relative frequency across both texts"
    ),
    x = "Relative frequency of word",
    y = NULL
  ) +
  theme_minimal()