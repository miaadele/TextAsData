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

all_stopwords %>% slice(1:10)

word_counts <- texts %>%
  unnest_tokens(word, text) %>% #automatically remove punctuation and lowercases
  mutate(word = str_to_lower(word)) %>%
  anti_join(all_stopwords, by = "word") %>%
  count(doc_title, word, sort = TRUE)
word_counts

plot_n_words <- 20

#select the most frequent words overall
word_comparison_tbl <- word_counts %>%
  pivot_wider(
    names_from = doc_title,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(max_n = pmax(`Text A`, `Text B`)) %>%
  arrange(desc(max_n))

word_plot_data <- word_comparison_tbl %>%
  slice_head(n = plot_n_words) %>%
  pivot_longer(
    cols = c(`Text A`, `Text B`),
    names_to = "doc_title",
    values_to = "n"
  ) %>%
  mutate(word = fct_reorder(word, n, .fun = max))

ggplot(word_plot_data, aes(x = n, y = word)) + 
  geom_col() +
  facet_wrap(~ doc_title, scales = "free_x") +
  labs(
    title = "Most frequent words (stopwords removed)",
    subtitle = paste0(
      "Top", plot_n_words,
      "words by maximum frequency across both texts"
    ),
    x = "Word frequency",
    y = NULL
  ) +
  theme_minimal()

bigrams <- texts %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams

#inspect each word separately in the bigram
bigrams_separated <- bigrams %>%
  separate(bigram, into = c("word1", "word2"), sep = " ")
bigrams_separated

#remove all stopwords
bigrams_filtered <- bigrams_separated %>%
  filter(
    !word1 %in% all_stopwords$word,
    !word2 %in% all_stopwords$word
  )
bigrams_filtered

#remove bigrams where either word is a stopword
bigram_counts <- bigrams_filtered %>%
  count(doc_title, word1, word2, sort = TRUE)
bigram_counts

#put the bigrams back together
bigram_counts <- bigram_counts %>%
  unite(bigram, word1, word2, sep = " ")
bigram_counts

#normalize by the number of bigrams within each document
bigram_relative <- bigram_counts %>%
  group_by(doc_title) %>%
  mutate(
    total_bigrams = sum(n),
    proportion = n / total_bigrams
  ) %>%
  ungroup()

#reshape data for comparison
bigram_wide <- bigram_relative %>%
  select(doc_title, bigram, proportion) %>%
  pivot_wider(
    names_from = doc_title,
    values_from = proportion,
    values_fill = 0
  )
bigram_wide

#determine which bigram is most characteristic of TextA relative to TextB
bigram_diff <- bigram_wide %>%
  mutate(
    diff = `Text A` - `Text B`
  ) %>%
  arrange(desc(abs(diff)))
bigram_diff %>% slice(1:20)

#if diff > 0, then the bigram is more prominant in TextA
#if diff < 0, then the bigram is more prominant in TextB