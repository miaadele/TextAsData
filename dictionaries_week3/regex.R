library(readr)
library(dplyr)
library(stringr)
library(tibble)
library(tidyr)
library(tidytext)
library(ggplot2)
library(forcats)

circle_raw <- read_file("texts/A07594__Circle_of_Commerce.txt")

text_tbl <- tibble(
  doc_title = "The Circle of Commerce",
  text = circle_raw
)
#nchar(text_tbl$text) #number of chars

#preserve original text in a column named text_original
text_tbl <- text_tbl %>%
  mutate(text_original = text)

text_tbl <- text_tbl %>%
  mutate(
    text_clean = str_replace_all(text_original, "ſ", "s") #normalize the early modern ſ to the modern s
  )

#sanity check
#tibble(
#  long_s_before = str_count(text_tbl$text_original, "ſ"),
#  long_s_after = str_count(text_tbl$text_clean, "ſ")
#)

text_tbl <- text_tbl %>%
  mutate(
    #remove all punctuation except for currency markers, apostrophes, and hyphens
    text_clean = str_replace_all(text_clean, regex("[[:punct:]&&[^£'-]]"), " ") 
  )

#define a standardization map 
name_map <- c(
  "Smythe" = "Smith", 
  "Smyth" = "Smith", 
  "Smithe" = "Smith",
  "traffick" = "traffic"
)

text_standard <- text_tbl %>%
 mutate(
   text_norm = text_clean %>%
     str_replace_all(regex("\\bSmythe\\b", ignore_case = TRUE), "Smith")%>%
     str_replace_all(regex("\\bSmyth\\b", ignore_case = TRUE), "Smith")%>%
     str_replace_all(regex("\\bSmithe\\b", ignore_case = TRUE), "Smith") %>%
     str_replace_all(regex("\\btraffick\\b", ignore_case = TRUE), "traffic")
   )

#tokenize into bigrams
bigrams_raw <- text_standard %>%
  select(doc_title, text_norm) %>%
  unnest_tokens(output = "bigram", input = text_norm, token = "ngrams", n = 2)
bigrams_raw %>% count(bigram, sort = TRUE) %>% slice_head(n = 10)

#clean text_standard
data("stop_words")
bigrams_clean <- bigrams_raw %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(str_detect(word1, "^[a-z]+$")) %>%
  filter(str_detect(word2, "^[a-z]+$"))
bigrams_clean %>%
  count(word1, word2, sort = TRUE) %>%
  slice_head(n = 10)

#put bigrams back together
bigram_counts <- bigrams_clean %>%
  count(word1, word2, sort = TRUE) %>%
  unite("bigram", word1, word2, sep = " ")

#visualize bigram_counts
bigram_counts %>%
  slice_head(n = 20) %>%
  mutate(bigram = fct_reorder(bigram, n)) %>%
  ggplot(aes(x = n, y = bigram)) + geom_col() + labs(
    title = "Most frequent bigrams after stopword filtering",
    x = "Count",
    y = NULL
  )

#filter bigrams with the token trade
trade_bigrams <- bigram_counts %>%
  filter(str_detect(bigram, "\\btrade\\b"))
trade_bigrams %>% slice_head(n = 25)

trade_bigrams %>%
  slice_head(n = 20) %>%
  mutate(bigram = fct_reorder(bigram, n)) %>%
  ggplot(aes(x = n, y = bigram)) + geom_col() + labs(
    title = "Bigrams that include the word 'trade'",
    x = "Count",
    y = NULL
  )

#create a trade lexicon
trade_lexicon <- c(
  "trade", "traffick", "traffic", "commerce", "merchant", "merchants",
  "exchange", "export", "import", "commodity", "commodities",
  "navigation", "shipping", "market", "markets"
)

trade_theme_bigrams <- bigrams_clean %>%
  filter(word1 %in% trade_lexicon | word2 %in% trade_lexicon) %>%
  count(word1, word2, sort = TRUE) %>%
  unite("bigram", word1, word2, sep = " ")
trade_theme_bigrams %>% slice_head(n =25)

trade_theme_bigrams %>%
  slice_head(n = 20) %>%
  mutate(bigram = fct_reorder(bigram, n)) %>%
  ggplot(aes(x = n, y = bigram)) + geom_col() + labs(
    title = "Trade-theme bigrams (lexicon-based)",
    x = "Count",
    y = NULL
  )