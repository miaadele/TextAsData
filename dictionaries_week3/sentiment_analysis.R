#create a targeted sentiment analysis where we measure sentiment only in passages near “trade” (and related terms)
#this will give us more granular insight over the documents’ tone around the concept of trade

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(ggplot2)

circle_raw <- read_file("texts/A07594__Circle_of_Commerce.txt")
free_raw   <- read_file("texts/B14801__Free_Trade.txt")

texts_miss <- tibble(
  doc_title = c("Circle of Commerce", "Free Trade"),
  text = c(circle_raw, free_raw)
)

#basic normalization
texts_miss <- texts_miss %>%
  mutate(
    text_norm = text %>%
      str_replace_all("ſ", "s") %>%   # long s
      str_replace_all("\\s+", " ") %>% # collapse whitespace
      str_to_lower()
  )

#keep track of the position of each word in the document
tokens <- texts_miss %>%
  unnest_tokens(word, text_norm, token = "words") %>%
  group_by(doc_title) %>%
  mutate(token_id = row_number()) %>%
  ungroup()
tokens
  
#define a trade keyword set
trade_terms <- c("trade", "commerce", "merchant", "merchants")

#identify target words' locations
trade_hits <- tokens %>%
  filter(word %in% trade_terms) %>%
  select(doc_title, hit_word = word, hit_token_id = token_id)

#extract token windows around each keyword occurrence
window_size <- 30
trade_windows <- tokens %>%
  inner_join(trade_hits, by = "doc_title") %>%
  filter(token_id >= hit_token_id - window_size, 
         token_id <= hit_token_id + window_size) %>%
  mutate(window_id = paste(doc_title, hit_token_id, sep = "_"))

#sanity check
trade_windows %>%
  filter(window_id == nth(unique(window_id), 10)) %>%
  summarise(window_text = str_c(word, collapse = " ")) %>%
  pull(window_text) %>%
  cat()

#compute sentiment using bing lexicon inside windows only
bing <- get_sentiments("bing")
window_sentiment <- trade_windows %>%
  inner_join(bing, by = "word") %>% #keep only sentiment-bearing words and label each token as poisitve or negative
  count(doc_title, window_id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(net_sentiment = positive - negative)

#summarize sentiments for each text
text_sentiment_summary <- window_sentiment %>%
  group_by(doc_title) %>%
  summarise(
    windows = n(), #rows of window_sentiment
    total_positive = sum(positive),
    total_negative = sum(negative),
    total_net_sentiment = sum(net_sentiment),
    avg_net_per_window = mean(net_sentiment),
    .groups = "drop"
  )
text_sentiment_summary

#compare both texts
ggplot(window_sentiment, aes(x = net_sentiment)) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(~ doc_title, ncol = 1) + 
  labs(
    title = "Sentiment in Trade-Centered Windows (±30 words)",
    x = "Net sentiment per window",
    y = "Number of trade windows"
  )