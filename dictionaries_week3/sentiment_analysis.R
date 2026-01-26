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
trade_terms <- ("trade", "commerce", "merchant", "merchants")

#identify target words' locations
trade_hits <- tokens %>%
  filter(word %in% trade_terms) %>%
  select(doc_title, hit_word = word, hit_token_id = token_id)

#extract token windows around each keyword occurrence

#compute sentiment inside windows only

#compare both texts