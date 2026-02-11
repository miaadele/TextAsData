# Goal: understand how Adam Smith discusses the term "labor"
# in Wealth of Nations (1776)

library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(tibble)
library(readr)
library(tidyverse)
library(ggplot2)

raw_txt <- read_file("wealth.txt")

texts <- tibble(
  doc_title = "wealth.txt",
  text = raw_txt
)
texts

#tokenize text and preserve token order
tokens <- texts %>%
  unnest_tokens(word, text) %>%
  group_by(doc_title) %>%
  mutate(token_id = row_number()) %>%
  ungroup()
#tokens %>% slice(1:20)

#"labor" is our anchor term
#check how many anchor words are in the text
anchor <- "labor"
tokens %>%
  filter(word == anchor) %>%
  count() #n = 1011

window_size <- 5
anchor_hits <- tokens %>%
  filter(word == anchor) %>%
  select(doc_title, anchor_id = token_id) #organize by location of anchor position
#anchor_hits

# pair each anchor occurrence with every token in the document
# then, calculate the distances between them
windows <- anchor_hits %>%
  #join each anchor occurrence to ALL tokens in the same doc
  left_join(
    tokens,
    by = "doc_title",
    relationship = "many-to-many"
  ) %>%
  
  # compute how far each token is from the anchor
  # negative distance = to the LEFT of the anchor
  # positive distance = to the RIGHT of the anchor
  mutate(distance = token_id - anchor_id) %>%
  
  #keep only tokens within the window size
  filter(abs(distance) <= window_size) %>%
  filter(distance != 0) %>% #remove the anchor word itself (i.e. keep context words only)
  
  mutate(
    window_id = paste0(
      doc_title,
      "_",
      anchor_id),
    anchor_word = anchor
  ) %>%
  
  select(
    doc_title,
    window_id,
    anchor_word,
    anchor_id,
    token_id,
    distance,
    word
  )

#each row in windows represents one word that appeared near one specific occurrence of the anchor
windows %>%
  arrange(anchor_id, distance) %>%
  slice(1:20)

#collapse all the context window rows into a single rankled list
#this measures how many time a give word appears within ±5 window_size tokens of the anchor across the whole document