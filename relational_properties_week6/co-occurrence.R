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
cooc <- windows %>%
  count(word, sort = TRUE, name = "cooc_n")
cooc

#Compute overall word frequencies, P(w) and P(a), in the document
total_tokens = nrow(tokens)

word_freq <- tokens %>%
  count(word, name = "word_n") %>%
  mutate(p_word = word_n/total_tokens)
word_freq

#calculate anchor frequency and probability
anchor_stats <- word_freq %>%
  filter(word == anchor) %>%
  transmute(anchor_n = word_n,
            p_anchor = p_word)
anchor_stats

total_window_tokens <-nrow (windows) 
#count how many times w appears inside anchor windows
p_w_given_windows <- windows %>%
  count(word, name = "cooc_n") %>%
  mutate(p_word_in_windows = cooc_n/total_window_tokens)
p_w_given_windows 

#join baseline frequencies and compute PMI
pmi_tbl <- p_w_given_windows %>%
  left_join(word_freq, by = "word") %>%
  mutate(
    pmi = log2(p_word_in_windows / 
                 (p_word * anchor_stats$p_anchor))
  ) %>%
  arrange(desc(pmi))
pmi_tbl

#filter by minimum co-occurrence count
pmi_tbl_filtered <- pmi_tbl %>%
  filter(cooc_n >= 3) %>%
  arrange(desc(pmi))
pmi_tbl_filtered

#visually compare raw co-occurrence
cooc %>%
  slice_max(cooc_n, n = 15) %>%
  mutate(word = reorder(word, cooc_n)) %>%
  ggplot(aes(x = cooc_n, y = word)) +
  geom_col() +
  labs(
    title = str_glue("Top co-occurring words within +- {window_size} of '{anchor}'"),
    x = "Co-occurrence count",
    y = NULL
  ) 

#top PMI (filtered by co-occurrence threshold)
pmi_tbl_filtered %>%
  slice_max(pmi, n = 15) %>%
  mutate(word = reorder(word, pmi)) %>%
  ggplot(aes(x = pmi, y = word)) +
  geom_col() +
  labs(
    title = str_glue("Top PMI-associated words near '{anchor}' (cooc_n greater than or equal to 3)"),
    x = "PMI (log2 scale)",
    y = NULL
  )