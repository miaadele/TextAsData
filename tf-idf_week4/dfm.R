library(tibble)
library(dplyr)
library(readr)
library(ggplot2)
library(quanteda)
library(quanteda.textstats)

txt_circle    <- read_file("texts/A07594__Circle_of_Commerce.txt")
txt_free      <- read_file("texts/B14801__Free_Trade.txt")
txt_third     <- read_file("texts/A06785.txt")

texts <- c(
  "Circle_of_Commerce" = txt_circle,
  "Free_Trade"         = txt_free,
  "Third_Text_A06785"  = txt_third
)

corp <- corpus(texts)

#tokenization and basic cleaning
toks <- tokens(
  corp,
  remove_punct   = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE
)

toks <- tokens_tolower(toks)

custom_stop <- c(
  "vnto","haue","doo","hath","bee","ye","thee","hee","shall","hast","doe",
  "beene","thereof","thus" 
)

toks <- tokens_remove(toks, pattern = c(stopwords("en"), custom_stop))

#build document-feature matrix
dfm_mat <- dfm(toks)
dfm_mat
#topfeatures(dfm_mat, 25)

#treat each doc as a word frequency vector, so each row of the DFM is a doc
#measure the pairwise Pearson correlation across the columns
sim_cor <- textstat_simil(
  dfm_mat, 
  method = "correlation",
  margin = "documents"
)
sim_cor

dfm_tfidf <- dfm_tfidf(dfm_mat)
dfm_tfidf
topfeatures(dfm_tfidf, 20)
tfidf_mat <- as.matrix(dfm_tfidf)

#extract words with the top TF-IDF values for each text
circle_tfidf <- tfidf_mat["Circle_of_Commerce", ]
top_circle <- sort(circle_tfidf, decreasing = TRUE)[1:20]
top_circle

free_tfidf <- tfidf_mat["Free_Trade", ]
top_free<- sort(free_tfidf, decreasing = TRUE)[1:20]
top_free

mystery_tfidf <- tfidf_mat["Third_Text_A06785", ]
top_mystery <- sort(mystery_tfidf, decreasing = TRUE)[1:20]
top_mystery

#visualize the most characteristic terms for each doc
tfidf_top_tbl <- bind_rows(
  tibble(document = "Circle of Commerce", 
         term = names(top_circle), 
         tfidf = unname(top_circle)),
  tibble(document = "Free Trade",         
         term = names(top_free),   
         tfidf = unname(top_free)),
  tibble(document = "Third Text",         
         term = names(top_mystery),  
         tfidf = unname(top_mystery))
)

ggplot(tfidf_top_tbl, aes(x = tfidf, y = reorder(term, tfidf))) +
  geom_col() +
  facet_wrap(~ document, scales = "free_y") +
  labs(
    title = "Most Characteristic Terms by Document (TF–IDF)",
    x = "TF–IDF score",
    y = NULL
  ) +
  theme_minimal()