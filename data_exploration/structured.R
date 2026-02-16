library(dplyr)
library(tidyr)
library(tidytext)
library(tibble)
library(readr)
library(tidyverse)
library(readr)
library(purrr) 
library(quanteda)
library(quanteda.textstats)
library(stringr)

#retrieve files
all_files <- list.files("texts/", full.names = TRUE)
all_files 

#create cleaned title vector
doc_titles <- all_files |>
  basename() |> #remove folder path
  str_remove("\\.txt$")
#doc_titles

#read each file into a tibble
texts_tbl <- map_dfr(
  seq_along(all_files),
  ~ tibble(
    doc_title = doc_titles[.x],
    text = read_file(all_files[.x])
  )
)
#texts_tbl

#normalization and justifications:
#normalize the early modern ſ to the modern s
#collapse white space because historical texts can have irregular spaces due to archaic typography
#normalize all words to lower case because I want to analyze how often the texts talk about different tokens regardless of capitalization
texts_clean <- texts_tbl %>%
  mutate(
    text = 
        str_replace_all(text, "ſ", "s") %>%
        str_replace_all("\\s+", " ") %>% # collapse whitespace
        str_to_lower() 
  )
#texts_clean

#combine texts into a quanteda corpus
corp <- corpus(texts_clean)
#corp


#create stopword list
data("stop_words")
custom_stopwords <- tibble(
  word = c(
    "vnto", "haue", "doo", "hath", "bee", "ye", "thee",
    "hee", "shall", "hast", "doe", "beene", "thereof", "thus"
  )
)
all_stopwords <- bind_rows(stop_words, custom_stopwords) %>%
  distinct(word)

#tokenize
#I am removing punctuation because I am not analyzing style
#I am removing numbers because pagination or rounded guesses may be distracting
#I am removing symbols because they are distracting, and
  #the only meaningful ones are likely currency markers, which are
  #no longer useful because numbers have been removed
toks <- tokens(
  corp,
  remove_punct = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE
)
#toks

#remove stopwords
#I am removing stopwords in order to decrease noise
toks <- tokens_remove(
  toks, pattern = all_stopwords$word
)
toks

df_mat <- dfm(toks)

#--------------------------------------------------
#Approach 1: TF-IDF, lexical distinctiveness

#Step 1: Construct a DFM for the full corpus
df_mat

#Step 2: Compute TF-IDF weights

#Step 3:For each document, extract the top 15 TF-IDF terms
#--------------------------------------------------
#Approach 2: Pearson correlation, similarity and distance between texts

#Step 1: Compute pairwise correlations between documents
  #first, trim rare words (frequency of less than 5)
df_trimmed = dfm_trim(df_mat, min_termfreq = 5)
df_trimmed

#correlation similarity
sim_cor <- textstat_simil(
  df_trimmed,
  method = "correlation",
  margin = "documents"
)
sim_cor 


#Step 2: Heatmap visualization

#--------------------------------------------------
#Approach 3: Syntactic complexity
#This code calculates syntactic complexity of the following two texts:
  #1.
  #2.

#Mean length of sentence (MLS)

#Clauses per sentence (C/S)

#Dependent clauses per sentence 

#Coordination per sentence

#Complex nominals per sentence

#Create summary table