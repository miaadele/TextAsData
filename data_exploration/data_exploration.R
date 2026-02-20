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

#--------------------------------------------------
#Approach 1: TF-IDF, lexical distinctiveness

#Step 1: Construct a DFM for the full corpus
df_mat <- dfm(toks)

#calculate total number of tokens in each doc
doc_lengths <- tibble(
  doc_title = docnames(df_mat),
  n_tokens = rowSums(df_mat)
)
doc_lengths

#Step 2: Compute TF-IDF weights
dfm_tfidf <- dfm_tfidf(df_mat)
dfm_tfidf

tfidf_mat <- as.matrix(dfm_tfidf)
tfidf_mat

#Step 3:For each document, extract the top 15 TF-IDF terms
top15_idf <- lapply(rownames(tfidf_mat), function(doc) {
  doc_tfidf <- tfidf_mat[doc, ]
  top_terms <- sort(doc_tfidf, decreasing = TRUE) [1:15]
  return(top_terms)
})
names(top15_idf) <- rownames(tfidf_mat)
top15_idf

#combine into a single table
top15_combined <- do.call(rbind, lapply(names(top15_idf), function(doc) {
  data.frame(
    doc_title = doc,
    word = names(top15_idf[[doc]]),
    tfidf = as.numeric(top15_idf[[doc]])
  )
}))
top15_combined

#visualize the top 15 characteristic terms for each document
for (doc in unique(top15_combined$doc_title)) {
  doc_data <- top15_combined %>%
    filter(doc_title == doc)
  
  p <- ggplot(doc_data, aes(x = tfidf, y = reorder(word, tfidf))) +
    geom_col() +
    labs(
      title = paste("Top 15 TF-IDF Terms:", doc),
      x = "TF-IDF score",
      y = NULL
    ) +
    theme_minimal()
  
  ggsave(
    filename = paste0("tfidf_", doc, ".png"),
    plot = p,
    width = 8,
    height = 6
  )
}

#from the top 15 words with the highest TF-IDF scores,
#filter for distinctive terms by document
distinct_terms <- top15_combined %>%
  group_by(word) %>%
  filter(n_distinct(doc_title) == 1) %>%
  ungroup()
distinct_terms

write.csv(distinct_terms, file = 'tfidf_distinct_terms_per_doc.csv')

tables_list <- distinct_terms %>%
  group_by(doc_title) %>%
  group_split()

names(tables_list) <- distinct_terms %>% 
  distinct(doc_title) %>% 
  pull(doc_title)
tables_list

for(name in names(tables_list)) {
  file_name <- paste0(name, ".csv")  # e.g., "Text A.csv"
  write.csv(tables_list[[name]], file = file_name, row.names = FALSE)
}

#--------------------------------------------------
#Approach 2: Pearson correlation, similarity and distance between texts

#Step 1: Compute pairwise correlations between documents
  #first, trim rare words (frequency of less than 5)
df_trimmed = dfm_trim(df_mat, min_termfreq = 5)
df_trimmed

#compute correlation similarity
sim_cor <- textstat_simil(
  df_trimmed,
  method = "correlation",
  margin = "documents"
)
sim_cor 
  
write.csv(sim_cor, file = 'pearson_correlations.csv')

#convert sim_cor to a matrix
sim_mat <- as.matrix(sim_cor)
dim(sim_mat)
sim_mat[1:5, 1:5] #print first 5 rows and first 5 columns
sim_mat <- round(sim_mat, 3)

#long format for ggplot
heat_df <- as.data.frame(sim_mat) %>%
  rownames_to_column("doc_i") %>%
  pivot_longer(-doc_i, names_to = "doc_j", values_to = "r")

#sort from highest to lowest correlation
sort_correlation <- heat_df %>%
  filter(doc_i != doc_j) %>% #remove self-correlations
  filter(doc_i < doc_j) %>% #remove duplicate pairs
  arrange(desc(r))
sort_correlation

#Highest Correlation: 1
slice_max(sort_correlation, r, n = 1)
# text13 (A32833.txt) and text9 (A32827.txt)

#Lowest Correlation: 0.049
slice_min(sort_correlation, r, n = 1)
# text 12 (A32830.txt) and text 21 (A93819.txt)

#Step 2: Heatmap visualization
ggplot(heat_df, aes(x = doc_j, y = doc_i, fill = r)) +
  geom_tile() +
  coord_fixed() +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0
  ) +
  labs(
    title = "Pearson Correlation Between Documents",
    x = NULL,
    y = NULL,
    fill = "Correlation"
    ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

#--------------------------------------------------
#Approach 3: Syntactic complexity
#This code calculates syntactic complexity of the least similar two texts
#(the two texts with the lowest pearson correlation score):
  #1. text 12 (A32830.txt) 
  #2. text 21 (A93819.txt)

#read the text files
text12 <- read_file("texts/A32830.txt")
text21 <- read_file("texts/A93819.txt")

#create dataframe with raw text
two_texts <- tibble(
  document = c("text 12", "text 21"),
  text = c(text12, text21)
)
two_texts

library(udpipe)

#Load an English UD (= Univesal Dependencies) model 
model_info <- udpipe_download_model(language = "english-ewt")
ud_model <- udpipe_load_model(model_info$file_model)

#annotate both texts using UDPipe
anno_df <- two_texts %>%
  mutate(
    #parse each text with UD parser and set doc_id to doc name
    anno = map2(
      text,
      document,
      ~ udpipe_annotate(ud_model, x = .x, doc_id = .y) %>%
        as.data.frame()
    )
  ) %>%
  #keep only parse annotations, then unnest into rows
  select(anno) %>%
  unnest(anno) %>%
  #use the UD doc_id as the doc label and drop any duplicates
  rename(document = doc_id) %>%
  #select columns for syntactic analysis
  select(
    document,
    paragraph_id,
    sentence_id,
    token_id,
    token,
    lemma,
    upos,
    feats,
    head_token_id,
    dep_rel
  )
anno_df %>%
  glimpse()

#identify syntactic features
syntax_df <- anno_df %>%
  mutate(
    is_word = upos != "PUNCT", #<--is it a word (and not punctuation?)
    
    
    # Is this an independent clause? finite verbs are proxy for indipendent clauses
    is_clause = (upos %in% c("VERB", "AUX")) &
      str_detect(coalesce(feats, ""), "VerbForm=Fin"),
    
    # Dependent clause? 
    is_dep_clause = dep_rel %in% c(
      "advcl", #adverbial clause 
      "ccomp", # clausal complement
      "xcomp", #open clausal complement
      "acl", #adnomial clause
      "acl:relcl" #relative clause
    ),
    
    # Is this coordination? That is, does it use "and" "or" etc.?
    is_coord = dep_rel %in% c("conj", "cc"),
    
    # Nominal complexity: these relations make noun phrases more complex
    is_complex_nominal = dep_rel %in% c(
      "amod", # adjective modifier ("big cup")
      "nmod", #nominal modifier ("cup of tea")
      "compound", # compound ("lemon tea")
      "appos" #apposition ("tea, my favorite!")
    )
    
  )

#count syntactic features for each individual sentence
sentence_df <- syntax_df %>%
  filter(is_word) %>%           #count words (not punctuation)
  group_by(document, sentence_id) %>%   #group by document and sentence
  summarise(
    words          = n(),   #number of words per sentence
    clauses        = sum(is_clause), # number of clauses per sentence
    dep_clauses    = sum(is_dep_clause), #number of dependent clauses per sentence
    .groups = "drop"
  )

sentence_df

#Mean length of sentence (MLS)
mls_df <- sentence_df %>%
  group_by(document) %>%
  summarise(
    MLS = mean(words), # Average words per sentence
    .groups = "drop"
  )
mls_df 

#Clauses per sentence (C/S)
clausal_density_df <- sentence_df %>%
  group_by(document) %>%
  summarise(
    sentences = n(),
    clauses   = sum(clauses),
    C_per_S   = clauses / sentences,
    .groups = "drop"
  )
clausal_density_df

#Dependent clauses per sentence 
subordination_df <- sentence_df %>%
  group_by(document) %>%
  summarise(
    clauses = sum(clauses),
    dep_clauses = sum(dep_clauses),
    sentences = n(),
    DC_per_C = dep_clauses / pmax(clauses, 1), #avoid division by 0 
    DC_per_S = dep_clauses / sentences,
    .groups = "drop"
  )
subordination_df

#Coordination per sentence
coordination_df <- syntax_df %>%
  group_by(document) %>%
  summarise(
    coord_relations = sum(is_coord),
    clauses         = sum(is_clause),
    sentences       = n_distinct(sentence_id),
    Coord_per_C     = coord_relations / pmax(clauses, 1),
    Coord_per_S     = coord_relations / sentences,
    .groups = "drop"
  )
coordination_df

#Complex nominals per sentence
nominal_df <- syntax_df %>%
  group_by(document) %>%
  summarise(
    complex_nominals = sum(is_complex_nominal),
    clauses          = sum(is_clause),
    sentences        = n_distinct(sentence_id),
    CN_per_C         = complex_nominals / pmax(clauses, 1),
    CN_per_S         = complex_nominals / sentences,
    .groups = "drop"
  )
nominal_df

#Create summary table
all_measures <- mls_df %>%  # ← Added mls_df %>%
  left_join(clausal_density_df %>% select(document, C_per_S), by = "document") %>%
  left_join(subordination_df %>% select(document, DC_per_C, DC_per_S), by = "document") %>%
  left_join(coordination_df %>% select(document, Coord_per_C, Coord_per_S), by = "document") %>%
  left_join(nominal_df %>% select(document, CN_per_C, CN_per_S), by = "document")

all_measures %>%
  knitr::kable(
    digits = 2,
    col.names = c("Document", "MLS", "C/S", "DC/C", "DC/S", 
                  "Coord/C", "Coord/S", "CN/C", "CN/S")
  )
write.csv(all_measures, file = "syntactic_complexity.csv")

# Reshape for plotting
syntax_long <- all_measures %>%  # ← Added %>%
  pivot_longer(
    cols = -document,
    names_to = "Measure",
    values_to = "Value"
  ) %>%
  mutate(
    Category = case_when(
      Measure == "MLS" ~ "Sentence Length",
      Measure == "C_per_S" ~ "Clausal Density",
      Measure %in% c("DC_per_C", "DC_per_S") ~ "Subordination",
      Measure %in% c("Coord_per_C", "Coord_per_S") ~ "Coordination",
      Measure %in% c("CN_per_C", "CN_per_S") ~ "Phrasal Complexity"
    )
  )

# Plot
ggplot(syntax_long, aes(x = Measure, y = Value, fill = document)) +
  geom_col(position = "dodge", width = 0.7) +
  facet_wrap(~Category, scales = "free", ncol = 2) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Syntactic Complexity: Complete Profile",
    subtitle = "Comparing multiple dimensions of syntactic complexity",
    x = NULL,
    y = "Value",
    fill = "Document"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )