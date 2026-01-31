#this code measures structure via frequency counts, ratios, or length indices

library(readr)
library(tidyverse)
library(tidyr)
library(tidytext)
library(ggplot2)
library(udpipe)
library(koRpus)
library(koRpus.lang.en)

circle <- read_file("texts/A07594__Circle_of_Commerce.txt")
mystery <- read_file("texts/A69858.txt")

texts_df <- tibble(
  document = c("Circle of Commerce", "A69858"),
  author = c("Misselden", "Unknown"),
  text = c(circle, mystery)
)
texts_df

#load an English universal dependency (UD) model once
model_info <- udpipe_download_model(language = "english-ewt")
ud_model <- udpipe_load_model(model_info$file_model)

#annotate both texts using UDPipe
anno_df <- texts_df %>%
  mutate(
    #parse each text with UD parser, and set doc_id to our doc name
    anno = map2(
      text,
      document,
      ~udpipe_annotate(
        ud_model,
        x = .x,
        doc_id = .y
      ) %>%
        as.data.frame()
    )
  ) %>%
  select(anno) %>% #keep only parsed annotations
  unnest(anno) %>% #unnest into rows
  rename(document = doc_id) %>% #use the UD doc_id as document label, and drop any duplicates cleanlu
  select( #select columns for syntactic analysis
    document,
    paragraph_id,
    sentence_id,
    token_id,
    token,
    lemma,
    upos, #part of speech
    feats, #grammatical features
    head_token_id, #head of dependency relation
    dep_rel #dependency relation type
  )
anno_df %>%
  glimpse()

#create an example parsed sentence
example_sentence <- tibble(
  token = c("The", "big", "dog", "barks"),
  token_id = c(1,2,3,4),
  head_token_id = c(3,3,4,0),
  Relationship = c(
    '"The" depends on word #3 (dog)',
    '"big" depends on word #3 (dog)',
    '"dog" depends on word #4 (barks)',
    '"barks" is the ROOT (doesn\'t depend on anything)'
  )
)

example_sentence %>%
  knitr::kable(
    caption = 'Example: Dependency structure of "The big dog barks"',
    align = c("l","c","c","l")
  )

#create binary flags for different syntactic structures
syntax_df <- anno_df %>%
  mutate(
    #is it a word (i.e. not punctuation)?
    is_word = upos != "PUNCT", 
    
    #is it an independent clause? Finite verbs are proxy for ind clauses
    is_clause = (upos %in% c("VERB", "AUX")) & str_detect(coalesce(feats, ""), "VerbForm=Fin"),
    
    #is it a dependent clause?
    is_dep_clause = dep_rel %in% c(
      "advcl", #adverbial clause
      "ccomp", #clausal complement
      "xcomp", #open clausal complement
      "acl", #adnomial clause
      "acl:relcl" #relative clause
    ),
    
    #is it coordination? 
    is_coord = dep_rel %in% c("conj", "cc"),
    
    #nominal complexity
    is_complex_nominal = dep_rel %in% c(
      "amod", #adjective modifier
      "nmod", #nominal modifier
      "compound", #compoud
      "appos" #apposition
    )
  )
syntax_df %>%
  select(document, token, upos, is_clause, is_dep_clause) %>%
  head(20)
#---------------------------------------------------------
#aggregate at the sentence level and count syntactic features for each individual sentence
sentence_df <- syntax_df %>%
  filter(is_word) %>%
  group_by(document, sentence_id) %>%
  summarise(
    words = n(), #number of words per sentence
    clauses = sum(is_clause), #number of clauses per sentence
    dep_clauses = sum(is_dep_clause), #number of dependent clauses per sentence
    .groups = "drop"
  )
sentence_df
#---------------------------------------------------------
#Measure 1: mean length of sentence (MLS)
#measure the average sentence length by number of words
#assumption: longer sentences tend to be more syntactically complex

mls_df <- sentence_df %>%
  group_by(document) %>%
  summarise(
    MLS = mean(words),
    .groups = "drop"
  )
mls_df

#---------------------------------------------------------
#Measure 2: clausal density (C/S)
#more clauses allow for the possibility of more syntactic complexity via coordination and subordination

clausal_density_df <- sentence_df %>%
  group_by(document) %>%
  summarise(
    sentences = n(),
    clauses = sum(clauses),
    C_per_S = clauses / sentences,
    .groups = "drop"
  )
clausal_density_df

#---------------------------------------------------------
#Measure 3: subordination(DC/C, DC/S)
#subordination creates hierarchical, embedded structures which is characteristic of complex syntax
#DC/C: What proportion of clauses are dependent
  #higher value means that there is more subordination relative to total clauses
#DC/S: How many dependent clauses are in each sentence on average?
  #higher value means that there are more embeddings/subordination per sentence

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

#---------------------------------------------------------
#Measure 4: coordination (Coord/C, Coord/S)
#measures how much the author links clauses with "and", "but", and "or"
#a measure of how horizontal the text is; count the frequency of parallel syntactic structures

#Densities of coordination:
  #Coord/C: total number of coordination markers divided by number of clauses
  #Coord/S: total number of coordination markers divided by number of sentences

coordination_df <- syntax_df %>%
  group_by(document) %>%
  summarise(
    coord_relations = sum(is_coord),
    clauses = sum(is_clause),
    sentences = n_distinct(sentence_id),
    Coord_per_C = coord_relations / pmax(clauses, 1),
    Coord_per_S = coord_relations / sentences,
    .groups = "drop"
  )
coordination_df

#---------------------------------------------------------
#Measure 5: phrasal complexity (CN/C, CN/S)
nominal_df <- syntax_df %>%
  group_by(document) %>%
  summarise(
    complex_nominals = sum(is_complex_nominal),
    clauses = sum(is_clause),
    sentences = n_distinct(sentence_id),
    CN_per_C = complex_nominals / pmax(clauses, 1),
    CN_per_S = complex_nominals / sentences,
    .groups = "drop"
  )
nominal_df

#---------------------------------------------------------
#combine all measures

# Combine all measures
all_measures <- mls_df %>% 
  left_join(clausal_density_df %>% 
              select(document, C_per_S), by = "document") %>%
  left_join(subordination_df %>% 
              select(document, DC_per_C, DC_per_S), by = "document") %>%
  left_join(coordination_df %>% 
              select(document, Coord_per_C, Coord_per_S), by = "document") %>%
  left_join(nominal_df %>% 
              select(document, CN_per_C, CN_per_S), by = "document")

all_measures %>%
  knitr::kable(
    digits = 2,
    col.names = c(
      "Document", 
      "MLS", 
      "C/S", 
      "DC/C", 
      "DC/S", 
      "Coord/C", 
      "Coord/S", 
      "CN/C", 
      "CN/S")
  )
  
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