#Three well-validated measures of lexical complexity

#Type-Token Ratio (TTR)
# TTR = [Types(unique words)] / [Tokens(total words)]

#Guiraud Index (Root TTR)
#Guiraud = Types / sqrt.(Tokens)
#more stable than TTR across different text lengths
#how many distinct word forms does a txt introduce 
  #once we partially correct for the fact that longer texts repeat terms more often?

#Measure of Textual Lexical Diversity (MTLD)
#how long can a text continue before its lexical diversity drops below a fixed threshold?
#steps:
  #read through the text sequentially
  #calculate a running TTR as it goes
  #count how many times TTR drops below 0.72
  #average the "length" needed before vocab starts repeating
# MTLD = total tokens / total factors
# MTLDfinal = (MTLDforward + MTLDbackward) / 2

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

texts_df %>%
  select(document, author) %>%
  knitr::kable(caption = "Our Two Texts")

#tokenization and cleaning
tokens <- texts_df %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_to_lower(word))

#---------------------------------------------------------
#Calculate TTR for each text
ttr_results <- tokens %>%
  group_by(document, author) %>%
  summarise(
    tokens = n(), #count total words
    types = n_distinct(word), #count unique words
    ttr = types / tokens, 
    .groups = "drop"
  )

#display results
ttr_results %>%
  knitr::kable(
    digits = 3,
    caption = "TTR Results",
    col.names = c(
      "Document",
      "Author",
      "Total Words",
      "Unique Words",
      "TTR"
    )
  )

#visualize results: create bar plot comparing TTR
ggplot(ttr_results, aes(x = author, y = ttr, fill = author)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(ttr, 3)),
            vjust = -0.5, size = 5) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Type-Token Ratio Comparison",
    subtitle = "Higher values = more diverse vocabulary",
    x = NULL, 
    y = "TTR"
  ) +
  theme(legend.position = "none") +
  ylim(0, max(ttr_results$ttr) * 1.15)

#---------------------------------------------------------
#Calculate Guiraud Index for each text
guiraud_results <- tokens %>%
  group_by(document, author) %>%
  summarise(
    tokens = n(),
    types = n_distinct(word), 
    guiraud = types / sqrt(tokens),
    .groups = "drop"
  )

guiraud_results %>%
  knitr::kable(
    digits = 3,
    caption = "Guiraud Index Results",
    col.names = c(
      "Document",
      "Author",
      "Total Words",
      "Unique Words",
      "Guiraud Index"
    )
  )

ggplot(guiraud_results, aes(x = author, y = guiraud, fill = author)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(guiraud, 2)),
            vjust = -0.5, size = 5) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Guiraud Index Comparison",
    subtitle = "Length-corrected measure of lexical diversity",
    x = NULL, 
    y = "Guiraud Index"
  ) +
  theme(legend.position = "none") +
  ylim(0, max(guiraud_results$guiraud) * 1.15)

#---------------------------------------------------------
#this fn converts Tidy data to a file for koRpus, 
#calls koRpus to compute MTLD, and 
#returns everything to Tidy
calculate_mtld <- function(text_string, docname) {
  temp_file <- tempfile(fileext = ".txt")
  writeLines(text_string, temp_file)
  
  tokenized <- tokenize(temp_file, lang = "en") #tokenize with koRpus
  mtld_result <- MTLD(tokenized) #calculate MTLD
  mtld_val <- mtld_result@MTLD$MTLD
  
  unlink(temp_file) #clean up
  return(mtld_val)
}

#Calculate MTLD for both texts
mtld_results <- texts_df %>%
  rowwise() %>%
  mutate(
    mtld = calculate_mtld(text, document)
  ) %>%
  ungroup() %>%
  select(document, author, mtld)

#display results
mtld_results %>%
  knitr::kable(
    digits = 2,
    caption = "MTLD Results",
    col.names = c(
      "Document",
      "Author",
      "MTLD"
    )
  )

# Create a bar plot comparing MTLD
ggplot(mtld_results, aes(x = author, y = mtld, fill = author)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(mtld, 1)), 
            vjust = -0.5, size = 5) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "MTLD Comparison",
    subtitle = "Mean Length of Sequential Word Strings (Higher = More Diverse)",
    x = NULL,
    y = "MTLD Score"
  ) +
  theme(legend.position = "none") +
  ylim(0, max(mtld_results$mtld) * 1.15)