library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(tibble)
library(readr)
library(tidyverse)
library(readr)
library(purrr)

#retrieve files
all_files <- list.files("texts/", full.names = TRUE)
all_files

raw_data <- bind_rows(
  lapply(all_files, read.table, header = TRUE)
)
raw_data