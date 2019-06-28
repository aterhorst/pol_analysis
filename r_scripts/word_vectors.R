###########################################
#                                         #
#       Word vector analysis of PDF       #
#   submissions to various inquiries      #
#      related to agriculture and         #     
#          the digital economy            #
#           Version 20190214              #
#           (c) A Terhorst                #
#                                         #
###########################################

# ********** Compile corpora ********** #

require(tidyverse)
require(tidyr)
require(tictoc)

# Run separate scripts for different corpora. 

source("~/ownCloud/digiscape/r_scripts/ts_nlp.R")
source("~/ownCloud/digiscape/r_scripts/da_nlp.R")
source("~/ownCloud/digiscape/r_scripts/de_nlp.R")
source("~/ownCloud/digiscape/r_scripts/nbn_nlp.R")
source("~/ownCloud/digiscape/r_scripts/ag_nlp.R")
source("~/ownCloud/digiscape/r_scripts/dd_nlp.R")

# Create merged corpus from individual corpora. Raw text.

merged_corpus <- bind_rows(ts_clean,
                           da_clean,
                           de_clean,
                           nbn_clean,
                           ag_clean,
                           dd_clean)

# Extract relevant docs (use pre-processed data file "submissions_list.csv").

submissions <- read.csv("~/owncloud/digiscape/submissions/submissions_list.csv", stringsAsFactors = F, header = T)

corpus_filtered <- merged_corpus %>%
  inner_join(submissions %>% select(doc_id), by = "doc_id")

# Tidy up corpus.

corpus <- as_tibble(corpus_filtered) %>%
  # clean doc_id entries
  mutate(doc_id = str_replace_all(doc_id, "\\.pdf|\\_attach1|\\_attach2|\\_attachment|\\.1|\\_attach|\\_att1", ""),
         doc_id = str_squish(doc_id))

# ********** Do word vector analysis ********** #

# Find skipgrams.

# Invoke sliding windows function to id skipgrams

slide_windows <- function(tbl, doc_var, window_size) {
  # each word gets a skipgram (window_size words) starting on the first
  # e.g. skipgram 1 starts on word 1, skipgram 2 starts on word 2
  
  each_total <- tbl %>% 
    group_by(!!doc_var) %>% 
    mutate(doc_total = n(),
           each_total = pmin(doc_total, window_size, na.rm = TRUE)) %>%
    pull(each_total)
  
  rle_each <- rle(each_total)
  counts <- rle_each[["lengths"]]
  counts[rle_each$values != window_size] <- 1
  
  # each word get a skipgram window, starting on the first
  # account for documents shorter than window
  id_counts <- rep(rle_each$values, counts)
  window_id <- rep(seq_along(id_counts), id_counts)
  
  
  # within each skipgram, there are window_size many offsets
  indexer <- (seq_along(rle_each[["values"]]) - 1) %>%
    map2(rle_each[["values"]] - 1,
         ~ seq.int(.x, .x + .y)) %>% 
    map2(counts, ~ rep(.x, .y)) %>%
    flatten_int() +
    window_id
  
  tbl[indexer, ] %>%
    bind_cols(data_frame(window_id)) %>%
    group_by(window_id) %>%
    filter(n_distinct(!!doc_var) == 1) %>%
    ungroup
}

# Compute word vectors. 

require(tidytext)
require(widyr)

# Determine pmi values for each pair of words. Values indicate liklihood of words appearing together or not.
# Takes a very long time (19 hours).

data("stop_words")

tic("skipgrams")
corpus_pmi <- corpus %>%
  unnest_tokens(word, text) %>%
  # remove useless entries
  filter(!str_detect(word, "[0-9]"),
         !str_detect(word, "australia"),
         !str_detect(word, ".au"),
         !str_detect(word, "nsw"),
         !str_detect(word, "\\b[a-z]\\b")) %>%
  anti_join(stop_words) %>%
  add_count(word) %>%
  filter(n > 20) %>%
  select(-n) %>%
  slide_windows(quo(doc_id), 8) %>%
  pairwise_pmi(word, window_id)
toc()

corpus_pmi

# Find word vectors from pmi values.

require(irlba)

tic("word vectors")
tidy_word_vectors <- corpus_pmi %>%
  widely_svd(item1, item2, pmi, nv = 256, maxit = 1000)
toc()

# Check nearest synonyms.

nearest_synonyms <- function(df, token) {
  df %>%
    widely(~ . %*% (.[token, ]), sort = TRUE)(item1, dimension, value) %>%
    select(-item2)
}

tidy_word_vectors %>% nearest_synonyms("digital")

tidy_word_vectors %>% nearest_synonyms("economy")

tidy_word_vectors %>% nearest_synonyms("agriculture")


# Check analogies.

analogy <- function(df, token1, token2, token3) {
  df %>%
    widely(~ . %*% (.[token1, ] - .[token2, ] + .[token3, ]), sort = TRUE)(item1, dimension, value) %>%
    select(-item2)
}

tidy_word_vectors %>%
  analogy("digital", "economy", "agriculture")

# Do PCA.

tic("principal component analysis")
tidy_word_vectors %>%
  filter(dimension <= 24) %>%
  group_by(dimension) %>%
  top_n(12, abs(value)) %>%
  ungroup %>%
  mutate(item1 = reorder(item1, value)) %>%
  group_by(dimension, item1) %>%
  arrange(desc(value)) %>%
  ungroup %>%
  mutate(item1 = factor(paste(item1, dimension, sep = "__"), 
                        levels = rev(paste(item1, dimension, sep = "__"))),
         dimension = factor(paste0("Dimension ", dimension),
                            levels = paste0("Dimension ", as.factor(1:24)))) %>%
  ggplot(aes(item1, value, fill = dimension)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~dimension, scales = "free_y", ncol = 4) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  coord_flip() +
  labs(x = NULL, y = "Value",
       title = "First 24 principal components of the digital agricultural economy corpus",
       subtitle = "Top words contributing to the components that explain the most variation")
toc()
