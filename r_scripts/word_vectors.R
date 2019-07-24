#################################################
#                                               #
#             Digiscape Word Vectors            #
#               Version 20190710                #
#                Andrew Terhorst                #
#                                               #
#################################################

# read submissions

require(tictoc)
require(tidyverse)
require(readtext)

data_dir <- "~/ownCloud/pol_analysis/submissions"

tic("read pdfs")
raw_corpus <- readtext(paste0(data_dir, "/*"))
toc()

# do some preliminary cleaning

require(textclean)

tic("clean corpus")
clean_corpus <- raw_corpus %>% 
  # use textclean functions
  mutate(text = replace_non_ascii(text), # get rid of weird characters
         text = replace_html(text), # remove html
         text = replace_contraction(text), # remove apostrophes
         text = replace_number(text, remove = TRUE), # take out numbers
         doc_id = str_squish(str_replace(doc_id, " .pdf", "")), 
         doc_id = str_squish(str_replace(doc_id, ".pdf", "")),
         doc_id = str_squish(str_replace(doc_id, ".PDF", "")),
         # group sub-documents together by trimming names
         doc_id = case_when(str_detect(doc_id, "ag_") ~ strtrim(doc_id, 10),
                            str_detect(doc_id, "dd_") ~ strtrim(doc_id, 10),
                            str_detect(doc_id, "de_") ~ strtrim(doc_id, 10),
                            str_detect(doc_id, "ts_") ~ strtrim(doc_id, 10),
                            str_detect(doc_id, "nbn_") ~ strtrim(doc_id, 11),
                            str_detect(doc_id, "da_sub_dr") ~ strtrim(doc_id, 13),
                            str_detect(doc_id, "da_sub") ~ strtrim(doc_id, 10),
                            TRUE ~ doc_id)) %>%
  # extract submissions referencing digital ag tech
  filter(case_when(str_detect(doc_id, "ag_") ~ str_detect(text, "digit"),
                   TRUE ~ str_detect(text, "agri|farm"))) 
toc()


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

# compute word vectors. 

# create tidy corpus

require(tidytext)
require(widyr)
require(qdapDictionaries)

custom_stopwords <- data.frame(word = c("government",
                                        "inquiry",
                                        "parliamentary",
                                        "productivity",
                                        "commission",
                                        "department",
                                        "joint",
                                        "standing",
                                        "committee",
                                        "australia",
                                        "australian",
                                        "submission",
                                        "can",
                                        "cent",
                                        "new"), 
                               stringsAsFactors = F) %>%
  bind_rows(data.frame(word = letters, stringsAsFactors = F),
            data.frame(word = tolower(month.name), stringsAsFactors = F))

comp_terms <- read.csv("https://raw.githubusercontent.com/aterhorst/pol_analysis/master/dictionaries/comp_terms_wikipedia.csv", stringsAsFactors = F)

dictionary <- as.data.frame(GradyAugmented, stringsAsFactors = F) %>% 
  select(word = GradyAugmented, everything()) %>%
  bind_rows(comp_terms) %>%
  distinct(word)


tic("skipgrams")
pmi_corpus <- clean_corpus %>%
  # tokenise
  unnest_tokens(word, text) %>%
  # get rid of numbers
  filter(!str_detect(word, "[0-9]+"), word %in% dictionary$word) %>%
  # get rid of stopwords
  anti_join(stop_words) %>%
  anti_join(custom_stopwords) %>%
  slide_windows(quo(doc_id), 8) %>%
  pairwise_pmi(word, window_id)
toc()


# find word vectors from pmi values.

require(irlba)

tic("word vectors")
tidy_word_vectors <- pmi_corpus %>%
  widely_svd(item1, item2, pmi, nv = 256, maxit = 1000)
toc()

# find proximal words.

require(gridExtra)
require(ggpubr)
require(grid)

col <- get_palette(palette = "default", 4)

nearest_synonyms <- function(df, token) {
  df %>%
    widely(~ . %*% (.[token, ]), sort = TRUE)(item1, dimension, value) %>%
    select(-item2)
}


prox_digit <- tidy_word_vectors %>% nearest_synonyms("digital") %>%
  filter(!item1 == "digital") %>%
  top_n(20, value) %>%
  ggplot(aes(reorder(item1, value), value)) +
  geom_col(fill = col[1]) +
  coord_flip() +
  xlab(NULL) +
  ggtitle("Proximal words to *digital*") +
  theme(plot.title = element_text(hjust = 0.5))

prox_econ <- tidy_word_vectors %>% nearest_synonyms("economy") %>%
  filter(!item1 == "economy") %>%
  top_n(20, value) %>%
  ggplot(aes(reorder(item1, value), value)) +
  geom_col(fill = col[2]) +
  coord_flip() +
  xlab(NULL) +
  ggtitle("Proximal words to *economy*") +
  theme(plot.title = element_text(hjust = 0.5))

prox_ag <- tidy_word_vectors %>% nearest_synonyms("agriculture") %>%
  filter(!item1 == "agriculture") %>%
  top_n(20, value) %>%
  ggplot(aes(reorder(item1, value), value)) +
  geom_col(fill = col[3]) +
  coord_flip() +
  xlab(NULL) +
  ggtitle("Proximal words to *agriculture*") +
  theme(plot.title = element_text(hjust = 0.5))

prox_tech <- tidy_word_vectors %>% nearest_synonyms("technology") %>%
  filter(!item1 == "technology") %>%
  top_n(20, value) %>%
  ggplot(aes(reorder(item1, value), value)) +
  geom_col(fill = col[4]) +
  coord_flip() +
  xlab(NULL) +
  ggtitle("Proximal words to *technology*") +
  theme(plot.title = element_text(hjust = 0.5))

prox <- arrangeGrob(prox_digit, prox_econ, prox_ag, prox_tech, nrow = 2)

ggsave("~/owncloud/digiscape/presentations/proximal_words.pdf", prox)


# check analogies.

analogy <- function(df, token1, token2, token3) {
  df %>%
    widely(~ . %*% (.[token1, ] - .[token2, ] + .[token3, ]), sort = TRUE)(item1, dimension, value) %>%
    select(-item2)
}

tidy_word_vectors %>%
  analogy("digital", "technology", "agriculture")


# principal comonent analysis.

tic("principal component analysis")
pca <- tidy_word_vectors %>%
  filter(dimension <= 16) %>%
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
  labs(x = NULL, y = "Value")
ggsave("~/owncloud/digiscape/presentations/word_vector_pca.pdf", pca)
toc()
