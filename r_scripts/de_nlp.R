###########################################
#                                         #
#   Text analysis of PDF submissions to   #
#     to the parliamentary inquiry on     #
#        the trade system and the         #
#             digital economy             # 
#                                         #
#                                         #
###########################################

# Joint Standing Committee on Trade and Investment Growth

# Terms of reference: To inquire into and report on the responsiveness of Australia's 
# trade architecture and regulatory system to the contemporary needs of the digital economy 
# and disruptive technology, and measures to improve the cyber resilience of Australia's 
# trade-focused business sector.

# 22 submissions. Closing date 01 December 2018.

# ********** Build corpus ********** #

# Create dataframe of pdf submissions.

require(readtext)

de_df <- readtext("~/ownCloud/digiscape/submissions/digital economy strategy/*.pdf")

# Do some preliminary cleaning.

require(tidyverse)
require(qdap)
require(qdapRegex)

de_clean <- de_df %>% mutate(text = clean(text),
                          # remove URLs
                          text = rm_url(text, pattern = "@rm_url2"),
                          # Get rid of underscores
                          text = str_replace_all(text, "_", " "),
                          # Get rid of weird characters
                          text = str_replace_all(text,"\u2019s |'s"," "),
                          # Get rid of numbers
                          text = str_remove_all(text, "[0-9]")) 

# identify submissions that reference agriculture or farming

de_agric <- de_clean %>%
  filter(str_detect(text, "agri|farm"))

# Extract emails.

de_email <- de_clean %>% 
  mutate(email = tolower(str_extract(text, "\\S+@\\S+"))) %>%
  select(doc_id, email) %>%
  filter(!is.na(email))
# Get standard stop words.

require(tidytext)

data(stop_words)

# Create a tidy corpus.

de_token <- de_clean %>% 
  # tokenize
  unnest_tokens(word, text) %>%
  # ditch numbers
  filter(!str_detect(word, "[0-9] | .")) %>%
  # ditch superfluous words
  anti_join(stop_words) 

# Custom stop words

custom <- data.frame(word = c("digital",
                              "economy",
                              "australia",
                              "australian",
                              "government",
                              "submission",
                              "consultation",
                              "discussion",
                              "paper",
                              "po",
                              "box",
                              "pty",
                              "limited",
                              "abn",
                              "corporation"), stringsAsFactors = F)

de_unfiltered <- de_token %>%
  anti_join(custom)

# Plot most frequently used words - all submissions.

require(ggplot2)

de_unfiltered %>% 
  count(word, sort = T) %>%
  top_n(20) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

ggsave("~/owncloud/digiscape/presentations/de/de_unigram_unfiltered.pdf")


# Generate list of documents that reference "agriculture" or "farming".

de_agric_token <- de_agric %>% 
  # tokenize
  unnest_tokens(word, text) %>%
  # ditch numbers
  filter(!str_detect(word, "[0-9] | .")) %>%
  # ditch superfluous words
  anti_join(stop_words) 

de_filtered <- de_agric_token %>%
  anti_join(stop_words) %>%
  anti_join(custom)

# Plot most frequently used words - filtered submissions.

de_filtered %>% 
  count(word, sort = T) %>%
  top_n(20) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

ggsave("~/owncloud/digiscape/presentations/de/de_unigram_filtered.pdf")

# Generate bigrams for unfiltered submissions.

require(tidyr)

de_bigram <- de_clean %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word,
         !word1 %in% custom$word, 
         !word2 %in% custom$word) %>%
  unite(bigram, word1, word2, sep = " ") 


# Plot 20 most frequently used bigrams - all submissions.

de_bigram %>% 
  count(bigram, sort = T) %>%
  top_n(20) %>%
  mutate(bigram = reorder(bigram,n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

ggsave("~/owncloud/digiscape/presentations/de/de_bigram_unfiltered.pdf")

# Plot 20 most frequently used bigrams - filtered submissions.

de_bigram_agric <- de_agric %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word,
         !word1 %in% custom$word, 
         !word2 %in% custom$word) %>%
  unite(bigram, word1, word2, sep = " ") 


de_bigram_agric %>% 
  count(bigram, sort = T) %>%
  top_n(20) %>%
  mutate(bigram = reorder(bigram,n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

ggsave("~/owncloud/digiscape/presentations/de/de_bigram_filtered.pdf")

# Perform sentiment analysis.

de_filtered %>% 
  # join sentiment library 
  inner_join(get_sentiments("bing") %>% filter(word != "cloud")) %>%
  count(word, sentiment, sort = T) %>%
  group_by(sentiment) %>%
  arrange(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip()

ggsave("~/owncloud/digiscape/presentations/de/de_sentiment_filtered.pdf")

de_unfiltered %>% 
  # join sentiment library 
  inner_join(get_sentiments("bing") %>% filter(word != "cloud")) %>%
  count(word, sentiment, sort = T) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip()

ggsave("~/owncloud/digiscape/presentations/de/de_sentiment_unfiltered.pdf")

# Do topic modelling with LDA.

de_unfiltered_dtm <- de_unfiltered %>%
  # stem words
  # mutate(word = wordStem(word, language = "english")) %>%
  # count words
  count(doc_id, word) %>%
  # create document frequency matrix
  cast_dtm(doc_id, word, n)

de_filtered_dtm <- de_filtered %>%
  # stem words
  # mutate(word = wordStem(word, language = "english")) %>%
  # count words
  count(doc_id, word) %>%
  # create document frequency matrix
  cast_dtm(doc_id, word, n)

require(topicmodels)

# run model.

de_model_unfiltered <- LDA(de_unfiltered_dtm, method = "Gibbs", k = 5, control = list(seed = 1234, best = T))
de_model_filtered <- LDA(de_filtered_dtm, method = "Gibbs", k = 5, control = list(seed = 1234, best = T))

# Display topic modelling results with 10 most frequently used words.

de_topics_unfiltered <- tidy(de_model_unfiltered, matrix = "beta")
de_topics_filtered <- tidy(de_model_filtered, matrix = "beta")


de_top_terms_unfiltered <- de_topics_unfiltered %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

de_top_terms_unfiltered %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

ggsave("~/owncloud/digiscape/presentations/de/de_top10_topics_unfiltered.pdf")

de_top_terms_filtered <- de_topics_filtered %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

de_top_terms_filtered %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

ggsave("~/owncloud/digiscape/presentations/de/de_top10_topics_filtered.pdf")

# wordvector analysis

# find skipgrams.

# invoke sliding windows function to id skipgrams

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

# create corpus subset

de_wv_filtered <- de_clean %>%
  inner_join(de_filtered %>% distinct(doc_id))

# Compute word vectors. 

require(widyr)
require(tictoc)

# Determine pmi values for each pair of words. Values indicate liklihood of words appearing together or not.
# Takes a very long time (19 hours).

data("stop_words")

tic("skipgrams")
de_wv_filtered_pmi <- de_wv_filtered %>%
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

require(irlba)

tic("word vectors")
de_word_vectors_filtered <- de_wv_filtered_pmi %>%
  widely_svd(item1, item2, pmi, nv = 256, maxit = 1000)
toc()

# Check nearest synonyms.

nearest_synonyms <- function(df, token) {
  df %>%
    widely(~ . %*% (.[token, ]), sort = TRUE)(item1, dimension, value) %>%
    select(-item2)
}

de_word_vectors_filtered %>% nearest_synonyms("digital")

de_word_vectors_filtered %>% nearest_synonyms("economy")

de_word_vectors_filtered %>% nearest_synonyms("agriculture")


# Check analogies.

analogy <- function(df, token1, token2, token3) {
  df %>%
    widely(~ . %*% (.[token1, ] - .[token2, ] + .[token3, ]), sort = TRUE)(item1, dimension, value) %>%
    select(-item2)
}

de_word_vectors_filtered %>%
  analogy("digital", "economy", "agriculture")

# Do PCA.

tic("principal component analysis")
de_word_vectors_filtered %>%
  filter(dimension <= 12) %>%
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
       title = "First 12 principal components",
       subtitle = "Top words contributing to the components that explain the most variation")
toc()

ggsave("~/owncloud/digiscape/presentations/de/de_wv_pca_filtered.pdf")


# unfiltered

tic("skipgrams")
de_wv_unfiltered_pmi <- de_clean %>%
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

tic("word vectors")
de_word_vectors_unfiltered <- de_wv_filtered_pmi %>%
  widely_svd(item1, item2, pmi, nv = 256, maxit = 1000)
toc()

# Check nearest synonyms.

de_word_vectors_unfiltered %>% nearest_synonyms("digital")

de_word_vectors_unfiltered %>% nearest_synonyms("economy")

de_word_vectors_unfiltered %>% nearest_synonyms("agriculture")


# Check analogies.


de_word_vectors_unfiltered %>%
  analogy("digital", "economy", "agriculture")

# Do PCA.

tic("principal component analysis")
de_word_vectors_unfiltered %>%
  filter(dimension <= 12) %>%
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
       title = "First 12 principal components",
       subtitle = "Top words contributing to the components that explain the most variation")
toc()

ggsave("~/owncloud/digiscape/presentations/de/de_wv_pca_unfiltered.pdf")
