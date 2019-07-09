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

ts_df <- readtext("~/ownCloud/digiscape/submissions/trade systems and the digital economy/*.pdf")

# Do some preliminary cleaning.

require(tidyverse)
require(qdap)
require(qdapRegex)

ts_clean <- ts_df %>% mutate(text = clean(text),
                          # remove URLs
                          text = rm_url(text, pattern = "@rm_url2"),
                          # Get rid of underscores
                          text = str_replace_all(text, "_", " "),
                          # Get rid of weird characters
                          text = str_replace_all(text,"\u2019s |'s"," "),
                          # Get rid of numbers
                          text = str_remove_all(text, "[0-9]")) 

# identify submissions that reference agriculture or farming

ts_agric <- ts_clean %>%
  filter(str_detect(text, "agri|farm"))

# Extract emails.

ts_email <- ts_clean %>% 
  mutate(email = tolower(str_extract(text, "\\S+@\\S+"))) %>%
  select(doc_id, email) %>%
  filter(!is.na(email))
# Get standard stop words.

require(tidytext)

data(stop_words)

# Create a tidy corpus.

ts_token <- ts_clean %>% 
  # tokenize
  unnest_tokens(word, text) %>%
  # ditch numbers
  filter(!str_detect(word, "[0-9] | .")) %>%
  # ditch superfluous words
  anti_join(stop_words) 

# Custom stop words

custom <- data.frame(word = c("joint",
                              "standing",
                              "committee",
                              "trade",
                              "digital",
                              "economy",
                              "australia",
                              "australian",
                              "submission",
                              "inquiry",
                              "system",
                              "government",
                              "department",
                              "committee",
                              "eca",
                              "sydney",
                              "pitt",
                              "st",
                              "gpo",
                              "box",
                              "iso",
                              "iec",
                              "nsw"), stringsAsFactors = F)

ts_unfiltered <- ts_token %>%
  anti_join(custom)

# Plot most frequently used words - all submissions.

require(ggplot2)

ts_unfiltered %>% 
  count(word, sort = T) %>%
  top_n(20) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

ggsave("~/owncloud/digiscape/presentations/ts/ts_unigram_unfiltered.pdf")


# Generate list of documents that reference "agriculture" or "farming".

ts_agric_token <- ts_agric %>% 
  # tokenize
  unnest_tokens(word, text) %>%
  # ditch numbers
  filter(!str_detect(word, "[0-9] | .")) %>%
  # ditch superfluous words
  anti_join(stop_words) 

ts_filtered <- ts_agric_token %>%
  anti_join(stop_words) %>%
  anti_join(custom)

# Plot most frequently used words - filtered submissions.

ts_filtered %>% 
  count(word, sort = T) %>%
  top_n(20) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

ggsave("~/owncloud/digiscape/presentations/ts/ts_unigram_filtered.pdf")

# Generate bigrams for unfiltered submissions.

require(tidyr)

ts_bigram <- ts_clean %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word,
         !word1 %in% custom$word, 
         !word2 %in% custom$word,
         !str_detect(word1, "[0-9]"),
         !str_detect(word2, "[0-9]"),
         !str_detect(word1, "ia"),
         !str_detect(word2, "cepa")) %>%
  unite(bigram, word1, word2, sep = " ") 


# Plot 20 most frequently used bigrams - all submissions.

ts_bigram %>% 
  count(bigram, sort = T) %>%
  top_n(20) %>%
  mutate(bigram = reorder(bigram,n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

ggsave("~/owncloud/digiscape/presentations/ts/ts_bigram_unfiltered.pdf")

# Plot 20 most frequently used bigrams - filtered submissions.

ts_bigram_agric <- ts_agric %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word,
         !word1 %in% custom$word, 
         !word2 %in% custom$word,
         !str_detect(word1, "[0-9]"),
         !str_detect(word2, "[0-9]"),
         !str_detect(word1, "ia"),
         !str_detect(word2, "cepa")) %>%
  unite(bigram, word1, word2, sep = " ") 


ts_bigram_agric %>% 
  count(bigram, sort = T) %>%
  top_n(20) %>%
  mutate(bigram = reorder(bigram,n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

ggsave("~/owncloud/digiscape/presentations/ts/ts_bigram_filtered.pdf")

# Perform sentiment analysis.

ts_filtered %>% 
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

ggsave("~/owncloud/digiscape/presentations/ts/ts_sentiment_filtered.pdf")

ts_unfiltered %>% 
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

ggsave("~/owncloud/digiscape/presentations/ts/ts_sentiment_unfiltered.pdf")

# Do topic modelling with LDA.

ts_unfiltered_dtm <- ts_unfiltered %>%
  # stem words
  # mutate(word = wordStem(word, language = "english")) %>%
  # count words
  count(doc_id, word) %>%
  # create document frequency matrix
  cast_dtm(doc_id, word, n)

ts_filtered_dtm <- ts_filtered %>%
  # stem words
  # mutate(word = wordStem(word, language = "english")) %>%
  # count words
  count(doc_id, word) %>%
  # create document frequency matrix
  cast_dtm(doc_id, word, n)

require(topicmodels)

# run model.

ts_model_unfiltered <- LDA(ts_unfiltered_dtm, method = "Gibbs", k = 5, control = list(seed = 1234, best = T))
ts_model_filtered <- LDA(ts_filtered_dtm, method = "Gibbs", k = 5, control = list(seed = 1234, best = T))

# Display topic modelling results with 10 most frequently used words.

ts_topics_unfiltered <- tidy(ts_model_unfiltered, matrix = "beta")
ts_topics_filtered <- tidy(ts_model_filtered, matrix = "beta")


ts_top_terms_unfiltered <- ts_topics_unfiltered %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ts_top_terms_unfiltered %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

ggsave("~/owncloud/digiscape/presentations/ts/ts_top10_topics_unfiltered.pdf")

ts_top_terms_filtered <- ts_topics_filtered %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ts_top_terms_filtered %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

ggsave("~/owncloud/digiscape/presentations/ts/ts_top10_topics_filtered.pdf")

