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

da_df <- readtext("~/ownCloud/digiscape/submissions/data availability and use/*.pdf")

# Do some preliminary cleaning.

require(tidyverse)
require(qdap)
require(qdapRegex)

da_clean <- da_df %>% mutate(text = clean(text),
                          # remove URLs
                          text = rm_url(text, pattern = "@rm_url2"),
                          # Get rid of underscores
                          text = str_replace_all(text, "_", " "),
                          # Get rid of weird characters
                          text = str_replace_all(text,"\u2019s |'s"," "),
                          # Get rid of numbers
                          text = str_remove_all(text, "[0-9]")) 

# identify submissions that reference agriculture or farming

da_agric <- da_clean %>%
  filter(str_detect(text, "agri|farm"))

# Extract emails.

da_email <- da_clean %>% 
  mutate(email = tolower(str_extract(text, "\\S+@\\S+"))) %>%
  select(doc_id, email) %>%
  filter(!is.na(email))
# Get standard stop words.

require(tidytext)

data(stop_words)

# Create a tidy corpus.

da_token <- da_clean %>% 
  # tokenize
  unnest_tokens(word, text) %>%
  # ditch numbers
  filter(!str_detect(word, "[0-9] | .")) %>%
  # ditch superfluous words
  anti_join(stop_words) 

# Custom stop words

custom <- data.frame(word = c("data",
                              "information",
                              "availability",
                              "access",
                              "australia",
                              "australian",
                              "productivity",
                              "commission",
                              "submission",
                              "po",
                              "box",
                              "pty",
                              "limited",
                              "abn",
                              "corporation"), stringsAsFactors = F)

da_unfiltered <- da_token %>%
  anti_join(custom)

# Plot most frequently used words - all submissions.

require(ggplot2)

da_unfiltered %>% 
  count(word, sort = T) %>%
  top_n(20) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

ggsave("~/owncloud/digiscape/presentations/da/da_unfiltered_unigrams.pdf")


# Generate list of documents that reference "agriculture" or "farming".

da_agric_token <- da_agric %>% 
  # tokenize
  unnest_tokens(word, text) %>%
  # ditch numbers
  filter(!str_detect(word, "[0-9] | .")) %>%
  # ditch superfluous words
  anti_join(stop_words) 

da_filtered <- da_agric_token %>%
  anti_join(stop_words) %>%
  anti_join(custom)

# Plot most frequently used words - filtered submissions.

da_filtered %>% 
  count(word, sort = T) %>%
  top_n(20) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

ggsave("~/owncloud/digiscape/presentations/da/da_filtered_unigrams.pdf")

# Generate bigrams for unfiltered submissions.

require(tidyr)

da_bigram <- da_clean %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word,
         !word1 %in% custom$word, 
         !word2 %in% custom$word) %>%
  unite(bigram, word1, word2, sep = " ") 


# Plot 20 most frequently used bigrams - all submissions.

da_bigram %>% 
  count(bigram, sort = T) %>%
  top_n(20) %>%
  mutate(bigram = reorder(bigram,n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

ggsave("~/owncloud/digiscape/presentations/da/da_unfiltered_bigrams.pdf")

# Plot 20 most frequently used bigrams - filtered submissions.

da_bigram_agric <- da_agric %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word,
         !word1 %in% custom$word, 
         !word2 %in% custom$word) %>%
  unite(bigram, word1, word2, sep = " ") 


da_bigram_agric %>% 
  count(bigram, sort = T) %>%
  top_n(20) %>%
  mutate(bigram = reorder(bigram,n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

ggsave("~/owncloud/digiscape/presentations/da/da_filtered_bigrams.pdf")

# Perform sentiment analysis.

da_filtered %>% 
  # join sentiment library 
  inner_join(get_sentiments("bing") %>% filter(word != "cloud")) %>%
  count(word, sentiment, sort = T) %>%
  group_by(sentiment) %>%
  arrange(sentiment) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip()

ggsave("~/owncloud/digiscape/presentations/da/da_filtered_sentiment.pdf")

da_unfiltered %>% 
  # join sentiment library 
  inner_join(get_sentiments("bing") %>% filter(word != "cloud")) %>%
  count(word, sentiment, sort = T) %>%
  group_by(sentiment) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip()

ggsave("~/owncloud/digiscape/presentations/da/da_unfiltered_sentiment.pdf")

# Do topic modelling with LDA.

da_unfiltered_dtm <- da_unfiltered %>%
  # stem words
  # mutate(word = wordStem(word, language = "english")) %>%
  # count words
  count(doc_id, word) %>%
  # create document frequency matrix
  cast_dtm(doc_id, word, n)

da_filtered_dtm <- da_filtered %>%
  # stem words
  # mutate(word = wordStem(word, language = "english")) %>%
  # count words
  count(doc_id, word) %>%
  # create document frequency matrix
  cast_dtm(doc_id, word, n)

require(topicmodels)

# run model.

da_model_unfiltered <- LDA(da_unfiltered_dtm, method = "Gibbs", k = 5, control = list(seed = 1234, best = T))
da_model_filtered <- LDA(da_filtered_dtm, method = "Gibbs", k = 5, control = list(seed = 1234, best = T))

# Display topic modelling results with 10 most frequently used words.

da_topics_unfiltered <- tidy(da_model_unfiltered, matrix = "beta")
da_topics_filtered <- tidy(da_model_filtered, matrix = "beta")


da_top_terms_unfiltered <- da_topics_unfiltered %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

da_top_terms_unfiltered %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

ggsave("~/owncloud/digiscape/presentations/da/da_top10_topics_unfiltered.pdf")

da_top_terms_filtered <- da_topics_filtered %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

da_top_terms_filtered %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

ggsave("~/owncloud/digiscape/presentations/da/da_top10_topics_filtered.pdf")

