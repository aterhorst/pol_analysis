###########################################
#                                         #
#   Text analysis of PDF submissions to   #
#     to the parliamentary inquiry on     #
#     the rollout of the NBN in rural     # 
#           and regional areas            #
#                                         #
#                                         #
###########################################

# Joint Standing Committee on the National Broadband Network

# Terms of reference: Investigate the rollout of the National Broadband Network (NBN) 
# in rural and regional areas, specifically focused on the capacity and reliability of 
# the satellite, fixed wireless and fixed line networks, in particular the:
# - planning, mapping and eligibility for satellite, fixed wireless and fixed line services
# - adequacy of plans and service reliability of satellite, fixed wireless and fixed line services
# - issues in relation to the future capacity of satellite, fixed wireless and fixed line services
# - provision of service by alternative providers of satellite, fixed wireless and fixed line services
# - any other related matters.

# 22 submissions. Closing date 29 March 2018.

# ********** Build corpus ********** #

# Create dataframe of pdf submissions.

require(readtext)

nbn_df <- readtext("~/ownCloud/digiscape/submissions/nbn rollout rural and regional areas/*.pdf")

# Do some preliminary cleaning.

require(tidyverse)
require(qdap)
require(qdapRegex)

nbn_clean <- nbn_df %>% mutate(text = clean(text),
                          # remove URLs
                          text = rm_url(text, pattern = "@rm_url2"),
                          # Get rid of underscores
                          text = str_replace_all(text, "_", " "),
                          # Get rid of weird characters
                          text = str_replace_all(text,"\u2019s |'s"," "),
                          # Get rid of numbers
                          text = str_remove_all(text, "[0-9]")) 

# identify submissions that reference agriculture or farming

nbn_agric <- nbn_clean %>%
  filter(str_detect(text, "agri|farm"))

# Extract emails.

nbn_email <- nbn_clean %>% 
  mutate(email = tolower(str_extract(text, "\\S+@\\S+"))) %>%
  select(doc_id, email) %>%
  filter(!is.na(email))
# Get standard stop words.

require(tidytext)

data(stop_words)

# Create a tidy corpus.

nbn_token <- nbn_clean %>% 
  # tokenize
  unnest_tokens(word, text) %>%
  # ditch numbers
  filter(!str_detect(word, "[0-9] | .")) %>%
  # ditch superfluous words
  anti_join(stop_words) 

# Custom stop words

custom <- data.frame(word = c("nbn",
                              "rollout",
                              "rural",
                              "regional",
                              "government",
                              "joint",
                              "standing",
                              "committee",
                              "australia",
                              "australian",
                              "submission",
                              "national",
                              "broadband",
                              "network",
                              "birrr",
                              "nbntm"), stringsAsFactors = F)

nbn_unfiltered <- nbn_token %>%
  anti_join(custom)

# Plot most frequently used words - all submissions.

require(ggplot2)

nbn_unfiltered %>% 
  count(word, sort = T) %>%
  top_n(20) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


# Generate list of documents that reference "agriculture" or "farming".

nbn_agric_token <- nbn_agric %>% 
  # tokenize
  unnest_tokens(word, text) %>%
  # ditch numbers
  filter(!str_detect(word, "[0-9] | .")) %>%
  # ditch superfluous words
  anti_join(stop_words) 

nbn_filtered <- nbn_agric_token %>%
  anti_join(custom)

# Plot most frequently used words - filtered submissions.

nbn_filtered %>% 
  count(word, sort = T) %>%
  top_n(20) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Generate bigrams for unfiltered submissions.

require(tidyr)

nbn_bigram <- nbn_clean %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word,
         !word1 %in% custom$word, 
         !word2 %in% custom$word) %>%
  unite(bigram, word1, word2, sep = " ") 


# Plot 20 most frequently used bigrams - all submissions.

nbn_bigram %>% 
  count(bigram, sort = T) %>%
  top_n(20) %>%
  mutate(bigram = reorder(bigram,n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Plot 20 most frequently used bigrams - filtered submissions.

nbn_bigram_agric <- nbn_agric %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word,
         !word1 %in% custom$word, 
         !word2 %in% custom$word) %>%
  unite(bigram, word1, word2, sep = " ") 


nbn_bigram_agric %>% 
  count(bigram, sort = T) %>%
  top_n(20) %>%
  mutate(bigram = reorder(bigram,n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Perform sentiment analysis.

nbn_filtered %>% 
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

nbn_unfiltered %>% 
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

# Do topic modelling with LDA.

nbn_unfiltered_dtm <- nbn_unfiltered %>%
  # stem words
  # mutate(word = wordStem(word, language = "english")) %>%
  # count words
  count(doc_id, word) %>%
  # create document frequency matrix
  cast_dtm(doc_id, word, n)

nbn_filtered_dtm <- nbn_filtered %>%
  # stem words
  # mutate(word = wordStem(word, language = "english")) %>%
  # count words
  count(doc_id, word) %>%
  # create document frequency matrix
  cast_dtm(doc_id, word, n)

require(topicmodels)

# run model.

nbn_model_unfiltered <- LDA(nbn_unfiltered_dtm, method = "Gibbs", k = 5, control = list(seed = 1234, best = T))
nbn_model_filtered <- LDA(nbn_filtered_dtm, method = "Gibbs", k = 5, control = list(seed = 1234, best = T))

# Display topic modelling results with 10 most frequently used words.

get_terms(nbn_model_unfiltered, 10)
get_terms(nbn_model_filtered, 10)
