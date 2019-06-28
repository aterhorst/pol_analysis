###########################################
#                                         #
#   Text analysis of PDF submissions to   #
#     to the parliamentary inquiry on    #
#         the digital delivery of         #
#           government services           #
#                                         # 
#                                         #
###########################################

# Finance and Public Administration References Committee

# Terms of reference: Investigate the digital delivery of government services.

# 26 submissions. Closing date 22 December 2017.

# ********** Build corpus ********** #

# Create dataframe of pdf submissions.

require(readtext)

dd_df <- readtext("~/ownCloud/digiscape/submissions/digital delivery of government services/*.pdf")

# Do some preliminary cleaning.

require(tidyverse)
require(qdap)
require(qdapRegex)

dd_clean <- dd_df %>% mutate(text = clean(text),
                          # remove URLs
                          text = rm_url(text, pattern = "@rm_url2"),
                          # Get rid of underscores
                          text = str_replace_all(text, "_", " "),
                          # Get rid of weird characters
                          text = str_replace_all(text,"\u2019s |'s"," "),
                          # Get rid of numbers
                          text = str_remove_all(text, "[0-9]")) 

# identify submissions that reference agriculture or farming

dd_agric <- dd_clean %>%
  filter(str_detect(text, "agri|farm"))

# Extract emails.

dd_email <- dd_clean %>% 
  mutate(email = tolower(str_extract(text, "\\S+@\\S+"))) %>%
  select(doc_id, email) %>%
  filter(!is.na(email))
# Get standard stop words.

require(tidytext)

data(stop_words)

# Create a tidy corpus.

dd_token <- dd_clean %>% 
  # tokenize
  unnest_tokens(word, text) %>%
  # ditch numbers
  filter(!str_detect(word, "[0-9] | .")) %>%
  # ditch superfluous words
  anti_join(stop_words) 

# Custom stop words

custom <- data.frame(word = c("digital",
                              "services",
                              "delivery",
                              "government",
                              "australia",
                              "australian",
                              "submission",
                              "accan.org.au",
                              "june",
                              "july",
                              "au"), stringsAsFactors = F)

dd_unfiltered <- dd_token %>%
  anti_join(custom)

# Plot most frequently used words - all submissions.

require(ggplot2)

dd_unfiltered %>% 
  count(word, sort = T) %>%
  top_n(20) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


# Generate list of documents that reference "agriculture" or "farming".

dd_agric_token <- dd_agric %>% 
  # tokenize
  unnest_tokens(word, text) %>%
  # ditch numbers
  filter(!str_detect(word, "[0-9] | .")) %>%
  # ditch superfluous words
  anti_join(stop_words) 

dd_filtered <- dd_agric_token %>%
  anti_join(custom)

# Plot most frequently used words - filtered submissions.

dd_filtered %>% 
  count(word, sort = T) %>%
  top_n(20) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Generate bigrams for unfiltered submissions.

require(tidyr)

dd_bigram <- dd_clean %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word,
         !word1 %in% custom$word, 
         !word2 %in% custom$word) %>%
  unite(bigram, word1, word2, sep = " ") 


# Plot 20 most frequently used bigrams - all submissions.

dd_bigram %>% 
  count(bigram, sort = T) %>%
  top_n(20) %>%
  mutate(bigram = reorder(bigram,n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Plot 20 most frequently used bigrams - filtered submissions.

dd_bigram_agric <- dd_agric %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word,
         !word1 %in% custom$word, 
         !word2 %in% custom$word) %>%
  unite(bigram, word1, word2, sep = " ") 


dd_bigram_agric %>% 
  count(bigram, sort = T) %>%
  top_n(20) %>%
  mutate(bigram = reorder(bigram,n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Perform sentiment analysis.

dd_filtered %>% 
  # join sentiment library 
  inner_join(get_sentiments("bing")) %>%
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

dd_unfiltered %>% 
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

dd_unfiltered_dtm <- dd_unfiltered %>%
  # stem words
  # mutate(word = wordStem(word, language = "english")) %>%
  # count words
  count(doc_id, word) %>%
  # create document frequency matrix
  cast_dtm(doc_id, word, n)

dd_filtered_dtm <- dd_filtered %>%
  # stem words
  # mutate(word = wordStem(word, language = "english")) %>%
  # count words
  count(doc_id, word) %>%
  # create document frequency matrix
  cast_dtm(doc_id, word, n)

require(topicmodels)

# run model.

dd_model_unfiltered <- LDA(dd_unfiltered_dtm, method = "Gibbs", k = 5, control = list(seed = 1234, best = T))
dd_model_filtered <- LDA(dd_filtered_dtm, method = "Gibbs", k = 5, control = list(seed = 1234, best = T))

# Display topic modelling results with 10 most frequently used words.

get_terms(dd_model_unfiltered, 10)
get_terms(dd_model_filtered, 10)
