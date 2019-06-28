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

get_terms(de_model_unfiltered, 10)
get_terms(de_model_filtered, 10)
