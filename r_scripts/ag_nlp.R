###########################################
#                                         #
#         Text analysis of PDF            #
#  submissions to parliamentary inquiry   #
#       on agricultural innovation        #
#                                         #
###########################################

# Standing Committee on Agriculture and Industry

# Terms of reference: Investigate (a) improvements in the efficiency of agricultural 
# practices due to new technology, and the scope for further improvements, (b) emerging 
# technology relevant to the agricultural sector, in areas including but not limited to
# telecommunications, remote monitoring and drones, plant genomics, and agricultural
# chemicals, and (c) barriers to the adoption of emerging technology.

# 116 submissions. Closing date 4 May 2016.

# ********** Build corpus ********** #

# Create dataframe of pdf submissions.

require(readtext)

ag_df <- readtext("~/ownCloud/digiscape/submissions/agricultural innovation/*.pdf")

# Do some preliminary cleaning.

require(tidyverse)
require(qdap)
require(qdapRegex)

ag_clean <- ag_df %>% mutate(text = clean(text),
                          # remove URLs
                          text = rm_url(text, pattern = "@rm_url2"),
                          # Get rid of underscores
                          text = str_replace_all(text, "_", " "),
                          # Get rid of weird characters
                          text = str_replace_all(text,"\u2019s |'s"," "),
                          # Get rid of numbers
                          text = str_remove_all(text, "[0-9]")) 

# identify submissions that reference agriculture or farming

ag_digital <- ag_clean %>%
  filter(str_detect(text, "digital"))

# Extract emails.

ag_email <- ag_clean %>% 
  mutate(email = tolower(str_extract(text, "\\S+@\\S+"))) %>%
  select(doc_id, email) %>%
  filter(!is.na(email))
# Get standard stop words.

require(tidytext)

data(stop_words)

# Create a tidy corpus.

ag_token <- ag_clean %>% 
  # tokenize
  unnest_tokens(word, text) %>%
  # ditch numbers
  filter(!str_detect(word, "[0-9] | .")) %>%
  # ditch superfluous words
  anti_join(stop_words) 

# Custom stop words

custom <- data.frame(word = c("agricultural",
                              "innovation",
                              "government",
                              "australia",
                              "australian",
                              "parliamentary",
                              "submission",
                              "standing",
                              "committee"), stringsAsFactors = F)

ag_unfiltered <- ag_token %>%
  anti_join(custom)

# Plot most frequently used words - all submissions.

require(ggplot2)

ag_unfiltered %>% 
  count(word, sort = T) %>%
  top_n(20) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


# Generate list of documents that reference "agriculture" or "farming".

ag_digital_token <- ag_digital %>% 
  # tokenize
  unnest_tokens(word, text) %>%
  # ditch numbers
  filter(!str_detect(word, "[0-9] | .")) %>%
  # ditch superfluous words
  anti_join(stop_words) 

ag_filtered <- ag_digital_token %>%
  anti_join(custom)

# Plot most frequently used words - filtered submissions.

ag_filtered %>% 
  count(word, sort = T) %>%
  top_n(20) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Generate bigrams for unfiltered submissions.

require(tidyr)

ag_bigram <- ag_clean %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word,
         !word1 %in% custom$word, 
         !word2 %in% custom$word) %>%
  unite(bigram, word1, word2, sep = " ") 


# Plot 20 most frequently used bigrams - all submissions.

ag_bigram %>% 
  count(bigram, sort = T) %>%
  top_n(20) %>%
  mutate(bigram = reorder(bigram,n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Plot 20 most frequently used bigrams - filtered submissions.

ag_bigram_digital <- ag_digital %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word,
         !word1 %in% custom$word, 
         !word2 %in% custom$word) %>%
  unite(bigram, word1, word2, sep = " ") 


ag_bigram_digital %>% 
  count(bigram, sort = T) %>%
  top_n(20) %>%
  mutate(bigram = reorder(bigram,n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Perform sentiment analysis.

ag_filtered %>% 
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

ag_unfiltered %>% 
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

ag_unfiltered_dtm <- ag_unfiltered %>%
  # stem words
  # mutate(word = wordStem(word, language = "english")) %>%
  # count words
  count(doc_id, word) %>%
  # create document frequency matrix
  cast_dtm(doc_id, word, n)

ag_filtered_dtm <- ag_filtered %>%
  # stem words
  # mutate(word = wordStem(word, language = "english")) %>%
  # count words
  count(doc_id, word) %>%
  # create document frequency matrix
  cast_dtm(doc_id, word, n)

require(topicmodels)

# run model.

ag_model_unfiltered <- LDA(ag_unfiltered_dtm, method = "Gibbs", k = 5, control = list(seed = 1234, best = T))
ag_model_filtered <- LDA(ag_filtered_dtm, method = "Gibbs", k = 5, control = list(seed = 1234, best = T))

# Display topic modelling results with 10 most frequently used words.

get_terms(ag_model_unfiltered, 10)
get_terms(ag_model_filtered, 10)
