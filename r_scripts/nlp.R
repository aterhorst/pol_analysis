###########################################
#                                         #
#           Text Analysis of PDF          #
#         documents for DigiScape         #
#                                         #
#                                         #
###########################################

# 1. digital economy strategy

setwd("~/owncloud/digiscape/submissions/digital economy strategy/")
setwd("/OSM/MEL/DPS_OI_Network/work/ownCloud/digiscape/submissions/digital economy strategy/")

# 2. digital delivery of government services

setwd("~/owncloud/digiscape/submissions/digital delivery of government services/")
setwd("/OSM/MEL/DPS_OI_Network/work/ownCloud/digiscape/submissions/digital delivery of governmnet services/")


# 3. agricultural innovation

setwd("~/ownCloud/digiscape/submissions/agricultural innovation")

# 4. data availability and use

setwd("~/ownCloud/digiscape/submissions/data availability and use")

# 5. rural & regional nbn roll-out

setwd("~/ownCloud/digiscape/submissions/nbn rollout rural and regional areas")

# trade systems

setwd("~/ownCloud/digiscape/submissions/trade systems and the digital economy")

# read pdf documents into R for a particular inquiry

require(readtext)

df <- readtext("*.pdf")

# preliminary cleaning

require(tidyverse)
require(qdap)
require(qdapRegex)

df_clean <- df %>% mutate(text = clean(text),
                            text = rm_url(text, pattern = "@rm_url2"),
                            text = str_replace_all(text, "_", " "),
                            text = str_replace_all(text,"\u2019s |'s"," ")) 

# get stop words

require(tidytext)

data(stop_words)

# create tidy dataset

tidy_df <- df_clean %>% 
  # tokenize
  unnest_tokens(word, text) %>%
  # ditch numbers
  filter(!str_detect(word, "[0-9]")) %>%
  # ditch superfluous words
  anti_join(stop_words) 



# subset docs

require(SnowballC)

subset <- tidy_df %>%
  mutate(word = wordStem(word, language = "english")) %>%
#  filter(word %in% c("agricultur", "farm", "agribusi")) %>%
  filter(word %in% c("digit")) %>%
  distinct(doc_id) 


# common words

require(ggplot2)

tidy_df %>% 
  count(word, sort = T) %>%
  top_n(50) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# extract emails

df_email <- df_clean %>% 
  mutate(email = tolower(str_extract(text, "\\S+@\\S+"))) %>%
  select(doc_id, email) %>%
#  filter(!is.na(email) & email != "digitaleconomy@industry.gov.au") %>%
  filter(!is.na(email)) %>%
  right_join(subset)



# define custom stop words

custom_stop_words <- data.frame(word = c("digital",
                                         "government",
                                         "data",
                                         "economy",
                                         "submission",
                                         "department",
                                         "organisation",
                                         "paper",
                                         "consultation",
                                         "australia",
                                         "australian",
                                         "corporation",
                                         "box",
                                         "pi",
                                         "po",
                                         "gpo",
                                         "uuid",
                                         "vc",
                                         "adia",
                                         "fya",
                                         "abn"), stringsAsFactors = F)

# agricultural innovation stopwords
custom_stop_words <- data.frame(word = c("agricultural",
                                         "parliament",
                                         "reps",
                                         "public",
                                         "representatives",
                                         "standing",
                                         "committee",
                                         "secretary",
                                         "house",
                                         "submission",
                                         "paper",
                                         "consultation",
                                         "australia",
                                         "australian",
                                         "corporation",
                                         "po",
                                         "gpo",
                                         "box",
                                         "journal",
                                         "vol"), stringsAsFactors = F)

custom_stop_words <- data.frame(word = c("data",
                                         "access",
                                         "australia",
                                         "australian",
                                         "corporation",
                                         "innovation",
                                         "po",
                                         "gpo",
                                         "box"), stringsAsFactors = F)

custom_stop_words <- data.frame(word = c("nbn",
                                         "po",
                                         "gpo",
                                         "box"), stringsAsFactors = F)


# subset docs

tidy_df_filtered <- tidy_df %>%
  inner_join(subset, by = "doc_id") %>%
  anti_join(custom_stop_words)



# common words

require(ggplot2)

tidy_df_filtered %>% 
  count(word, sort = T) %>%
  top_n(50) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# common words after stemming

tidy_df_filtered %>% 
  mutate(word = wordStem(word)) %>%
  count(word, sort = T) %>%
  top_n(50) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# bigrams

require(tidyr)

tidy_df_bigram <- df_clean %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  inner_join(subset, by = "doc_id") %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word,
         !word1 %in% custom_stop_words$word,
         !word2 %in% custom_stop_words$word,
         !str_detect(word1, "[0-9]"),
         !str_detect(word2, "[0-9]")) %>%
  unite(bigram, word1, word2, sep = " ") 

tidy_df_bigram %>% 
  count(bigram, sort = T) %>%
  top_n(50) %>%
  mutate(bigram = reorder(bigram,n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# sentiment analysis

tidy_df_filtered %>% 
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = T) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip()

# frequency analysis

tidy_df_tf_filtered <- tidy_df_filtered %>%
  count(doc_id, word, sort = TRUE) %>%
  bind_tf_idf(word, doc_id, n) %>%
  filter(tf_idf > quantile(tf_idf, 0.001)) %>%
  select(doc_id, word)

tidy_df_tf_filtered %>% 
  count(word, sort = T) %>%
  top_n(50) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


tidy_df_tf_bigram <- tidy_df_bigram %>%
  count(doc_id, bigram, sort = TRUE) %>%
  bind_tf_idf(bigram, doc_id, n) %>%
  filter(tf_idf > quantile(tf_idf, 0.001)) %>%
  select(doc_id, bigram)

tidy_df_tf_bigram %>% 
  count(bigram, sort = T) %>%
  top_n(50) %>%
  mutate(bigram = reorder(bigram,n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


# determine number of topics

require(ldatuning)

control_list_gibbs <- list(burnin = 2500,
                           iter = 5000,
                           seed = 0:4,
                           nstart = 5,
                           best = TRUE)

system.time(
  topic_number <- FindTopicsNumber(tidy_dtm,
                                   topics = c(seq(from = 2, to = 9, by = 1), seq(10, 20, 2), seq(25, 50, 5)),
                                   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                                   method = "Gibbs",
                                   control = control_list_gibbs,
                                   mc.cores = 2L,
                                   verbose = TRUE)
)

FindTopicsNumber_plot(topic_number)

# lda analysis

require(topicmodels)

topics <- c(9,14,18,35)

topic_lda <- LDA(tidy_dtm , k = 16 , control = list(seed = 1234))

topic_word_probabilities <- tidy(topic_lda, matrix = "beta") 

top_words_topics <- topic_word_probabilities %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_words_topics %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

doc_word_probabilities <- tidy(topic_lda, matrix = "gamma") 

doc_word_probabilities %>% 
  mutate(title = reorder(document, gamma * topic )) %>% 
  ggplot(aes(factor(topic), gamma)) + 
  geom_boxplot() + 
  facet_wrap(~ title)
