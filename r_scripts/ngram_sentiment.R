#################################################
#                                               #
#               Digiscape n-grams               #
#               Version 20190710                #
#                Andrew Terhorst                #
#                                               #
#################################################

# read submissions

require(tictoc)
require(tidyverse)
require(readtext)

data_dir <- "~/owncloud/pol_analysis/submissions"

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
                   TRUE ~ str_detect(text, "agri")))
toc()

# reference inquiries

inquiry_id <- clean_corpus %>%
  mutate(inquiry = case_when(str_detect(doc_id, "ag_") ~ "agricultural innovation",
                             str_detect(doc_id, "da_") ~ "data access",
                             str_detect(doc_id, "de_") ~ "digital economy strategy",
                             str_detect(doc_id, "dd_") ~ "digital delivery",
                             str_detect(doc_id, "nbn_") ~ "nbn roll-out",
                             str_detect(doc_id, "ts_") ~ "trade systems")) %>%
  distinct(doc_id, inquiry)

# create custom stop words

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
                               stringsAsFactors = F)

ag_stopwords <- data.frame(word = c("agricultural",
                                    "innovation",
                                    "journal"),
                           stringsAsFactors = F)

da_stopwords <- data.frame(word = c("data",
                                    "availability",
                                    "use",
                                    "act",
                                    "cth"),
                           stringsAsFactors = F)

de_stopwords <- data.frame(word = c("digital",
                                    "economy",
                                    "strategy",
                                    "consultation"),
                           stringsAsFactors = F)

nbn_stopwords <- data.frame(word = c("nbn",
                                     "regional",
                                     "rural",
                                     "areas",
                                     "rollout",
                                     "co"),
                             stringsAsFactors = F)

ts_stopwords <- data.frame(word = c("trade",
                                    "systems",
                                    "digital",
                                    "economy",
                                    "tbt",
                                    "iec"),
                           stringsAsFactors = F)


# tokenise

require(tidytext)

unigrams <- clean_corpus %>% 
  # tokenise unigrams
  unnest_tokens(word, text) %>%
  # get rid of numbers
  filter(!str_detect(word, "[0-9]+")) %>%
  # ditch superfluous words
  anti_join(get_stopwords()) %>%
  anti_join(custom_stopwords) %>%
  inner_join(inquiry_id)

bigrams <- clean_corpus %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!str_detect(bigram, "[0-9]+")) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word,
         !word1 %in% custom_stopwords$word, 
         !word2 %in% custom_stopwords$word) %>%
  inner_join(inquiry_id)

# plot top 20 unigrams
  
require(gridExtra)
require(ggpubr)

col <- get_palette(palette = "default", 5)

uni_ag <- unigrams %>%
  filter(inquiry == "agricultural innovation") %>%
  anti_join(ag_stopwords) %>%
  count(word, sort = T) %>%
  top_n(20) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = col[1]) +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Agricultural Innovation") +
  theme(plot.title = element_text(hjust = 0.5))

uni_da <- unigrams %>%
  filter(inquiry == "data access") %>%
  anti_join(da_stopwords) %>%
  count(word, sort = T) %>%
  top_n(20) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = col[2]) +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Data Availability & Use") +
  theme(plot.title = element_text(hjust = 0.5))

uni_de <- unigrams %>%
  filter(inquiry == "digital economy strategy") %>%
  anti_join(de_stopwords) %>%
  count(word, sort = T) %>%
  top_n(20) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = col[3]) + 
  xlab(NULL) +
  coord_flip() +
  ggtitle("Digital Economy Strategy") +
  theme(plot.title = element_text(hjust = 0.5))

uni_nbn <- unigrams %>%
  filter(inquiry == "nbn roll-out") %>%
  anti_join(nbn_stopwords) %>%
  count(word, sort = T) %>%
  top_n(20) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = col[4]) +
  xlab(NULL) +
  coord_flip() +
  ggtitle("NBN Rollout") +
  theme(plot.title = element_text(hjust = 0.5))

uni_ts <- unigrams %>%
  filter(inquiry == "trade systems") %>%
  anti_join(ts_stopwords) %>%
  count(word, sort = T) %>%
  top_n(20) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = col[5]) +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Trade Systems and the Digital Economy") +
  theme(plot.title = element_text(hjust = 0.5))

uni_all <- unigrams %>%
  count(word, sort = T) %>%
  top_n(20) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = col[5]) +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Complete corpus subset") +
  theme(plot.title = element_text(hjust = 0.5))

u <- arrangeGrob(uni_ag, uni_da, uni_de, uni_nbn, uni_ts, uni_all, nrow = 2)

ggsave("~/owncloud/digiscape/presentations/top20unigrams.pdf", u)

# plot top 10 bigrams

bi_ag <- bigrams %>%
  filter(inquiry == "agricultural innovation",
         !word1 %in% ag_stopwords$word, 
         !word2 %in% ag_stopwords$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = T) %>%
  top_n(10) %>%
  mutate(bigram = reorder(bigram,n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col(fill = col[1]) +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Agricultural Innovation") +
  theme(plot.title = element_text(hjust = 0.5))

bi_da <- bigrams %>%
  filter(inquiry == "data access",
         !word1 %in% da_stopwords$word, 
         !word2 %in% da_stopwords$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = T) %>%
  top_n(10) %>%
  mutate(bigram = reorder(bigram,n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col(fill = col[2]) +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Data Availability & Use") +
  theme(plot.title = element_text(hjust = 0.5))

bi_de <- bigrams %>%
  filter(inquiry == "digital economy strategy",
         !word1 %in% de_stopwords$word, 
         !word2 %in% de_stopwords$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = T) %>%
  top_n(10) %>%
  mutate(bigram = reorder(bigram,n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col(fill = col[3]) +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Digital Economy Strategy") +
  theme(plot.title = element_text(hjust = 0.5))

bi_nbn <- bigrams %>%
  filter(inquiry == "nbn roll-out",
         !word1 %in% nbn_stopwords$word, 
         !word2 %in% nbn_stopwords$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = T) %>%
  top_n(10) %>%
  mutate(bigram = reorder(bigram,n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col(fill = col[4]) +
  xlab(NULL) +
  coord_flip() +
  ggtitle("NBN Rollout") +
  theme(plot.title = element_text(hjust = 0.5))

bi_ts <- bigrams %>%
  filter(inquiry == "trade systems",
         !word1 %in% ts_stopwords$word, 
         !word2 %in% ts_stopwords$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = T) %>%
  top_n(10) %>%
  mutate(bigram = reorder(bigram,n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col(fill = col[5]) +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Trade Systems and the Digital Economy") +
  theme(plot.title = element_text(hjust = 0.5))

bi_all <- bigrams %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = T) %>%
  top_n(10) %>%
  mutate(bigram = reorder(bigram,n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col(fill = col[5]) +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Complete corpus subset") +
  theme(plot.title = element_text(hjust = 0.5))

b <- arrangeGrob(bi_ag, bi_da, bi_de, bi_nbn, bi_ts, bi_all, nrow = 2)

ggsave("~/owncloud/digiscape/presentations/top20bigrams.pdf", b)

# plot top 10 positive and negative sentiments

sent_ag <- unigrams %>%
  filter(inquiry == "agricultural innovation") %>%
  anti_join(ag_stopwords) %>%
  inner_join(get_sentiments("bing") %>% filter(word != "cloud",word != "drones" )) %>%
  count(word, sentiment, sort = T) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip() +
  xlab(NULL) +
  ggtitle("Agricultural Innovation") +
  theme(plot.title = element_text(hjust = 0.5))
  
sent_da <- unigrams %>%
  filter(inquiry == "data access") %>%
  anti_join(da_stopwords) %>%
  inner_join(get_sentiments("bing") %>% filter(word != "cloud")) %>%
  count(word, sentiment, sort = T) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip() +
  xlab(NULL) +
  ggtitle("Data Availability & Use") +
  theme(plot.title = element_text(hjust = 0.5))

sent_de <- unigrams %>%
  filter(inquiry == "digital economy strategy") %>%
  anti_join(de_stopwords) %>%
  inner_join(get_sentiments("bing") %>% filter(word != "cloud")) %>%
  count(word, sentiment, sort = T) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip() +
  xlab(NULL) +
  ggtitle("Digital Economy Strategy") +
  theme(plot.title = element_text(hjust = 0.5))

sent_nbn <- unigrams %>%
  filter(inquiry == "nbn roll-out") %>%
  anti_join(nbn_stopwords) %>%
  inner_join(get_sentiments("bing") %>% filter(word != "cloud")) %>%
  count(word, sentiment, sort = T) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip() +
  xlab(NULL) +
  ggtitle("NBN Rollout") +
  theme(plot.title = element_text(hjust = 0.5))

sent_ts <- unigrams %>%
  filter(inquiry == "trade systems") %>%
  anti_join(ts_stopwords) %>%
  inner_join(get_sentiments("bing") %>% filter(word != "cloud")) %>%
  count(word, sentiment, sort = T) %>%
  group_by(sentiment) %>%
  #top_n(10) %>%
  #slice(1:5) %>%
  do(head(., n = 10)) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip() +
  xlab(NULL) +
  ggtitle("Trade Systems and the Digital Economy") +
  theme(plot.title = element_text(hjust = 0.5))

sent_all <- unigrams %>%
  inner_join(get_sentiments("bing") %>% filter(word != "cloud")) %>%
  count(word, sentiment, sort = T) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip() +
  xlab(NULL) +
  ggtitle("Trade Systems and the Digital Economy") +
  theme(plot.title = element_text(hjust = 0.5))

s <- arrangeGrob(sent_ag, sent_da, sent_de, sent_nbn, sent_ts, sent_all, nrow = 2)

ggsave("~/owncloud/digiscape/presentations/top20sentiments.pdf", s)


