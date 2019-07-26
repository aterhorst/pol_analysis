#################################################
#                                               #
#          Digiscape Topic Modelling            #
#               Version 20190710                #
#                Andrew Terhorst                #
#                                               #
#################################################

# read submissions

require(tictoc)
require(tidyverse)
require(readtext)

data_dir <- "/OSM/MEL/DPS_OI_Network/work/ownCloud/pol_analysis/submissions"

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
         text = replace_contraction(text), # replace contractions with long form
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

# total number of documents in corpus

nrow(clean_corpus %>% distinct(doc_id))

doc_counts <- clean_corpus %>%
  mutate(inquiry = case_when(str_detect(doc_id, "ag_") ~ "agricultural innovation",
                             str_detect(doc_id, "da_") ~ "data access",
                             str_detect(doc_id, "de_") ~ "digital economy strategy",
                             str_detect(doc_id, "dd_") ~ "digital delivery",
                             str_detect(doc_id, "nbn_") ~ "nbn roll-out",
                             str_detect(doc_id, "ts_") ~ "trade systems")) %>%
  group_by(inquiry) %>%
  count()


# create tidy corpus

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

require(tidytext)
require(tidyr)
require(qdapDictionaries)

comp_terms <- read.csv("https://raw.githubusercontent.com/aterhorst/pol_analysis/master/dictionaries/comp_terms_wikipedia.csv", stringsAsFactors = F)

dictionary <- as.data.frame(GradyAugmented, stringsAsFactors = F) %>% 
  select(word = GradyAugmented, everything()) %>%
  bind_rows(comp_terms) %>%
  distinct(word)


tic("tidy corpus")
tidy_corpus <- clean_corpus %>%
  # tokenise
  unnest_tokens(word, text) %>%
  # get rid of numbers and nonsensical words
  filter(!str_detect(word, "[0-9]+"), word %in% dictionary$word) %>%
  # get rid of stop words
  anti_join(get_stopwords()) %>%
  anti_join(custom_stopwords)
toc()  

# create a document-term matrix

tic("generate dtm")
corpus_dtm <- tidy_corpus %>%
  count(doc_id, word) %>%
  cast_dtm(doc_id, word, n)
toc()

# determine k for topic modelling

require(ldatuning)

tic("ldatuning")
how_many_topics <- FindTopicsNumber(corpus_dtm,
                                    topics = c(seq(2, 20, 2), seq(25, 50, 5), seq(60, 100, 10)),
                                    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                                    method = "Gibbs",
                                    control = list(seed = 1234),
                                    mc.cores = 8L,
                                    verbose = F)
toc()

FindTopicsNumber_plot(how_many_topics)

# run topic model

require(topicmodels)

tic("lda")
model <- LDA(corpus_dtm, method = "Gibbs", k = 18, control = list(seed = 1234, best = T))
toc()

# explore topic model (beta, gamma values)

corpus_beta <- tidy(model, matrix = "beta")
corpus_gamma <- tidy(model, matrix = "gamma")


# visualise topics

require(ggthemes)

top_terms <- corpus_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

gamma_terms <- corpus_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms %>%
  top_n(10, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.005, size = 3.5) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.55),
                     labels = percent_format()) +
  labs(x = NULL, y = expression(gamma)) +
  theme(text = element_text(size = 14))

ggsave("/OSM/MEL/DPS_OI_Network/work/ownCloud/digiscape/presentations/top10topics.pdf")

# list documents with spread of topic probabilities.

topic_map <- tidy(model, matrix = "gamma") %>%
  spread(topic, gamma)

# list documents showing top topic and potential ambiguity.

top_topic <- as_tibble(tidy(model, matrix = "gamma") %>%
  group_by(document) %>%
  # rank topic probabilities from most to least probable by document
  mutate(rank = rank(desc(gamma))) %>%
  arrange(document, rank) %>%
  # compute ratios: ((top) / (top - 1)) and ((top - 1) / (top - 2))
  mutate(num_1 = case_when(rank == 1 ~ gamma,
                           TRUE ~ 0),
         den_1 = case_when(rank == 2 ~ gamma,
                           TRUE ~0),
         check_one = max(num_1)/max(den_1),
         num_2 = case_when(rank == 2 ~ gamma,
                           TRUE ~ 0),
         den_2 = case_when(rank == 3 ~ gamma,
                           TRUE ~ 0),
         check_two = max(num_2)/max(den_2),
         doc_id = document) %>%
    ungroup() %>%
  select(-c(den_1, den_2, num_1, num_2, document)) %>%
  # show top topics with ratios
  filter(rank == 1))

write.csv(top_topic, file = "/OSM/MEL/DPS_OI_Network/work/ownCloud/pol_analysis/topic mapping/topic_rank.csv", row.names = F)
