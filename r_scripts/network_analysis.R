#################################################
#                                               #
#           Digiscape Similarity Ties           #
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

require(tidytext)
require(tidyr)


tic("tidy corpus")
tidy_corpus <- clean_corpus %>%
  # tokenise
  unnest_tokens(word, text) %>%
  # get rid of stop words
  anti_join(get_stopwords()) %>%
  # get rid of underscores
  mutate(word = str_replace_all(word, "_", ""),
         word = ifelse(word == "", NA, word)) %>%
  drop_na() %>%
  # get rid of numbers
  filter(!str_detect(word, "[0-9]+"))
toc()  


# Compute cosine similarity. 

require(SnowballC)

corpus_dfm <- tidy_corpus %>%
  # stem words
  mutate(word = wordStem(word, language = "english")) %>%
  # count word frequency
  count(doc_id, word) %>%
  # compute tf_idf
  bind_tf_idf(word, doc_id, n) %>%
  # create document frequency matrix
  cast_dfm(doc_id, word, tf_idf)

require(quanteda)

cosine_similarity <- as.matrix(textstat_simil(corpus_dfm, margin = "documents", method = "cosine"))

# threshold cosine similarity matrix

cosine_similarity[abs(cosine_similarity) < 0.5] <- 0
cosine_similarity[abs(cosine_similarity) >= 0.5] <- 1

# generate similarity network from cosine similarity matrix.

require(igraph)
require(tidygraph)

all_ties <- simplify(graph_from_adjacency_matrix(cosine_similarity, mode = "undirected")) 
similarity_ties <- as_tbl_graph(all_ties) 


# plot similarity network

# compute label angles

require(pracma)

lo <- layout.circle(similarity_ties)
angle <- as_tibble(cart2pol(lo)) %>% mutate(degree = phi * 180/pi)

# generate plot

require(ggraph)

ggraph(similarity_ties, layout = "circle") +
  geom_edge_link() +
  geom_node_point(colour = "light blue", size = 3) +
  geom_node_text(aes(label = name), 
                 size = 3, 
                 hjust = ifelse(lo[,1] > 0, -0.2, 1.2),
                 angle = case_when(lo[,2] > 0 & lo[,1] > 0 ~ angle$degree, 
                                   lo[,2] < 0 & lo[,1] > 0 ~ angle$degree,
                                   lo[,1] == 1 ~angle$degree,
                                  TRUE ~ angle$degree - 180)) +
  coord_cartesian(xlim=c(-1.25,1.25), ylim=c(-1.25,1.25))

