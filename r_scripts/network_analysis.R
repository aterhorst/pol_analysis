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
  # get rid of numbers
  filter(!str_detect(word, "[0-9]+"), word %in% dictionary$word) %>%
  # get rid of stop words
  anti_join(get_stopwords()) %>%
  anti_join(data.frame(word = letters, stringsAsFactors = F)) %>%
  anti_join(data.frame(word = tolower(month.name), stringsAsFactors = F)) 
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

topic_rank <- read.csv("~/owncloud/pol_analysis/topic mapping/topic_rank.csv", stringsAsFactors = F)

all_ties <- simplify(graph_from_adjacency_matrix(cosine_similarity, mode = "undirected")) 
similarity_ties <- as_tbl_graph(all_ties) %>%
  activate(nodes) %>%
  inner_join(topic_rank, by = c("name" = "doc_id"))



# plot similarity network

# compute label angles

require(pracma)

lo <- layout.circle(similarity_ties)
angle <- as_tibble(cart2pol(lo)) %>% mutate(degree = phi * 180/pi)

# generate plot

require(ggraph)
require(Cairo)

net <- ggraph(similarity_ties, layout = "circle") +
  geom_edge_link() +
  geom_node_point(aes(colour = factor(topic), alpha = gamma), size = 4) +
  geom_node_text(aes(label = name), 
                 size = 3, 
                 hjust = ifelse(lo[,1] > 0, -0.2, 1.2),
                 angle = case_when(lo[,2] > 0 & lo[,1] > 0 ~ angle$degree, 
                                   lo[,2] < 0 & lo[,1] > 0 ~ angle$degree,
                                   lo[,1] == 1 ~angle$degree,
                                   TRUE ~ angle$degree - 180)) +
  labs(colour = "Top topic") + 
  labs(alpha = expression(paste(gamma, " value"))) +
  #labs(alpha = "\u03B3 value") +
  guides(color = guide_legend(order = 1),
         alpha = guide_legend(order = 2)) +
  theme(panel.background = element_blank(),
        legend.background=element_blank(), 
        legend.key = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_cartesian(xlim=c(-1.275, 1.45), ylim=c(-1.275, 1.275))

ggsave(plot = net, "~/owncloud/digiscape/presentations/network_analysis.png", device = "png", type = "cairo", dpi = 300, height = 9.37, width = 11.2, units = "in")
