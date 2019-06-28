###########################################
#                                         #
#   Text analysis of PDF submissions to   #
#      various inquiries related to       #
#  agriculture and the digital economy    #
#             Version 20190208            #
#              (c) A Terhorst             #
#                                         #
###########################################

# ********** Compile corpora ********** #

require(tidyverse)
require(tidyr)

# Run separate scripts for different corpora. Each script
# imports public submissions, does some basic tidying (e.g. remove punctuation,
# stop words, tokenise, word stemming) and filters on key words (e.g. "agriculture", 
# "farming", "economony", "digital"). The scripts also extract email addresses from
# public submissions - used to identify network actors.

source("~/ownCloud/digiscape/r_scripts/ts_nlp.R")
source("~/ownCloud/digiscape/r_scripts/da_nlp.R")
source("~/ownCloud/digiscape/r_scripts/de_nlp.R")
source("~/ownCloud/digiscape/r_scripts/nbn_nlp.R")
source("~/ownCloud/digiscape/r_scripts/ag_nlp.R")
source("~/ownCloud/digiscape/r_scripts/dd_nlp.R")

# Combine filtered corpora to create one big corpus. 

merged_df <- bind_rows(ts_filtered,
                       da_filtered,
                       de_filtered,
                       nbn_filtered,
                       ag_filtered,
                       dd_filtered)

# Clean corpus. Remove references to "australia", clean doc_id entries.

merged_clean <- as_tibble(merged_df) %>%
  # remove strings containing "australia" 
  filter(!str_detect(word, "australia"), !str_detect(word, ".au")) %>%
  # clean doc_id entries
  mutate(doc_id = str_squish(str_replace(doc_id, " .pdf", "")), doc_id = str_squish(str_replace(doc_id, ".pdf", "")))

long_names <- merged_clean %>% 
  # test for long names
  mutate(long = ifelse(str_count(doc_id) > 10, T, F)) %>%
  # subset long names
  filter(long == T) %>%
  # compact list
  distinct(doc_id)

merged_sparkle <- merged_clean %>%
  # group sub documents together
  mutate(doc_id = case_when(doc_id == "da_sub_dr_215_01" ~ "da_sub_dr_215",
                            doc_id == "ag_sub_001_01" ~ "ag_sub_001",
                            doc_id == "ag_sub_004_01" ~ "ag_sub_004",
                            doc_id == "ag_sub_025_01" ~ "ag_sub_025",
                            doc_id == "ag_sub_025_02" ~ "ag_sub_025",
                            doc_id == "ag_sub_040_01" ~ "ag_sub_040",
                            doc_id == "ag_sub_055_01" ~ "ag_sub_055",
                            doc_id == "ag_sub_061_01" ~ "ag_sub_061",
                            doc_id == "ag_sub_085_01" ~ "ag_sub_085",
                            doc_id == "ag_sub_091_01" ~ "ag_sub_091",
                            doc_id == "dd_sub_011_02" ~ "dd_sub_011",
                            doc_id == "dd_sub_018_01" ~ "dd_sub_018",
                            TRUE ~ doc_id))

word_count <- merged_sparkle %>%
  group_by(doc_id) %>%
  count()



# Plot word frequency.

require(ggplot2)

merged_sparkle %>% 
  count(word, sort = T) %>%
  top_n(100) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
  
# Create a document term matrix from the big corpus.

require(tidytext)
require(SnowballC)

merged_dtm <- merged_sparkle %>%
  # stem words
  mutate(word = wordStem(word, language = "english")) %>%
  # count words
  count(doc_id, word) %>%
  # create document frequency matrix
  cast_dtm(doc_id, word, n)

# ********** topic modelling ********** #

# Determine the optimum number of topics (k). This takes a very long time.

# require(ldatuning)
# 
# how_many_topics <- FindTopicsNumber(merged_dtm,
#                                     topics = c(seq(2, 20,2), seq(25,50,5), seq(60,100,10)),
#                                     metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#                                     method = "Gibbs",
#                                     control = list(seed = 1234),
#                                     mc.cores = 4L,
#                                     verbose = TRUE)
# 
# # Visually results and choose k value(s) for topic modelling.
# 
# FindTopicsNumber_plot(how_many_topics)

# Do topic modelling with LDA.

require(topicmodels)

# run model.
model <- LDA(merged_dtm, method = "Gibbs", k = 5, control = list(seed = 1234, best = T))

# Display topic modelling results with 20 most frequently used words.

get_terms(model, 40)
write.csv(get_terms(model, 40), "~/ownCloud/digiscape/presentations/k5.csv", row.names = F) 

# List documents with spread of topic probabilities.

topic_map <- tidy(model, matrix = "gamma") %>%
  spread(topic, gamma)

# List documents showing top topic and potential ambiguity.

top_topic <- tidy(model, matrix = "gamma") %>%
  group_by(document) %>%
  # rank topic probabilities from most to least probable by document
  mutate(rank = rank(desc(gamma))) %>%
  arrange(document,rank) %>%
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
         check_two = max(num_2)/max(den_2)) %>%
  select(-c(den_1, den_2, num_1, num_2)) %>%
  # show top topics with ratios
  filter(rank == 1)


# Plot topic probabilities by document.
         
tidy(model, matrix = "gamma") %>% 
  mutate(title = reorder(document, gamma * topic )) %>% 
  ggplot(aes(factor(topic), gamma)) + 
  geom_boxplot() + 
  facet_wrap(~ title)

# *********** Similarity ties *********** #

merged_dfm <- merged_sparkle %>%
  # stem words
  mutate(word = wordStem(word, language = "english")) %>%
  # count word frequency
  count(doc_id, word) %>%
  # compute tf_idf
  bind_tf_idf(word, doc_id, n) %>%
  # create document frequency matrix
  cast_dfm(doc_id, word, tf_idf)

# Compute cosine similarity. 

require(quanteda)

cosine_similarity <- as.matrix(textstat_simil(merged_dfm, margin = "documents", method = "cosine"))

# threshold cosine similarity matrix

cosine_similarity[abs(cosine_similarity) < 0.5] <- 0
cosine_similarity[abs(cosine_similarity) >= 0.5] <- 1

# Generate network from cosine similarity matrix.

require(igraph)
require(tidygraph)
 
all_ties <- simplify(graph_from_adjacency_matrix(cosine_similarity, mode = "undirected")) 

similarity_ties <- as_tbl_graph(all_ties) %>%
  activate(nodes) %>%
  mutate(name = trimws(str_replace(name, ".pdf", ""))) %>%
  left_join(topic_map, by = c( "name" = "document")) %>%
  left_join(word_count, by = c("name" = "doc_id"))

attributes <- as.data.frame(similarity_ties %>%
                              activate(nodes) %>% 
                              rename(topic_1 = "1",
                                     topic_2 = "2",
                                     topic_3 = "3",
                                     topic_4 = "4",
                                     topic_5 = "5") %>%
                              mutate(inquiry = case_when(str_detect(name, "ag") ~ "ag_innovation",
                                                         str_detect(name, "de") ~ "digital_economy",
                                                         str_detect(name, "ts") ~ "trade_systems",
                                                         str_detect(name, "da") ~ "data_access",
                                                         str_detect(name, "dd") ~ "digital_delivery",
                                                         str_detect(name, "nbn") ~ "nbn_rollout"),
                                     topic_1 = round(topic_1, 2),
                                     topic_2 = round(topic_2, 2),
                                     topic_3 = round(topic_3, 2),
                                     topic_4 = round(topic_4, 2),
                                     topic_5 = round(topic_5, 2)) %>%
                              dplyr::select(-name))

write.table(attributes %>% dplyr::select(-inquiry), "~/owncloud/digiscape/mp_net/data/cont_att_A.txt", sep = "\t", row.names = F, quote = F)
write.table(attributes  %>% dplyr::select(inquiry), "~/owncloud/digiscape/mp_net/data/categ_att_A.txt", sep = "\t", row.names = F, quote = F)

# ************ Create MPNet files ********** #

# generate adjacency matrix for submissions (A)

require(MASS)

submissions_adj <- as_adjacency_matrix(similarity_ties, type = "both", names = F, sparse = F)

write.matrix(submissions_adj, "~/owncloud/digiscape/mp_net/data/submissions_net_A.txt", sep = " ")

# generate attribute files (A)

cont_att_A <- attributes %>% dplyr::select(-inquiry)

inquiry_lut <- attributes %>% dplyr::select(inquiry) %>% distinct(inquiry) %>% rownames_to_column(var = "inquiry_lut")

cate_att_A <- attributes %>% left_join(inquiry_lut) %>% dplyr::select(inquiry_lut)

write.table(cont_att_A, "~/owncloud/digiscape/mp_net/data/cont_att_A.txt", sep = "\t", row.names = F, quote = F)
write.table(cate_att_A, "~/owncloud/digiscape/mp_net/data/cate_att_A.txt", sep = "\t", row.names = F, quote = F)

# generate adjacency matrix for entities (B)

entities_adj <- matrix(0:0, nrow = 139, ncol = 139)
write.matrix(entities_adj, "~/owncloud/digiscape/mp_net/data/entities_net_B.txt", sep = " ")

# generate attribute files (B)

submissions <- read.csv("~/owncloud/digiscape/submissions/submissions_checked.csv", stringsAsFactors = F, header = T)

entities <- submissions %>%
  distinct(entity) %>%
  mutate(industry = case_when(str_detect(entity, "Department") ~ "Public Administration",
                              str_detect(entity, "Extension Network") ~ "Agriculture, Forestry and Fishing Support Services", 
                              str_detect(entity, "Government") ~ "Public Administration",
                              str_detect(entity, "Minister") ~ "Public Administration",
                              str_detect(entity, "Regional Development") ~ "Public Administration",
                              str_detect(entity, "University") ~ "Tertiary Education",
                              str_detect(entity, "Education") ~ "Tertiary Education",
                              str_detect(entity, "TAFE") ~ "Tertiary Education",
                              str_detect(entity, "Ecosystem Science Council of Australia") ~ "Personal and Other Services",
                              str_detect(entity, "Research") ~ "Professional, Scientific and Technical Services ",
                              str_detect(entity, "Standards") ~ "Professional, Scientific and Technical Services",
                              str_detect(entity, "Bureau") ~ "Professional, Scientific and Technical Services",
                              str_detect(entity, "Data Service") ~ "Professional, Scientific and Technical Services",
                              str_detect(entity, "Library") ~ "Library and Other Information Services",
                              str_detect(entity, "Resilience") ~ "Personal and Other Services",
                              str_detect(entity, "Museum") ~ "Heritage Activities",
                              str_detect(entity, "Archive") ~ "Library and Other Information Services",
                              str_detect(entity, "Grower") ~ "Personal and Other Services",
                              str_detect(entity, "Farm") ~ "Personal and Other Services",
                              str_detect(entity, "Cattle") ~ "Personal and Other Services",
                              str_detect(entity, "Sheep") ~ "Personal and Other Services",
                              str_detect(entity, "Farming") ~ "Personal and Other Services",
                              str_detect(entity, "CropLife Australia") ~ "Professional, Scientific and Technical Services",
                              str_detect(entity, "Agriculture") ~ "Personal and Other Services",
                              str_detect(entity, "Feeder") ~ "Personal and Other Services",
                              str_detect(entity, "Precision Cropping") ~ "Professional, Scientific and Technical Services",
                              str_detect(entity, "Telstra") ~ "Telecommunication Services",
                              str_detect(entity, "Nokia") ~ "Telecommunication Services",
                              str_detect(entity, "Google") ~ "Internet Service Providers, Web Search Portals and Data Processing Services",
                              str_detect(entity, "Property") ~ "Personal and Other Services",
                              str_detect(entity, "IoT") ~ "Personal and Other Services",
                              str_detect(entity, "Food Alliance") ~ "Personal and Other Services",
                              str_detect(entity, "Nutri") ~ "Food Product Manufacturing",
                              str_detect(entity, "Graziers") ~ "Personal and Other Services",
                              str_detect(entity, "Facility") ~ "Professional, Scientific and Technical Services",
                              str_detect(entity, "Dean") ~ "Personal and Other Services",
                              str_detect(entity, "Management Authority") ~ "Public Administration",
                              str_detect(entity, "Electronics") ~ "Machinery and Equipment Manufacturing",
                              str_detect(entity, "Ag Institute") ~ "Personal and Other Services",
                              str_detect(entity, "Women") ~ "Personal and Other Services",
                              str_detect(entity, "Rabobank") ~ "Finance",
                              str_detect(entity, "One Ventures") ~ "Finance",
                              str_detect(entity, "DXC.technology") ~ "Professional, Scientific and Technical Services",
                              str_detect(entity, "Export") ~ "Personal and Other Services",
                              str_detect(entity, "PriceWaterhouseCoopers") ~ "Professional, Scientific and Technical Services",
                              str_detect(entity, "Information Industry Association") ~ "Personal and Other Services",
                              str_detect(entity, "Pearcey") ~ "Computer System Design and Related Services",
                              str_detect(entity, "SST") ~ "Computer System Design and Related Services",
                              str_detect(entity, "Department") ~ "Public Administration",
                              str_detect(entity, "MYOB") ~ "Computer System Design and Related Services",
                              str_detect(entity, "Reason") ~ "Professional, Scientific and Technical Services",
                              str_detect(entity, "GES") ~ "Professional, Scientific and Technical Services",
                              str_detect(entity, "Spatial Industries") ~ "Personal and Other Services",
                              str_detect(entity, "City") ~ "Public Administration",
                              str_detect(entity, "Shire") ~ "Public Administration",
                              str_detect(entity, "CropLife") ~ "Professional, Scientific and Technical Services",
                              str_detect(entity, "Crawford") ~ "Professional, Scientific and Technical Services",
                              str_detect(entity, "Winemakers") ~ "Personal and Other Services",
                              str_detect(entity, "Pathways") ~ "Public Adminstration",
                              str_detect(entity, "AusBiotech") ~ "Personal and Other Services",
                              str_detect(entity, "Entrevator") ~ "Professional, Scientific and Technical Services",
                              str_detect(entity, "Cotton") ~ "Personal and Other Services",
                              str_detect(entity, "Collective") ~ "Personal and Other Services",
                              str_detect(entity, "Vanderfield") ~ "Motor Vehicle and Motor Vehicle Parts Retailing",
                              str_detect(entity, "Consulting") ~ "Professional, Scientific and Technical Services",
                              str_detect(entity, "Nuclear") ~ "Professional, Scientific and Technical Services",
                              str_detect(entity, "CropScience") ~ "Professional, Scientific and Technical Services",
                              str_detect(entity, "KimMic") ~ "Computer System Design and Related Services",
                              str_detect(entity, "Adobe") ~ "Computer System Design and Related Services",
                              str_detect(entity, "Investing") ~ "Finance",
                              str_detect(entity, "Academy") ~ "Personal and Other Services",
                              str_detect(entity, "Association") ~ "Personal and Other Services",
                              str_detect(entity, "Alliance") ~ "Personal and Other Services",
                              str_detect(entity, "Industry Council") ~ "Personal and Other Services",
                              str_detect(entity, "AI") ~ "Personal and Other Services",
                              str_detect(entity, "Society") ~ "Personal and Other Services",
                              str_detect(entity, "Soil Science") ~ "Personal and Other Services",
                              str_detect(entity, "Science & Technology Australia") ~ "Personal and Other Services",
                              str_detect(entity, "Phoensight") ~ "Computer System Design and Related Services",
                              str_detect(entity, "AgriDigital") ~ "Computer System Design and Related Services",
                              str_detect(entity, "AlphaBeta") ~ "Professional, Scientific and Technical Services",
                              str_detect(entity, "Infoxchange") ~ "Computer System Design and Related Services",
                              str_detect(entity, "Standing Committee") ~ "Public Administration",
                              str_detect(entity, "Pearson") ~ "Publishing",
                              TRUE ~ "Individual"),
         entity = str_replace_all(tolower(trimws(entity)), " ", "_"),
         industry = str_replace_all(tolower(trimws(industry)), " ", "_"))

industry_lut <- entities %>%
  distinct(industry) %>%
  rownames_to_column(var = "industry_lut")

entities <- entities %>%
  inner_join(industry_lut)

cate_att_B <- entities %>% dplyr::select(industry_lut)

write.table(cate_att_B, "~/owncloud/digiscape/mp_net/data/cate_att_B.txt", sep = "\t", row.names = F, quote = F)

# generate adjacency matrix for bipartite network (X)

submissions_consolidated <- merged_sparkle %>%
  distinct(doc_id) %>%
  full_join()

bipartite_edges <- read.csv("~/owncloud/digiscape/submissions/submissions_checked.csv") %>% 
  mutate(entity = str_replace_all(tolower(entity), " ", "_"))

bipartite_graph <- graph_from_data_frame(bipartite_edges, directed = F) # igraph

bipartite_tidygraph <- as_tbl_graph(bipartite_graph) # convert into tidygraph

bp_adj <- get.adjacency(bipartite_tidygraph, sparse = F, names = F)[1:165,166:304]

write.matrix(bp_adj, "~/owncloud/digiscape/mp_net/data/bipartite_net_X.txt", sep = " ")


# generate attribute files (X)

cont_att_b <- data.frame()
cont_att_b[1:139,1] <- 0
colnames(cont_att_b) <- "dummy"

x_cont <- bind_rows(cont_att_A, cont_att_b) %>% mutate_all(funs(replace_na(.,0)))
x_cate <- bind_rows(cate_att_A, cate_att_B) %>% mutate_all(funs(replace_na(.,0)))

write.table(x_cont, "~/owncloud/digiscape/mp_net/data/cont_att_X.txt", sep = "\t", row.names = F, quote = F)
write.table(x_cate, "~/owncloud/digiscape/mp_net/data/cate_att_X.txt", sep = "\t", row.names = F, quote = F)

# *********** Generate network diagrams ********** #

require(ggraph)

# Plot bipartite network

ggraph(bipartite_tidygraph, layout = "nicely") +
  geom_edge_link() +
  geom_node_point(aes()) +
  theme_graph()




# Plot similarity network.



ggraph(similarity_ties, layout = "circle") +
  geom_edge_link() +
  geom_node_point() +
  theme_graph()

# Plot degree distribution.

dd <- degree_distribution(similarity_ties)
dd_df <- data.frame(deg = 0:(length(dd)-1), prop = dd)

ggplot(dd_df[-1, ], aes(deg, prop)) + 
  geom_point(shape = 1) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw() +
  ggtitle('Degree Distribution (log-log scale)')+
  xlab('Degree') +
  ylab('Frequency')


# ********** Compile email addresses *********** #

merged_email <- bind_rows(ts_email,
                          da_email,
                          de_email,
                          nbn_email,
                          ag_email,
                          dd_email)

email_blacklist <- data.frame(email = c("jsctig@aph.gov.au",
                                        "data.access@pc.gov.au",
                                        "data.access@pc.govt.au",
                                        ".access@pc.gov.au",
                                        "agind.reps@aph.gov.au",
                                        "aglnd.reps@aph.gov.au",
                                        "digitaleconomy@industry.gov.au",
                                        "digitaleconomy@industry.com.au",
                                        "digitaleconomy@industry.gov.au.",
                                        "digitaieconomy@industrv.gov.au",
                                        "fpa.sen@aph.gov.au",
                                        "fl@world",
                                        "aglnd.rcps@aph.gov.au",
                                        "www.abs.gov.au/ausstats/abs@.nsf/mf/8146.0"),
                              stringsAsFactors = F)

clean_email <- merged_email %>%
  anti_join(email_blacklist)%>%
  mutate(email = str_replace_all(email, "\\<|\\>|\\email:|\\e-mail:|\\(|\\)", "")) 

edges <- submissions %>%
  filter(entity != "Department of Industry, Innovation, and Science" | forum != "Digital Economy Strategy") %>%
  select(entity, doc_id, forum) %>%
  mutate(doc_id = trimws(doc_id), doc_id = str_replace_all(doc_id, "\\.pdf|\\_attach1|\\_attach2|\\_attachment|\\.1|\\_attach|\\_att1", "")) %>%
  distinct(entity,doc_id,forum)

m <- as.matrix(edges)

g <- simplify(graph_from_edgelist(rbind(m[,1:2], m[,2:3]), directed = F))

l <- layout_with_sugiyama(g, ceiling(match(V(g)$name, m)/nrow(m)))


plot(g, 
     layout = l$layout[,2:1],
     vertex.label = NA,
     vertex.size = 2)



edges <- submissions %>%
  select(entity, doc_id) %>%
  rename(alpha = entity, beta = doc_id)

nodes <- submissions %>%
  select(entity, forum) %>%
  rename(alpha = entity, beta = forum) %>%
  bind_rows(edges) %>%
  gather(key, label) %>%
  arrange(key) %>%
  distinct(label) %>%
  rowid_to_column("id")

edges <- submissions %>%
  select(entity, forum) %>%
  rename(alpha = entity, beta = forum) %>%
  bind_rows(edges) %>%
  left_join(nodes, by = c("alpha" = "label")) %>%
  rename(from = id)

edges <- edges %>%
  left_join(nodes, by = c("beta" = "label")) %>%
  rename(to = id) %>%
  select(from, to)
  
  


  

edges <- submissions %>%
  left_join(nodes, by = c("entity" = "label")) %>%
  rename(from = id) 
  
  

  # group by forum and enity
  group_by(forum, entity) %>%
  # count number of documents tendered
  count()

# Create node and edge lists

nodes <- clean_submissions %>%
  gather(key, value) %>%
  arrange(key) %>%
  distinct(value) %>%
  rename(label = value) %>%
  filter(!str_detect(label,"[0-9]")) %>%
  rowid_to_column("id")
  
edges <- clean_submissions %>%
  left_join(nodes, by = c("entity" = "label")) %>%
  rename(from = id) 

edges <- edges %>%
  left_join(nodes, by = c("forum" = "label")) %>%
  rename(to = id)

edges <- edges %>%
  select(from, to, n) %>%
  rename(weight = n)

similarity_from <- as.data.frame(as_edgelist(similarity_ties), stringsAsFactors = F) %>%
  select(V1) %>%
  inner_join(submissions, by = c("V1" = "doc_id")) %>%
  select(entity)

similarity_to <- as.data.frame(as_edgelist(similarity_ties), stringsAsFactors = F) %>%
  select(V2) %>%
  inner_join(submissions, by = c("V2" = "doc_id")) %>%
  select(entity)

similarity_edge <- bind_cols(similarity_from, similarity_to) %>%
  left_join(nodes, by = c("entity" = "label")) %>%
  rename(from = id) 

similarity_edge <- similarity_edge %>%
  left_join(nodes, by = c("entity1" = "label")) %>%
  rename(to = id)
