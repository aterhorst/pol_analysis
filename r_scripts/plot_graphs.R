#################################################
#                                               #
#       R script for generating bipartite       #
#     networks for policy network analysis      #
#               Version 2018-09-25              #
#              (c) Andrew Terhorst              #
#                                               #
#################################################


require(tidyverse)
require(tidygraph)

# read in pre-prepared submission list

submission_list <- read.csv("~/owncloud/digiscape/submissions/submissions_list.csv", stringsAsFactors = F, header = T)

# tidy up submission list


clean_submission_list <- submission_list %>% 
  select(-c(organisation, email)) %>%
  distinct(submission, organisation_edited, forum) %>%
  select(-submission) %>%
  arrange(organisation_edited, forum) %>%
  group_by(forum, organisation_edited) %>%
  count() %>%
  ungroup() %>%
  select(organisation_edited, forum, n) 

# create node and edge lists

nodes <- clean_submission_list %>%
  gather(key, value) %>%
  arrange(key) %>%
  distinct(value) %>%
  rename(label = value) %>%
  filter(!str_detect(label,"[0-9]")) %>%
  rowid_to_column("id")


edges <- clean_submission_list %>%
  left_join(nodes, by = c("organisation_edited" = "label")) %>%
  rename(from = id) 


edges <- edges %>%
  left_join(nodes, by = c("forum" = "label")) %>%
  rename(to = id)


edges <- edges %>%
  select(from, to, n) %>%
  rename(weight = n)

# generate network

require(igraph)
require(tidygraph)
require(ggraph)

g <- tbl_graph(edges = edges, nodes = nodes, directed = F)

g <- g %>% 
  activate(nodes) %>%
  mutate(type = bipartite_mapping(g)$type)

# plot network

p <- ggraph(g, layout = "bipartite") +
  geom_edge_link(aes(edge_width = weight), alpha = 0.5, edge_colour = "grey") +
  geom_node_point(aes(shape = type, color = type), size = if_else(V(g)$type == "TRUE", 2, 4)) +
  geom_node_text(aes(label = label),
                 size = 2.5,
                 nudge_y = if_else(V(g)$type == "TRUE", -0.025, 0.025), 
                 hjust = if_else(V(g)$type == "TRUE", 1, 0)) +
  ylim(-0.5, 1.5) +
  coord_flip() +
  theme_void() 

# save plot as PDF

ggsave("~/owncloud/digiscape/submissions/bipartite_network.pdf", p, "pdf", width = 14, height = 14, units = "in")
