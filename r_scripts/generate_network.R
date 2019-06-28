


# Read in pre-processed data

submissions <- read.csv("~/owncloud/digiscape/submissions/submissions_list.csv", stringsAsFactors = F, header = T)

edges <- submissions %>%
  # remove self-references
  filter(entity != "Department of Industry, Innovation, and Science" | forum != "Digital Economy Strategy") %>%
  select(entity, doc_id, forum) %>%
  # clean doc_id entries
  mutate(doc_id = trimws(doc_id), doc_id = str_replace_all(doc_id, "\\.pdf|\\_attach1|\\_attach2|\\_attachment|\\.1|\\_attach|\\_att1", "")) %>%
  # remove duplicate entries
  distinct(entity,doc_id,forum)

# Generate graph
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








new_master <- master %>%
  inner_join(submission_list)

entities <- top %>%
  left_join(org_list)


submission_edges <- merged_clean %>%
  distinct(doc_id) %>%
  mutate(to = case_when(str_detect(doc_id, "ag") ~ "agricultural_innovation",
                        str_detect(doc_id, "ts") ~ "trade_systems_and_the_digital_economy",
                        str_detect(doc_id, "de") ~ "digital_economy_strategy",
                        str_detect(doc_id, "nbn") ~ "nbn_rollout_rural",
                        str_detect(doc_id, "dd") ~ "digital_delivery_gov_services",
                        str_detect(doc_id, "da") ~ "data_access")) %>%
  rename(from = "doc_id")

similarity_edges <- as.data.frame(as_edgelist(similarity_ties), stringsAsFactors = F) %>%
  rename(from = "V1", to = "V2") 

