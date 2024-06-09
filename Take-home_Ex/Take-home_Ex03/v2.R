# Load necessary packages
library(pacman)
p_load(tidyverse, jsonlite, igraph, dplyr, lubridate, tidygraph, ggraph)

# Load JSON data
json_data <- fromJSON("data/MC3/mc3.json")

# Extract nodes and edges
nodes <- as.data.frame(json_data$nodes)
edges <- as.data.frame(json_data$links)

# Convert date fields
nodes$founding_date <- as.Date(nodes$founding_date, format = "%Y-%m-%dT%H:%M:%S")
edges$start_date <- as.Date(edges$start_date, format = "%Y-%m-%dT%H:%M:%S")

# Identify SouthSeafood Express Corp
southseafood_id <- nodes %>% filter(name == "SouthSeafood Express Corp") %>% pull(id)

# Extract subnetwork
connected_nodes <- unique(c(
  edges %>% filter(from == southseafood_id) %>% pull(to),
  edges %>% filter(to == southseafood_id) %>% pull(from),
  southseafood_id
))

nodes_subgraph <- nodes %>% filter(id %in% connected_nodes)
edges_subgraph <- edges %>% filter(from %in% connected_nodes & to %in% connected_nodes)

# Create subgraph
subgraph <- tbl_graph(nodes = nodes_subgraph, edges = edges_subgraph, directed = TRUE, node_key = "id")

# Community Detection
walktrap_communities <- cluster_walktrap(as.igraph(subgraph))

# Add community membership to nodes
nodes_subgraph$community <- as.factor(membership(walktrap_communities))

# Update subgraph
subgraph <- tbl_graph(nodes = nodes_subgraph, edges = edges_subgraph, directed = TRUE, node_key = "id")

# Plot the subgraph with communities
ggraph(subgraph, layout = 'kk') + 
  geom_edge_link(aes(colour = factor(Weight)), alpha = 0.5) + 
  geom_node_point(aes(colour = community), size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) + 
  theme_minimal() + 
  ggtitle("Network of SouthSeafood Express Corp with Communities")

# Temporal Analysis and Centrality Measures
# Assuming `edges` has a 'date' column to perform temporal analysis
pre_legal_troubles <- edges %>% filter(start_date < as.Date("YYYY-MM-DD"))  # Replace with actual date
post_legal_troubles <- edges %>% filter(start_date >= as.Date("YYYY-MM-DD")) # Replace with actual date

# Create pre and post subgraphs
subgraph_pre <- tbl_graph(nodes = nodes, edges = pre_legal_troubles, directed = TRUE, node_key = "id")
subgraph_post <- tbl_graph(nodes = nodes, edges = post_legal_troubles, directed = TRUE, node_key = "id")

# Calculate centrality
pre_centrality <- as.data.frame(centrality_degree(as.igraph(subgraph_pre)))
post_centrality <- as.data.frame(centrality_degree(as.igraph(subgraph_post)))

# Identify benefiting companies
benefited_companies <- post_centrality %>% 
  filter(degree > pre_centrality$degree) %>% 
  select(id, name, degree)

# Visualize the changes
p_pre <- ggraph(subgraph_pre, layout = 'kk') + 
  geom_edge_link(aes(colour = factor(Weight)), alpha = 0.5) + 
  geom_node_point(aes(colour = degree), size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) + 
  theme_minimal() + 
  ggtitle("Network Before Legal Troubles")

p_post <- ggraph(subgraph_post, layout = 'kk') + 
  geom_edge_link(aes(colour = factor(Weight)), alpha = 0.5) + 
  geom_node_point(aes(colour = degree), size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) + 
  theme_minimal() + 3
  ggtitle("Network After Legal Troubles")

# Display the plots
library(gridExtra)
grid.arrange(p_pre, p_post, ncol = 1)

# Anomaly Detection
# Assuming `edges` has transaction details that can be analyzed for anomalies
# This part of the analysis would require more details on the structure of the dataset
