getwd()

library(igraph) 

# Load necessary packages
library(pacman)
p_load(tidyverse, jsonlite, igraph, dplyr, lubridate, tidygraph, ggraph)

# Load JSON data
json_data <- fromJSON("data/MC3/mc3.json")

# Extract nodes and edges
nodes <- as.data.frame(json_data$nodes)
edges <- as.data.frame(json_data$links)

# Ensure 'id' columns and edge 'source'/'target' columns are character type
nodes$id <- as.character(nodes$id)
edges$source <- as.character(edges$source)
edges$target <- as.character(edges$target)

# Remove any rows with NA values in 'source' or 'target'
edges <- edges %>%
  filter(!is.na(source) & !is.na(target))

# Filter the nodes and edges related to SouthSeafood Express Corp
southseafood_node <- nodes %>%
  filter(grepl("SouthSeafood Express Corp", id))

if (nrow(southseafood_node) > 0) {
  southseafood_id <- southseafood_node$id
  
  related_edges <- edges %>%
    filter(source == southseafood_id | target == southseafood_id)
  
  related_nodes_ids <- unique(c(related_edges$source, related_edges$target))
  related_nodes <- nodes %>%
    filter(id %in% related_nodes_ids)
  
  # Ensure unique vertex names by using the 'id' field
  related_nodes <- related_nodes %>%
    distinct(id, .keep_all = TRUE)
  
  # Select a feature for clustering (e.g., revenue if available)
  if ("revenue" %in% names(related_nodes)) {
    node_features <- related_nodes %>%
      select(revenue) %>%
      na.omit()  # Remove rows with NA values
    
    # Perform hierarchical clustering
    dendrogram <- hclust(dist(node_features))
    
    # Convert hclust object to a dendrogram
    dendro_data <- as.dendrogram(dendrogram)
    
    # Plot the dendrogram using ggraph
    ggraph(dendro_data, layout = 'dendrogram') +
      geom_edge_elbow() +
      geom_node_text(aes(label = label), vjust = -0.5, hjust = 1, angle = 90) +
      theme_void() +
      ggtitle("Dendrogram of Nodes Related to SouthSeafood Express Corp")
  } else {
    print("The 'revenue' column is not available in the nodes data.")
  }
} else {
  print("SouthSeafood Express Corp not found in the dataset")
}