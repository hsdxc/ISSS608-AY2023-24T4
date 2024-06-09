# Load the necessary libraries
library(ggraph)
library(dendextend)
library(tidygraph)
library(jsonlite)
library(dplyr)
library(grid)

# Load JSON data
file_path <- 'data/MC3/mc3.json'
json_data <- fromJSON(file_path)

# Extract the nodes and edges
nodes <- json_data$nodes
edges <- json_data$links

# Ensure 'id' columns and edge 'source'/'target' columns are character type
nodes$id <- as.character(nodes$id)
edges$source <- as.character(edges$source)
edges$target <- as.character(edges$target)

# Ensure edges are not NULL
if (is.null(edges)) {
  edges <- data.frame()
}

# Check for NA or empty values in the node IDs or edge source/target
if (any(is.na(nodes$id) | nodes$id == "")) {
  stop("NA or empty values found in node IDs.")
}
if (any(is.na(edges$source) | edges$source == "" | is.na(edges$target) | edges$target == "")) {
  stop("NA or empty values found in edge source or target.")
}

# Remove rows with NA or empty values
nodes <- nodes %>% filter(!is.na(id) & id != "")
edges <- edges %>% filter(!is.na(source) & source != "" & !is.na(target) & target != "")

# Convert edges to a list format for easy traversal
edge_list <- split(edges[, c("target", "source")], seq(nrow(edges)))

# Create an adjacency list
adj_list <- list()
for (edge in edge_list) {
  source <- edge[1, "source"]
  target <- edge[1, "target"]
  
  if (!source %in% names(adj_list)) {
    adj_list[[source]] <- c()
  }
  if (!target %in% names(adj_list)) {
    adj_list[[target]] <- c()
  }
  
  adj_list[[source]] <- c(adj_list[[source]], target)
  adj_list[[target]] <- c(adj_list[[target]], source)
}

# Function to perform BFS
bfs <- function(adj_list, start_node) {
  visited <- c()
  queue <- c(start_node)
  connected_nodes <- c()
  
  while (length(queue) > 0) {
    current_node <- queue[1]
    queue <- queue[-1]
    
    if (!current_node %in% connected_nodes) {
      connected_nodes <- c(connected_nodes, current_node)
      neighbors <- adj_list[[current_node]]
      if (!is.null(neighbors)) {
        queue <- c(queue, neighbors)
      }
    }
  }
  
  return(connected_nodes)
}

# Find all nodes connected to "SouthSeafood Express Corp"
target_company <- "SouthSeafood Express Corp"
connected_nodes <- bfs(adj_list, target_company)

# Filter the nodes and edges based on connected nodes
filtered_nodes <- nodes %>% filter(id %in% connected_nodes)
filtered_edges <- edges %>% filter(source %in% connected_nodes & target %in% connected_nodes)

# Ensure IDs are character type
filtered_nodes$id <- as.character(filtered_nodes$id)
filtered_edges$source <- as.character(filtered_edges$source)
filtered_edges$target <- as.character(filtered_edges$target)

# Create an adjacency matrix
adj_matrix <- matrix(0, nrow = nrow(filtered_nodes), ncol = nrow(filtered_nodes))
rownames(adj_matrix) <- filtered_nodes$id
colnames(adj_matrix) <- filtered_nodes$id

for (i in 1:nrow(filtered_edges)) {
  source <- filtered_edges[i, "source"]
  target <- filtered_edges[i, "target"]
  adj_matrix[source, target] <- 1
  adj_matrix[target, source] <- 1
}

# Create a dendrogram
dendrogram <- as.dendrogram(hclust(dist(adj_matrix)))

# Plot the dendrogram using ggraph and add labels, then flip coordinates
ggraph(dendrogram, 'dendrogram') + 
  geom_edge_elbow() + 
  geom_node_text(aes(label = label, filter = leaf), hjust = 0, nudge_x = 0.5, size = 3, angle = 0) +
  coord_flip() +
  scale_y_reverse() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.margin = unit(c(1, 10, 1, 1), "cm")) # Significantly increased the right margin
