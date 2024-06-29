# Load the necessary libraries
library(ggraph)
library(dendextend)
library(tidygraph)
library(jsonlite)
library(dplyr)
library(grid)
library(ape)
library(ggdendro)
library(igraph)

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

# Convert date columns to Date type
edges$end_date <- as.Date(edges$end_date)

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

# Create a new label combining company name and revenue
nodes$label <- paste(nodes$id, "- Revenue:", nodes$revenue)

# Create the graph using igraph
graph <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

# Function to perform BFS
bfs <- function(graph, start_node) {
  bfs_result <- bfs(graph, root = start_node, dist = TRUE)
  return(bfs_result$order[bfs_result$order > 0])
}

# Find all nodes connected to "SouthSeafood Express Corp"
target_company <- "SouthSeafood Express Corp"
connected_nodes <- bfs(graph, target_company)

# Filter the nodes and edges based on connected nodes
filtered_nodes <- nodes %>% filter(id %in% connected_nodes)
filtered_edges <- edges %>% filter(source %in% connected_nodes & target %in% connected_nodes)

# Key date
key_date <- as.Date("2035-05-25")

# Separate the edges based on the key date
before_edges <- filtered_edges %>% filter(is.na(end_date) | end_date <= key_date)
after_edges <- filtered_edges %>% filter(is.na(end_date) | end_date >= key_date)

# Function to create a dendrogram plot using ggdendro
create_dendrogram <- function(nodes, edges, plot_title) {
  # Create the graph using igraph
  graph <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
  
  # Calculate the shortest path distances
  dist_matrix <- distances(graph, v = V(graph), to = V(graph), weights = NA)
  
  # Create a hierarchical clustering object
  hc <- hclust(as.dist(dist_matrix), method = "complete")
  
  # Convert to a dendrogram object
  dend <- as.dendrogram(hc)
  
  # Convert the dendrogram to a format suitable for ggplot
  dend_data <- dendro_data(dend)
  
  # Plot the dendrogram using ggdendro
  ggdendrogram(dend_data, rotate = TRUE, theme_dendro = FALSE) +
    ggtitle(plot_title) +
    theme(plot.margin = unit(c(1, 15, 1, 1), "cm")) # Increase the right margin
}

# Create and plot the "before" dendrogram
create_dendrogram(filtered_nodes, before_edges, "Before 2035-05-25")

# Create and plot the "after" dendrogram
create_dendrogram(filtered_nodes, after_edges, "After 2035-05-25")
