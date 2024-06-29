# Load the necessary libraries
library(ggraph)
library(dendextend)
library(tidygraph)
library(jsonlite)
library(dplyr)
library(grid)
library(ape)
library(ggdendro)

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

# Ensure unique id values
nodes$id <- make.unique(nodes$id)

# Replace NA values in id with a unique identifier
nodes$id[is.na(nodes$id)] <- paste0("NA_", seq_along(nodes$id[is.na(nodes$id)]))

# Create a new label combining company name and revenue
nodes$label <- paste(nodes$id,",Rev:", nodes$revenue)

# Function to perform BFS
bfs <- function(edges, start_node) {
  adj_list <- list()
  for (i in 1:nrow(edges)) {
    source <- edges[i, "source"]
    target <- edges[i, "target"]
    if (!source %in% names(adj_list)) adj_list[[source]] <- c()
    if (!target %in% names(adj_list)) adj_list[[target]] <- c()
    adj_list[[source]] <- c(adj_list[[source]], target)
    adj_list[[target]] <- c(adj_list[[target]], source)
  }
  
  visited <- setNames(rep(FALSE, length(adj_list)), names(adj_list))
  queue <- c(start_node)
  visited[start_node] <- TRUE
  
  while (length(queue) > 0) {
    node <- queue[1]
    queue <- queue[-1]
    neighbors <- adj_list[[node]]
    for (neighbor in neighbors) {
      if (!visited[neighbor]) {
        queue <- c(queue, neighbor)
        visited[neighbor] <- TRUE
      }
    }
  }
  return(names(visited[visited]))
}

# Find all nodes connected to "SouthSeafood Express Corp"
target_company <- "SouthSeafood Express Corp"
connected_nodes <- bfs(edges, target_company)

# Filter the nodes and edges based on connected nodes
filtered_nodes <- nodes %>% filter(id %in% connected_nodes)
filtered_edges <- edges %>% filter(source %in% connected_nodes & target %in% connected_nodes)

# Move 'source' and 'target' columns to the leftmost positions
filtered_edges <- filtered_edges %>% select(source, target, everything())
filtered_nodes <- filtered_nodes %>% select(id, everything())

# Key date
key_date <- as.Date("2035-05-25")

# Separate the edges based on the key date
before_edges <- filtered_edges %>% filter(is.na(end_date) | end_date <= key_date)
after_edges <- filtered_edges %>% filter(is.na(end_date) | end_date >= key_date)

# Function to create a dendrogram plot using ggdendro
create_dendrogram <- function(nodes, edges, plot_title) {
  # Create tidygraph object
  graph <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)
  
  # Calculate shortest path distances
  dist_matrix <- distances(graph, to = V(graph), mode = "all")
  
  # Create a hierarchical clustering object
  hc <- hclust(as.dist(dist_matrix), method = "complete")
  
  # Convert to a dendrogram object
  dend <- as.dendrogram(hc)
  
  # Convert the dendrogram to a format suitable for ggplot
  dend_data <- dendro_data(dend)
  
  # Extract node labels
  labels <- nodes$label[as.integer(dend_data$labels$label)]
  
  # Plot the dendrogram using ggdendro
  ggplot() +
    geom_segment(data = segment(dend_data), aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_text(data = label(dend_data), aes(x = x, y = y, label = labels), hjust = 0, size = 3) +
    coord_flip() +
    scale_y_reverse(expand = c(0.2, 0)) +
    ggtitle(plot_title) +
    theme_minimal() +
    theme(plot.margin = unit(c(1, 1, 1, 10), "cm")) # Increase the left margin for labels
}

# Create and plot the "before" dendrogram
create_dendrogram(filtered_nodes, before_edges, "Before 2035-05-25")

# Create and plot the "after" dendrogram
create_dendrogram(filtered_nodes, after_edges, "After 2035-05-25")
