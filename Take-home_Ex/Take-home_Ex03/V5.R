# Load the necessary libraries
library(ggraph)
library(igraph)
library(dendextend)
library(tidygraph)
library(jsonlite)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)

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

# Convert date fields in nodes
nodes$`founding_date` <- as.Date(nodes$`founding_date`, format = "%Y-%m-%dT")
nodes$`_last_edited_date` <- as.Date(nodes$`_last_edited_date`, format = "%Y-%m-%dT")
nodes$`_date_added` <- as.Date(nodes$`_date_added`, format = "%Y-%m-%dT")

# Convert date fields in edges
edges$`start_date` <- as.Date(edges$`start_date`, format = "%Y-%m-%dT")
edges$`end_date` <- as.Date(edges$`end_date`, format = "%Y-%m-%dT")


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

# Remove duplicate nodes based on the 'id' column
nodes <- nodes %>% distinct(id, .keep_all = TRUE)

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

# Check for duplicate node IDs in filtered_nodes
if (any(duplicated(filtered_nodes$id))) {
  cat("Duplicate node IDs found and removed:\n")
  print(filtered_nodes %>% filter(duplicated(id)))
  filtered_nodes <- filtered_nodes %>% distinct(id, .keep_all = TRUE)
}

# Check for NA values and replace with "NA"
filtered_nodes$id[is.na(filtered_nodes$id)] <- "NA"
filtered_edges$source[is.na(filtered_edges$source)] <- "NA"
filtered_edges$target[is.na(filtered_edges$target)] <- "NA"

# Replace NA values in start_date and end_date
filtered_edges <- filtered_edges %>%
  mutate(
    start_date = ifelse(is.na(start_date), as.Date("1901-01-01"), start_date),
    end_date = ifelse(is.na(end_date), as.Date("1999-12-31"), end_date)
  )

# Convert date fields in nodes
nodes$`founding_date` <- as.Date(nodes$`founding_date`, format = "%Y-%m-%dT")
nodes$`_last_edited_date` <- as.Date(nodes$`_last_edited_date`, format = "%Y-%m-%dT")
nodes$`_date_added` <- as.Date(nodes$`_date_added`, format = "%Y-%m-%dT")

# Convert date fields in edges
edges$`start_date` <- as.Date(edges$`start_date`, format = "%Y-%m-%dT")
edges$`end_date` <- as.Date(edges$`end_date`, format = "%Y-%m-%dT")

# Remove rows with NA or empty values in nodes and edges
nodes <- nodes %>% filter(!is.na(id) & id != "")
edges <- edges %>% filter(!is.na(source) & source != "" & !is.na(target) & target != "")

# Rearrange columns
filtered_edges <- edges %>% select(source, target, everything())
filtered_nodes <- nodes %>% select(id, everything())

# Create an igraph object from the filtered edges
graph <- graph_from_data_frame(d = filtered_edges, vertices = filtered_nodes, directed = FALSE)
cat("Created igraph object for the entire graph.\n")

# Derive unique change points from the connected edges
change_points <- sort(unique(c(filtered_edges$start_date, filtered_edges$end_date)))
cat("Change points:\n")
print(change_points)

# Initialize an empty list to store dendrograms
dendrograms <- list()
labels <- c()

# Loop through each change point interval to create filtered edges and dendrograms
for (i in seq_along(change_points[-length(change_points)])) {
  interval_start <- change_points[i]
  interval_end <- change_points[i + 1]
  
  cat("Processing interval:", format(interval_start, "%Y-%m-%d"), "to", format(interval_end, "%Y-%m-%d"), "\n")
  
  # Filter edges based on the date range
  interval_edges <- filtered_edges %>%
    filter(start_date >= interval_start & start_date < interval_end)
  print(paste("Filtered edges:", nrow(interval_edges)))
  
  if (nrow(interval_edges) == 0) {
    cat("No edges found in this interval.\n")
    next
  } else {
    print("Filtered edges in this interval:")
    print(interval_edges)
  }
  
  # Check for NA or empty values in the node IDs or edge source/target
  if (any(is.na(filtered_nodes$id) | filtered_nodes$id == "")) {
    stop("NA or empty values found in node IDs.")
  }
  if (any(is.na(interval_edges$source) | interval_edges$source == "" | is.na(interval_edges$target) | interval_edges$target == "")) {
    stop("NA or empty values found in edge source or target.")
  }
  cat("No NA or empty values in node IDs and edge source/target.\n")
  
  # Ensure IDs are character type
  filtered_nodes$id <- as.character(filtered_nodes$id)
  interval_edges$source <- as.character(interval_edges$source)
  interval_edges$target <- as.character(interval_edges$target)
  
  # Create an adjacency matrix
  adj_matrix <- matrix(0, nrow = nrow(filtered_nodes), ncol = nrow(filtered_nodes))
  rownames(adj_matrix) <- filtered_nodes$id
  colnames(adj_matrix) <- filtered_nodes$id
  
  for (j in 1:nrow(interval_edges)) {
    source <- interval_edges[j, "source"]
    target <- interval_edges[j, "target"]
    adj_matrix[source, target] <- 1
    adj_matrix[target, source] <- 1
  }
  
  # Debug: Print the adjacency matrix to ensure it is correctly formed
  cat("Adjacency Matrix for interval", format(interval_start, "%Y-%m-%d"), "to", format(interval_end, "%Y-%m-%d"), ":\n")
  print(adj_matrix)
  
  # Create a dendrogram
  if (nrow(adj_matrix) > 1) {
    dendrogram <- as.dendrogram(hclust(dist(adj_matrix)))
    dendrograms <- c(dendrograms, list(dendrogram))
    labels <- c(labels, paste0(format(interval_start, "%Y-%m-%d"), " to ", format(interval_end, "%Y-%m-%d")))
  }
}

# Prepare the data for faceting
filtered_edges$interval <- cut(filtered_edges$start_date, breaks = change_points, include.lowest = TRUE)

# Create a graph object with tidygraph for ggraph
graph_tbl <- as_tbl_graph(graph_from_data_frame(filtered_edges, vertices = filtered_nodes))

# Set the graph style
g <- ggraph(graph_tbl, layout = 'nicely') + 
  geom_edge_link(aes(width = 0.5), alpha = 0.5) +
  geom_node_point(aes(colour = id), size = 2) +
  scale_edge_width(range = c(0.1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Add faceting by interval
g + facet_edges(~interval)
