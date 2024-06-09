# Load necessary packages
library(pacman)
p_load(tidyverse, jsonlite, igraph, dplyr, lubridate, tidygraph,ggraph)

# Load JSON data
json_data <- fromJSON("data/MC3/mc3.json")

# Review loaded JSON data
glimpse(json_data)

# Assuming 'nodes' and 'links' are directly accessible and properly formatted
nodes <- as.data.frame(json_data$nodes)
edges <- as.data.frame(json_data$links)

# Convert date fields in nodes
nodes$founding_date <- as.Date(nodes$founding_date, format = "%Y-%m-%dT%H:%M:%S")
nodes$`_last_edited_date` <- as.Date(nodes$`_last_edited_date`, format = "%Y-%m-%dT%H:%M:%S")
nodes$`_date_added` <- as.Date(nodes$`_date_added`, format = "%Y-%m-%dT%H:%M:%S")

# Convert date fields in edges
edges$start_date <- as.Date(edges$start_date, format = "%Y-%m-%dT%H:%M:%S")
edges$end_date <- as.Date(edges$end_date, format = "%Y-%m-%dT%H:%M:%S")  # Handle NA values properly

# Ensure 'from' and 'to' columns in edges correctly reference nodes 'id'
edges$from <- edges$source
edges$to <- edges$target

# Aggregate edges to combine duplicates and calculate weights
edges_aggregated <- edges %>%
  group_by(from, to) %>%
  summarise(Weight = n(), .groups = "drop")  # Summarise number of interactions between each pair

# Remove rows with NA in critical columns
edges_aggregated <- na.omit(edges_aggregated)

# Check for NaN values in Weight
sum(is.nan(edges_aggregated$Weight))  # Should be 0

# Replace NaN values in Weight with 0 (or any appropriate value)
edges_aggregated$Weight[is.nan(edges_aggregated$Weight)] <- 0

# Ensure all values in Weight are integers
edges_aggregated$Weight <- as.integer(edges_aggregated$Weight)

# Export nodes and edges to CSV
write.csv(nodes, "data/MC3/nodes.csv", row.names = FALSE)
write.csv(edges_aggregated, "data/MC3/edges.csv", row.names = FALSE)

# Create the graph using tbl_graph from the tidygraph package
mc3_graph <- tbl_graph(nodes = nodes, edges = edges_aggregated, directed = TRUE,node_key="id")

print(mc3_graph)


