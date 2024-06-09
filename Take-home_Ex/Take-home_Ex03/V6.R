# Load necessary libraries
library(jsonlite)
library(ggplot2)
library(igraph)
library(dplyr)

# Load JSON data
file_path <- 'data/MC3/mc3.json'
json_data <- fromJSON(file_path)

# Extract the nodes and edges
nodes <- json_data$nodes
edges <- json_data$links

# Convert nodes to a data frame for filtering purposes
nodes_df <- as.data.frame(nodes)

# Filter the dataset for companies involved in fishing, sea food, and seafood
fishing_companies <- nodes_df %>%
  filter(grepl("fish|sea food|seafood", TradeDescription, ignore.case = TRUE))

# Remove rows with missing or non-finite revenue values
fishing_companies <- fishing_companies %>%
  filter(!is.na(revenue) & is.finite(revenue))

# Identify top outliers by revenue (95th percentile)
top_outliers <- fishing_companies %>%
  filter(revenue > quantile(revenue, 0.95, na.rm = TRUE))

# Display top outliers
print(top_outliers)

# Extract the IDs of the top outliers
top_outlier_ids <- top_outliers$id

# Convert edges to a data frame for filtering purposes
edges_df <- as.data.frame(edges)

# Filter edges to include only those related to top outliers
related_edges <- edges_df %>%
  filter(source %in% top_outlier_ids | target %in% top_outlier_ids)

# Create an edge list for igraph
edge_list <- as.matrix(related_edges[, c("source", "target")])

# Create graph object
g <- graph_from_edgelist(edge_list, directed = FALSE)

# Add vertex attributes for revenue and country
V(g)$revenue <- ifelse(V(g)$name %in% top_outliers$id, top_outliers$revenue[match(V(g)$name, top_outliers$id)], NA)
V(g)$country <- ifelse(V(g)$name %in% top_outliers$id, top_outliers$country[match(V(g)$name, top_outliers$id)], NA)

# Improve the layout and reduce label density
V(g)$size <- ifelse(V(g)$name %in% top_outliers$id, 10, 5)  # Increase size for top outliers
V(g)$color <- ifelse(V(g)$name %in% top_outliers$id, "skyblue", "orange")  # Different colors for top outliers
E(g)$color <- "gray"  # Edge color

plot(g, vertex.size = V(g)$size, vertex.label.cex = 0.6, vertex.color = V(g)$color, 
     vertex.label = ifelse(V(g)$name %in% top_outliers$id, V(g)$name, NA), 
     edge.color = E(g)$color, layout = layout_with_fr, 
     main = "Network Graph of Top Revenue Outliers")
