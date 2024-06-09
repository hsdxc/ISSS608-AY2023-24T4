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

# Sample 10% of the data
set.seed(123)  # For reproducibility
nodes_sample <- nodes %>% sample_frac(0.05)
edges_sample <- edges %>% sample_frac(0.05)

# Create new JSON structure
json_sample <- list(nodes = nodes_sample, links = edges_sample)

# Write to new JSON file
write_json(json_sample, "data/MC3/mc3_sample.json")

# Print confirmation message
cat("Sample data has been saved to 'data/MC3/mc3_sample.json'")