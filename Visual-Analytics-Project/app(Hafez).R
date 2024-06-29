# Install required packages (if not installed)
install.packages(c("Rcpp", "ggplot2", "munsell", "htmltools", "DBI", "assertthat",
                   "gridExtra", "digest", "fpc", "TSP", "registry", "gclus", "gplots", "RColorBrewer",
                   "stringr", "labeling", "yaml","shiny","pacman"))

# Load required packages
pacman::p_load(shiny, ggraph, dendextend, tidygraph, jsonlite, dplyr, lubridate, igraph, ggplot2, gridExtra, ggdendro, ape)

# Load JSON data
mc3_data <- fromJSON("data/mc3_cleaned.json")

# Extract nodes and edges
nodes <- mc3_data$nodes
edges <- mc3_data$links

# Ensure 'id' columns and edge 'source'/'target' columns are character type
nodes$id <- as.character(nodes$id)
edges$source <- as.character(edges$source)
edges$target <- as.character(edges$target)
edges$end_date <- as.Date(edges$end_date)

# Data Processing
nodes <- nodes %>% filter(!is.na(id) & id != "")
edges <- edges %>% filter(!is.na(source) & source != "" & !is.na(target) & target != "")
nodes$id <- make.unique(nodes$id)

# Create unique list of companies
company_list <- unique(c(edges$source, edges$target, nodes$id))

# Define UI
ui <- fluidPage(
  titlePanel("Network Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("target_company", "Select Company", choices = NULL, selected = "SouthSeafood Express Corp"),
      dateInput("key_date", "Select Key Date", value = "2035-05-25", format = "dd M yyyy"),
      radioButtons("outlier_type", "Select Data Range", choices = c("Top", "Bottom"), selected = "Top"),
      numericInput("percentage", "Insert Percentage (%)", value = 5, min = 1, max = 100),
      actionButton("analyze", "Analyze Network")
    ),
    
    mainPanel(
      fluidRow(
        column(6, plotOutput("before_plot")),
        column(6, plotOutput("after_plot"))
      ),
      fluidRow(
        column(12, plotOutput("network_graph"))
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Update selectizeInput with company list
  updateSelectizeInput(session, "target_company", choices = company_list, server = TRUE)
  
  observeEvent(input$analyze, {
    req(input$target_company)
    
    withProgress(message = 'Analyzing Network', value = 0, {
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
      
      incProgress(0.2, detail = "Finding connected nodes")
      # Find all nodes connected to the selected company
      target_company <- input$target_company
      connected_nodes <- bfs(edges, target_company)
      
      incProgress(0.2, detail = "Filtering nodes and edges")
      # Filter the nodes and edges based on connected nodes
      filtered_nodes <- nodes %>% filter(id %in% connected_nodes)
      filtered_edges <- edges %>% filter(source %in% connected_nodes & target %in% connected_nodes)
      
      # Move 'source' and 'target' columns to the leftmost positions
      filtered_edges <- filtered_edges %>% select(source, target, everything())
      filtered_nodes <- filtered_nodes %>% select(id, everything())
      
      key_date <- as.Date(input$key_date, format = "%d %b %Y")
      
      # Separate the edges based on the key date
      before_edges <- filtered_edges %>% filter(is.na(end_date) | end_date <= key_date)
      after_edges <- filtered_edges %>% filter(is.na(end_date) | end_date > key_date)
      
      create_dendrogram <- function(nodes, edges, plot_title, highlight_nodes = NULL) {
        graph <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)
        dist_matrix <- distances(graph, to = V(graph), mode = "all")
        hc <- hclust(as.dist(dist_matrix), method = "complete")
        dend <- as.dendrogram(hc)
        dend_data <- dendro_data(dend)
        labels <- nodes$id[as.integer(dend_data$labels$label)]  # Use node IDs instead of labels
        
        plot <- ggplot() +
          geom_segment(data = segment(dend_data), aes(x = x, y = y, xend = xend, yend = yend)) +
          geom_text(data = label(dend_data), aes(x = x, y = y, label = labels), hjust = 0, size = 3) +
          coord_flip() +
          ggtitle(plot_title) +
          theme_minimal() +
          theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
                axis.text = element_blank(), axis.title = element_blank(),  # Remove axis labels and ticks
                axis.ticks = element_blank(), axis.line = element_blank(),  # Remove axis ticks and lines
                panel.grid = element_blank())  # Remove grid lines
        
        if (!is.null(highlight_nodes)) {
          highlight_labels <- labels %in% highlight_nodes
          plot <- plot +
            geom_text(data = label(dend_data)[highlight_labels, ], aes(x = x, y = y, label = labels[highlight_labels]), hjust = 0, size = 3, color = "red")
        }
        
        return(plot)
      }
      
      incProgress(0.2, detail = "Creating before dendrogram")
      output$before_plot <- renderPlot({
        create_dendrogram(filtered_nodes, before_edges, paste("Before and On", format(key_date, "%d %b %Y")), highlight_nodes = c(target_company))
      })
      
      incProgress(0.2, detail = "Creating after dendrogram")
      output$after_plot <- renderPlot({
        create_dendrogram(filtered_nodes, after_edges, paste("After", format(key_date, "%d %b %Y")), highlight_nodes = c(target_company))
      })
      
      incProgress(0.1, detail = "Processing network graph")
      nodes_df <- as.data.frame(nodes)
      fishing_companies <- nodes_df %>%
        filter(grepl("fish|sea food|seafood", TradeDescription, ignore.case = TRUE)) %>%
        filter(!is.na(revenue) & is.finite(revenue))
      
      percentage <- input$percentage / 100
      
      if (input$outlier_type == "Top") {
        outliers <- fishing_companies %>%
          filter(revenue > quantile(revenue, 1 - percentage, na.rm = TRUE))
      } else {
        outliers <- fishing_companies %>%
          filter(revenue < quantile(revenue, percentage, na.rm = TRUE))
      }
      
      outlier_ids <- outliers$id
      edges_df <- as.data.frame(edges)
      
      related_edges <- edges_df %>%
        filter(source %in% outlier_ids | target %in% outlier_ids)
      
      edge_list <- as.matrix(related_edges[, c("source", "target")])
      
      g <- graph_from_edgelist(edge_list, directed = FALSE)
      V(g)$revenue <- ifelse(V(g)$name %in% outliers$id, outliers$revenue[match(V(g)$name, outliers$id)], NA)
      V(g)$country <- ifelse(V(g)$name %in% outliers$id, outliers$country[match(V(g)$name, outliers$id)], NA)
      V(g)$size <- ifelse(V(g)$name %in% outliers$id, 10, 5)
      V(g)$color <- ifelse(V(g)$name %in% outliers$id, "skyblue", "red")
      V(g)$shape <- ifelse(V(g)$name %in% outliers$id, "square", "circle")
      E(g)$color <- adjustcolor("gray", alpha.f = 0.5)
      
      output$network_graph <- renderPlot({
        plot_title <- paste("Network Graph of", input$outlier_type, input$percentage, "% Revenue Outliers")
        plot(g, vertex.size = V(g)$size, vertex.label.cex = ifelse(V(g)$name %in% outliers$id, 0.8, 0.6), 
             vertex.color = V(g)$color, vertex.shape = V(g)$shape,
             vertex.label = NA, edge.color = E(g)$color, layout = layout_with_fr, 
             main = plot_title)
      })
      
      incProgress(0.1, detail = "Calculating network metrics")
      output$edge_metrics <- renderTable({
        edge_metrics <- data.frame(
          Source = as.character(related_edges$source),
          Target = as.character(related_edges$target),
          Weight = ifelse(!is.null(E(g)$weight), E(g)$weight, NA)
        )
        edge_metrics
      })
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
