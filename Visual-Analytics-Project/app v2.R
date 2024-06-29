
# Load required packages
pacman::p_load(shiny, ggraph, dendextend, tidygraph, jsonlite, dplyr, lubridate, igraph, ggplot2, gridExtra, ggdendro, ape)

# Load JSON data
json_data <- fromJSON('data/MC3/mc3.json')

# Extract nodes and edges
nodes <- json_data$nodes
edges <- json_data$links

# Ensure 'id' columns and edge 'source'/'target' columns are character type
nodes$id <- as.character(nodes$id)
edges$source <- as.character(edges$source)
edges$target <- as.character(edges$target)
edges$end_date <- as.Date(edges$end_date)

# Data Processing
nodes <- nodes %>% filter(!is.na(id) & id != "")
edges <- edges %>% filter(!is.na(source) & source != "" & !is.na(target) & target != "")
nodes$id <- make.unique(nodes$id)

nodes$label <- paste(nodes$id, ",Rev:", nodes$revenue)

# Create unique list of companies
company_list <- unique(c(edges$source, edges$target, nodes$id))

# Define UI
ui <- fluidPage(
  titlePanel("Network Analysis of SouthSeafood Express Corp"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("target_company", "Select Company", choices = NULL, selected = "SouthSeafood Express Corp"),
      dateInput("key_date", "Select Key Date", value = "2035-05-25", format = "dd-M-yy"),
      radioButtons("outlier_type", "Select Outlier Type", choices = c("Top", "Bottom"), selected = "Top"),
      numericInput("percentage", "Select Percentage", value = 5, min = 1, max = 100),
      actionButton("analyze", "Analyze Network")
    ),
    
    mainPanel(
      fluidRow(
        column(6, plotOutput("before_plot")),
        column(6, plotOutput("after_plot"))
      ),
      plotOutput("network_graph")
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Update selectizeInput with company list
  updateSelectizeInput(session, "target_company", choices = company_list, server = TRUE)
  
  observeEvent(input$analyze, {
    req(input$target_company)
    
    withProgress(message = 'Analyzing network', value = 0, {
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
      
      key_date <- as.Date(input$key_date, format = "%d-%b-%Y")
      
      # Separate the edges based on the key date
      before_edges <- filtered_edges %>% filter(is.na(end_date) | end_date <= key_date)
      after_edges <- filtered_edges %>% filter(is.na(end_date) | end_date > key_date)
      
      create_dendrogram <- function(nodes, edges, plot_title) {
        graph <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)
        dist_matrix <- distances(graph, to = V(graph), mode = "all")
        hc <- hclust(as.dist(dist_matrix), method = "complete")
        dend <- as.dendrogram(hc)
        dend_data <- dendro_data(dend)
        labels <- nodes$label[as.integer(dend_data$labels$label)]
        
        ggplot() +
          geom_segment(data = segment(dend_data), aes(x = x, y = y, xend = xend, yend = yend)) +
          geom_text(data = label(dend_data), aes(x = x, y = y, label = labels), hjust = 0, size = 3) +
          coord_flip() +
          scale_y_reverse(expand = c(0.2, 0)) +
          ggtitle(plot_title) +
          theme_minimal() +
          theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) 
      }
      
      incProgress(0.2, detail = "Creating before dendrogram")
      output$before_plot <- renderPlot({
        create_dendrogram(filtered_nodes, before_edges, paste("Before", format(key_date, "%d-%b-%Y")))
      })
      
      incProgress(0.2, detail = "Creating after dendrogram")
      output$after_plot <- renderPlot({
        create_dendrogram(filtered_nodes, after_edges, paste("After", format(key_date, "%d-%b-%Y")))
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
        ggraph(g, layout = 'fr') + 
          geom_edge_link(aes(color = E(g)$color), alpha = 0.5) + 
          geom_node_point(aes(color = revenue, size = revenue)) + 
          scale_color_gradient(low = "yellow", high = "red") +
          geom_node_text(aes(label = name), repel = TRUE) +
          ggtitle(plot_title) +
          theme_graph()
      })
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
