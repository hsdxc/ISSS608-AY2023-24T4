pacman::p_load(shiny, shinydashboard, shinythemes,
               plotly, DT, jsonlite, igraph, 
               tidygraph, ggraph, visNetwork,
               ggforce, skimr, tidyverse, rsconnect)


mc3_data <- fromJSON("data/mc3_cleaned.json")

mc3_nodes_raw <- as_tibble(mc3_data$nodes) %>%
  distinct()

mc3_nodes <- mc3_nodes_raw %>%
  mutate(founding_date = as.Date(founding_date),
         country = as.character(country),
         id = as.character(id),
         ProductServices = as.character(ProductServices),
         revenue = as.numeric(as.character(revenue)),
         type = as.character(type),
         HeadOfOrg = as.character(HeadOfOrg),
         PointOfContact = as.character(PointOfContact)) %>%
  select(id, 
         founding_date, 
         country, 
         type, 
         revenue, 
         ProductServices, 
         HeadOfOrg,
         PointOfContact) %>%
  rename(nodes_type = type)

mc3_edges_raw <- as_tibble(mc3_data$links) %>%
  distinct()

mc3_edges <- mc3_edges_raw %>%
  select(source, 
         target, 
         type, 
         start_date, 
         end_date) %>%
  mutate(source = as.character(source),
         target = as.character(target),
         type = as.character(type),
         start_date = as.Date(start_date),
         end_date = as.Date(end_date)) 

mc3_nodes_Organization <- mc3_nodes %>%
  mutate(founding_year = format(founding_date, format="%Y")) %>%
  filter(str_like(nodes_type, "%Entity.Organization%"))

Organization_historical_year <- mc3_nodes_Organization %>%
  group_by(`founding_year`, `nodes_type`) %>%
  summarise(count = n())

Organization_historical_year_select <- Organization_historical_year %>%
  select(founding_year, nodes_type, count) %>%
  pivot_wider(names_from = nodes_type, values_from = count) %>%
  rename(`Founding Year` = founding_year,
         `Company` = Entity.Organization.Company,
         `Fishing Company` = Entity.Organization.FishingCompany,
         `Logistics Company` = Entity.Organization.LogisticsCompany,
         `Financial Company` = Entity.Organization.FinancialCompany,
         `News Company` = Entity.Organization.NewsCompany,
         `NGO` = Entity.Organization.NGO)

Organization_historical_year_select <- 
  Organization_historical_year_select[
    order(Organization_historical_year_select$`Founding Year`),]


nodes_type <- mc3_nodes %>%
  select(id, nodes_type)

mc3_edges <- mc3_edges %>%
  left_join(nodes_type, by = c("source" = "id")) %>%
  rename(nodes_type_source = nodes_type) %>%
  left_join(nodes_type, by = c("target" = "id")) %>%
  rename(nodes_type_target = nodes_type)

edges_BO_indv_count <- mc3_edges %>%
  filter(type == "Event.Owns.BeneficialOwnership") %>%
  group_by(start_date, source) %>%  
  summarise(BO_indv_count = n())%>%
  group_by(source) %>%
  mutate(BO_indv_total = cumsum(BO_indv_count)) %>%
  ungroup()

BO_owners_count <- mc3_edges %>%
  filter(type == "Event.Owns.BeneficialOwnership") %>%
  group_by(start_date, target) %>%  
  summarise(BeneficialOwnership_count = n())%>%
  group_by(target) %>%
  mutate(BeneficialOwnership_total = cumsum(BeneficialOwnership_count)) %>%
  ungroup()

mc3_nodes_id <- mc3_nodes %>%
  rename(label = id) %>%
  mutate(id = row_number()) %>%
  select(id, label, nodes_type)

nodes_type_id <- mc3_nodes_id %>%
  select(id, label)

mc3_edges_id <- mc3_edges %>%
  left_join(nodes_type_id, by = c("source" = "label")) %>%
  rename(from = id) %>%
  left_join(nodes_type_id, by = c("target" = "label")) %>%
  rename(to = id) %>%
  select(from, 
         to, 
         source, 
         target, 
         nodes_type_source, 
         nodes_type_target,
         type, 
         start_date)

mc3_edges_id$width <- with(mc3_edges_id, 
                           ifelse(str_detect(type, 
                                             'Event.Owns.BeneficialOwnership'),
                                  '0.1',
                                  ifelse(str_detect(type, 
                                                    'Event.Owns.Shareholdership'),
                                         '0.01','0.001')))
mc3_edges_id$label <- with(mc3_edges_id, 
                           ifelse(str_detect(type, 
                                             'Event.Owns.BeneficialOwnership'),
                                  'BO',
                                  ifelse(str_detect(type, 
                                                    'Event.Owns.Shareholdership'),
                                         'SH',
                                         ifelse(str_detect(type, 
                                                           'Event.WorksFor'),
                                                'WF', 'FR'))))
mc3_edges_id$font.color <- with(mc3_edges_id, 
                                ifelse(str_detect(type, 
                                                  'Event.Owns.BeneficialOwnership'),
                                       'red',
                                       ifelse(str_detect(type, 
                                                         'Event.Owns.Shareholdership'),
                                              'blue',
                                              ifelse(str_detect(type, 
                                                                'Event.WorksFor'),
                                                     'lightblue', 'yellow'))))


# Define UI for application
ui <- navbarPage(
  title = "VAST 2024 MC3: Interactive Data Exploration and Analysis",
  fluid = TRUE,
  theme = shinytheme("flatly"),
  id = "navbarID",
  navbarMenu("Timeseries",
             tabPanel("Yearly founded organizations",
                      mainPanel(
                        width = 12,
                        plotlyOutput(
                          outputId = "Plot1"
                          )
                        )
                      ),
             tabPanel(
               "Individuals Beneficial Ownership",
               sidebarLayout(
                 sidebarPanel(
                   width = 3,
                   selectizeInput(
                     inputId = "source",
                     label = "Select an individual",
                     choices = unique(edges_BO_indv_count$source),
                     selected = "",
                     multiple = TRUE
                     ),
                   ),
                 mainPanel(
                   width = 12,
                   plotlyOutput(
                     outputId = "Plot2"
                     )
                   )
                 )
               ),
             tabPanel(
               "Organizations Beneficial Ownership",
               sidebarLayout(
                 sidebarPanel(
                   width = 3,
                   selectizeInput(
                     inputId = "target",
                     label = "Select an organization",
                     choices = unique(BO_owners_count$target),
                     selected = "",
                     multiple = TRUE
                     ),
                   ),
                 mainPanel(width = 12,
                           plotlyOutput(
                             outputId = "Plot3"
                             )
                           )
                        )
                      )
             ),
  tabPanel("Network Graph",
    sidebarLayout(
      sidebarPanel(width = 2,
                   height = "fit-content",
                   dateInput(inputId = "date",
                             label = "Select the specific date:",
                             format = "yyyy-mm-dd",
                             min = "2000-01-01",
                             max = "2036-01-01",
                             value  = "2035-01-01"),
                   sliderInput(inputId ="date", 
                               label = "Select the current year:",
                               min = as.Date("2000", "%Y"), 
                               max = as.Date("2036", "%Y"),
                               value = as.Date("2035", "%Y"),
                               step = 1,
                               timeFormat= "%Y"),
                   sliderInput(inputId ="relationship_min", 
                               label = "Select min relationship:",
                               min = 0, 
                               max = 120,
                               value = 48),
                   sliderInput(inputId ="relationship_max", 
                               label = "Select max relationship:",
                               min = 0, 
                               max = 120,
                               value = 120),
                   checkboxGroupInput(
                     inputId = "type",
                     label = "Select relationship type",
                     choices = unique(mc3_edges_id$label),
                     selected = "BO")
                     
                   ),
      mainPanel(
        width = 10,
        height = 1200,
        visNetworkOutput("NetworkPlot", 
                         width = "100%", 
                         height = "620px")
      )
    )
    
))
  


# Define server logic
server <- function(input, output) {
  output$Plot1 <- renderPlotly({
    plot_ly(as.data.frame(Organization_historical_year_select),
            x = ~`Founding Year`,
            y = ~`Company`,
            name = "Company",
            type = 'scatter',
            mode = 'lines+markers',
            text = ~paste("Year: ", `Founding Year`, 
                          "<br>Founded: ", Company),
            hoverinfo = 'text') %>%
      add_trace(y = ~`Fishing Company`, 
                name = 'Fishing Company', 
                mode = 'lines+markers',
                text = ~paste("Year: ", `Founding Year`,
                              "<br>Founded: ", `Fishing Company`),
                hoverinfo = 'text') %>%
      add_trace(y = ~`Logistics Company`, 
                name = 'Logistics Company', 
                mode = 'lines+markers',
                text = ~paste("Year: ", `Founding Year`,
                              "<br>Founded: ", `Logistics Company`),
                hoverinfo = 'text') %>%
      add_trace(y = ~`Financial Company`, 
                name = 'Financial Company', 
                mode = 'lines+markers',
                text = ~paste("Year: ", `Founding Year`,
                              "<br>Founded: ", `Financial Company`),
                hoverinfo = 'text') %>%
      add_trace(y = ~`News Company`, 
                name = 'News Company', 
                mode = 'lines+markers',
                text = ~paste("Year: ", `Founding Year`,
                              "<br>Founded: ", `News Company`),
                hoverinfo = 'text') %>%
      add_trace(y = ~`NGO`, 
                name = 'NGO', 
                mode = 'lines+markers',
                text = ~paste("Year: ", `Founding Year`,
                              "<br>Founded: ", `NGO`),
                hoverinfo = 'text') %>%
      layout(legend = list(orientation = 'h', x = 0, y = 1.2),
             xaxis = list(title = "Founding Year"
                          
                          # rangeslider = list(visible = TRUE, 
                          #                    thickness = 0.03)
                          ),
             yaxis = list(title = "Count"))
    })
  output$Plot2 <- renderPlotly({
    plot_ly(edges_BO_indv_count,
              x = ~start_date,
              y = ~BO_indv_total,
              split = ~source,
              name = ~source,
              text = ~paste("Day: ", start_date, 
                            "<br>Own: ", BO_indv_total,
                            "<br>Name: ", source),
              hoverinfo = 'text') %>%
      filter(source %in% input$source) %>%
      group_by(source) %>%
      add_trace(mode = "lines+markers") %>%
      layout(title = 'Individual Beneficial Ownership over time',
             xaxis = list(title = "Time"
                          # rangeslider = list(visible = TRUE,
                          #                    thickness = 0.03)
                          ),
             yaxis = list(title = "Count"),
             showlegend = TRUE)
    })
  output$Plot3 <- renderPlotly({
    plot_ly(BO_owners_count,
            x = ~start_date,
            y = ~BeneficialOwnership_total,
            split = ~target,
            name = ~target,
            text = ~paste("Day: ", start_date, 
                          "<br>Owners: ", BeneficialOwnership_total,
                          "<br>Name: ", target),
            hoverinfo = 'text') %>%
      filter(target %in% input$target) %>%
      group_by(target) %>%
      add_trace(mode = "lines+markers") %>%
      layout(title = 'Organization Beneficial Owners over time',
             xaxis = list(title = "Time"
                          # rangeslider = list(visible = TRUE,
                          #                    thickness = 0.03)
                          ),
             yaxis = list(title = "Count"),
             showlegend = TRUE)
      })
  
  output$NetworkPlot <- renderVisNetwork({
    relationship_count <- mc3_edges_id %>%
      filter(label %in% input$type) %>%
      filter(start_date <= input$date) %>%
      # group_by(start_date, target) %>%
      # summarise(count = n())%>%
      group_by(target) %>%
      mutate(total = n()) %>%
      ungroup()
    
    Company_list <- 
      relationship_count[
        order(relationship_count$total,
              decreasing = T),] %>%
      filter(between(total, 
                     input$relationship_min,
                     input$relationship_max)) %>%
      # filter(total>= input$relationship_min &
      #          total <= input$relationship_max) %>%
      select(target) %>%
      distinct()
      
    edges <- mc3_edges_id %>%
      filter(start_date <= input$date) %>%
      filter(label %in% input$type) %>%
      filter(target %in% Company_list$target)
    
    nodes <- mc3_nodes_id %>%
      filter(id %in% edges$from | 
               id %in% edges$from | 
               label %in% Company_list$target)
    
    nodes$group <- with(nodes,
                        ifelse(str_detect(nodes_type, 
                                          'Entity.Organization.Company'),
                               'Company',
                               ifelse(str_detect(nodes_type,
                                                 'Entity.Organization.LogisticsCompany'),
                                      'Logistics Company',
                                      ifelse(str_detect(nodes_type,
                                                        'Entity.Organization.FishingCompany'),
                                             'Fishing Company', 
                                             ifelse(str_detect(nodes_type,
                                                               'Entity.Organization.FinancialCompany'),
                                                    'Financial Company',
                                                    ifelse(str_detect(nodes_type,
                                                                      'Entity.Organization.NewsCompany'),
                                                           'News Company',
                                                           ifelse(str_detect(nodes_type,
                                                                             'Entity.Organization.NGO'),
                                                                  'NGO',
                                                                  ifelse(str_detect(nodes_type,
                                                                                    'Entity.Person.CEO'),
                                                                         'CEO', 
                                                                         ifelse(str_detect(nodes_type,
                                                                                           'Entity.Person'),
                                                                                'Person','Person')
                                                                  )
                                                           )
                                                    )
                                             )
                                      )
                               )
                        )
    )
    
    nodes <- transform(nodes,
                       `Company` = ifelse(
                         nodes_type %in% c("Entity.Organization.Company",
                                           "Entity.Organization.LogisticsCompany",
                                           "Entity.Organization.FishingCompany",
                                           "Entity.Organization.FinancialCompany",
                                           "Entity.Organization.NewsCompany",
                                           "Entity.Organization.NGO"),
                         label, "--------"))
    
    visNetwork(nodes, edges) %>%
      visLayout(randomSeed = 1234) %>%
      addFontAwesome()%>%
      # visNodes(size = 1) %>%
      visEdges(selectionWidth = 0.1) %>%
      visIgraphLayout(layout = "layout_nicely") %>%
      visGroups(groupname =  "Company",
                shape = "icon",
                icon = list(code = "f1ad",
                            color = "#ff0000",
                            size = 20),
                font = list(size = 8)) %>%
      visGroups(groupname =  "Logistics Company",
                shape = "icon",
                icon = list(code = "f1ad",
                            color = "#00ff40",
                            size = 20),
                font = list(size = 8)) %>%
      visGroups(groupname =  "Fishing Company",
                shape = "icon",
                icon = list(code = "f1ad",
                            color = "#FFD43B",
                            size = 20),
                font = list(size = 8)) %>%
      visGroups(groupname =  "Financial Company",
                shape = "icon",
                icon = list(code = "f1ad",
                            color = "#ff006f",
                            size = 20),
                font = list(size = 8)) %>%
      visGroups(groupname =  "News Company",
                shape = "icon",
                icon = list(code = "f1ad",
                            color = "#0c0d0d",
                            size = 20),
                font = list(size = 8)) %>%
      visGroups(groupname =  "NGO",
                shape = "icon",
                icon = list(code = "f1ad",
                            color = "#B197FC",
                            size = 20),
                font = list(size = 8)) %>%
      visGroups(groupname =  "Person",
                shape = "icon",
                icon = list(code = "f007",
                            size = 12),
                font = list(size = 5)) %>%
      visGroups(groupname =  "CEO",
                shape = "icon",
                icon = list(code = "f21b",
                            color = "#FFD43B",
                            size = 12),
                font = list(size = 5)) %>%
      visEdges(
        # arrows = list(to = list(enabled = TRUE, 
        #                                caleFactor = 0.1)),
               smooth = list(enabled = TRUE,
                             type = "vertical"),
               font = list(size = 2)) %>%  
      
    #   'dynamic', 'continuous', 'discrete', 'diagonalCross', 
    # 'straightCross', 'horizontal', 'vertical', 'curvedCW', 
    # 'curvedCCW', 'cubicBezier'.
    
      visLegend(width = 0.15) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visOptions(
        # nodesIdSelection = TRUE,
        highlightNearest = list(enabled = T,
                                degree = 2,
                                hover = F),
        selectedBy = list(variable = "Company",
                          highlight = TRUE))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)