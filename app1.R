library(shiny)
library(shinyWidgets) 
library(dplyr)
library(ggplot2)
library(visNetwork)
library(DT)
library(cronologia)
library(igraph)

comm_full <- read.csv("data/communications_full.csv") |>
  mutate(date = as.Date(date))             # make sure it’s Date, not character

valid_dates <- sort(unique(comm_full$date))

ui <- fluidPage(
  titlePanel(""),
  tags$head(
    tags$style(HTML("
    /* Data selection input UI 
    .well {
      background-color: #181d31 !important;
      color: white; 
    }*/
    
    /* Bold and pink for active tab */
    .nav-tabs > li.active > a, 
    .nav-tabs > li.active > a:focus, 
    .nav-tabs > li.active > a:hover {
      background-color: #f6e3f3 !important;  /* Light pink */
      font-weight: bold !important;
      color: black !important;
    }

    /* Inactive tabs style*/
    .nav-tabs > li > a {
      background-color: #f9f9f9;
      color: black;
      font-weight: bold !important;
    }

    /* Hover style */
    .nav-tabs > li > a:hover {
      background-color: #f1f1f1;
      color: #333;
    }
    
    /* Add spacing between tabs and content */
    .tab-content .shiny-plot-output,
    .tab-content .datatables,
    .tab-content .vis-network {
      margin-top: 20px;
    }
    
    /* Add spacing below the tab bar, including 'Select by id' */
    .tab-content .vis-network-html-widget {
      margin-top: 20px;
    }

    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      # Week selector -- select-all built-in
      pickerInput(
        "week_select", "Select Week(s)",
        choices   = unique(comm_full$week_label),
        selected  = unique(comm_full$week_label),
        multiple  = TRUE,
        options   = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      
      # Date range selector that only allows valid_dates
      airDatepickerInput(
        inputId   = "date_range",
        label     = "Select Date Range",
        range     = TRUE,
        value     = c(min(valid_dates), max(valid_dates)),
        minDate   = min(valid_dates),
        maxDate   = max(valid_dates),
        disabledDates = setdiff(
          seq(min(valid_dates), max(valid_dates), by = "day"),
          valid_dates                         # <- the dates you want to grey-out
        )
      ),
      
      sliderInput("hour_range", "Hour of Day", min = 0, max = 23, value = c(0, 23)),
      
      ## Sender entity / label
      pickerInput(
        "sender_entity", "Sender Entity Type",
        choices  = unique(comm_full$sender_type),
        selected = unique(comm_full$sender_type),
        multiple = TRUE,
        options  = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      pickerInput(
        "sender", "Sender",
        choices  = unique(comm_full$sender_label),
        selected = unique(comm_full$sender_label),
        multiple = TRUE,
        options  = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      
      ## Receiver entity / label
      pickerInput(
        "receiver_entity", "Receiver Entity Type",
        choices  = unique(comm_full$receiver_type),
        selected = unique(comm_full$receiver_type),
        multiple = TRUE,
        options  = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      pickerInput(
        "receiver", "Receiver",
        choices  = unique(comm_full$receiver_label),
        selected = unique(comm_full$receiver_label),
        multiple = TRUE,
        options  = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      
      actionButton("update", "Update View")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Temporal Pattern", plotOutput("time_plot")),
        tabPanel("Heatmap",        plotOutput("heatmap_plot")),
        tabPanel("Communication Network",
                 h4("Network Graph"),
                 visNetworkOutput("net_plot"),
                 br(),br(),br(),
                 h4("Centrality Measure"),
                 selectInput("centrality_type", "Select Centrality Measure",
                             choices = c("PageRank", "Betweenness", "Degree"),
                             selected = "PageRank"),
                 dataTableOutput("centrality_table")),
        tabPanel("Receipts", dataTableOutput("comm_table")),
        tabPanel("Communication Timeline", uiOutput("cronologia"))
      )
    )
  )
)


# ---- Server ----
server <- function(input, output, session) {
  
  # Reactive data filtering
  filtered_data <- eventReactive(
    input$update,
    {                           
      comm_full %>%
        filter(
          hour >= input$hour_range[1] & hour <= input$hour_range[2],
          (sender_label   %in% input$sender          | length(input$sender)          == 0),
          (receiver_label %in% input$receiver        | length(input$receiver)        == 0),
          (sender_type    %in% input$sender_entity   | length(input$sender_entity)   == 0),
          (receiver_type  %in% input$receiver_entity | length(input$receiver_entity) == 0),
          (week_label     %in% input$week_select     | length(input$week_select)     == 0),
          date >= input$date_range[1] & date <= input$date_range[2]
        )
    },
    ignoreInit = FALSE,   
    ignoreNULL = FALSE    
  )
  
  
  # ---- Time Series Plot (Daily with Facet by Week) ----
  output$time_plot <- renderPlot({
    df <- filtered_data() %>%
      count(week_label, date)
    
    ggplot(df, aes(x = date, y = n, group = week_label)) +
      geom_line(color = "#0072B2", linewidth = 1) +
      facet_wrap(~ week_label, scales = "free_x", ncol = 1) +
      labs(title = "Daily Communication Volume per Week", x = "Date", y = "Messages") +
      theme_minimal()+
      theme(
        plot.title = element_text(face = "bold", size = 18)  
      )
  })
  
  # ---- Heatmap Plot (Weekday vs Hour) ----
  output$heatmap_plot <- renderPlot({
    df <- filtered_data() %>%
      count(week_label,weekday, hour)
    
    ggplot(df, aes(x = hour, y = weekday, fill = n)) +
      geom_tile(color = "white") +
      scale_fill_viridis_c() +
      labs(title = "Communication Heatmap", x = "Hour", y = "Weekday", fill = "Messages") +
      facet_wrap(~ week_label, ncol = 1) +
      theme_minimal()+
      theme(
        plot.title = element_text(face = "bold", size = 18)  
      )
  })
  
  # ---- Network Graph ----
  output$net_plot <- renderVisNetwork({
    df <- filtered_data() %>%
      filter(!is.na(sender_label), !is.na(receiver_label)) %>%
      count(sender_label, receiver_label, name = "value")  # value used for edge weight
    
    if (nrow(df) == 0) return(NULL)
    
    # Get entity types from filtered data
    sender_types <- filtered_data() %>%
      select(name = sender_label, type = sender_type)
    
    receiver_types <- filtered_data() %>%
      select(name = receiver_label, type = receiver_type)
    
    node_types <- bind_rows(sender_types, receiver_types) %>%
      distinct(name, .keep_all = TRUE)
    
    # Shape mapping for entity type
    shape_map <- c(
      "Person" = "dot",
      "Organization" = "box",
      "Vessel" = "triangle",
      "Location" = "diamond",
      "Group" = "square"
    )
    
    # Create node list with the entity shape mapped
    graph_nodes <- tibble(name = unique(c(df$sender_label, df$receiver_label))) %>%
      left_join(node_types, by = "name") %>%
      mutate(
        id = name,
        label = name,
        group = type,
        shape = shape_map[type] %||% "dot"  # fallback to dot if NA
      )
    
    # Rename edge columns for visNetwork
    graph_edges <- df %>%
      rename(from = sender_label, to = receiver_label)
    
    # Define legend
    legend_nodes <- graph_nodes %>%
      filter(!is.na(group)) %>%
      distinct(group, shape) %>%
      mutate(
        label = group,
        color = case_when(
          group == "Person" ~ "#A0C1F7",
          group == "Vessel" ~ "#FFFF54",
          group == "Organization" ~ "#EB8584",
          group == "Location" ~ "#94DF5C",
          group == "Group" ~ "#DD83EE",
          TRUE ~ "gray"
        )
      ) %>%
      select(label, shape, color) %>%
      purrr::transpose()
    
    # visNetwork
    visNetwork(nodes = graph_nodes, edges = graph_edges) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 123) %>%
      visPhysics(stabilization = TRUE) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visLegend(useGroups = FALSE, addNodes = legend_nodes)
    
    
  })
  
  # ---- Centrality Measures ----
  centralitymeasure_df <- reactive({
    df <- filtered_data() %>%
      filter(!is.na(sender_label), !is.na(receiver_label)) %>%
      count(sender_label, receiver_label, name = "value")
    
    if (nrow(df) == 0) return(NULL)
    
    sender_types <- filtered_data() %>%
      select(name = sender_label, type = sender_type)
    
    receiver_types <- filtered_data() %>%
      select(name = receiver_label, type = receiver_type)
    
    node_types <- bind_rows(sender_types, receiver_types) %>%
      distinct(name, .keep_all = TRUE)
    
    graph_nodes <- tibble(name = unique(c(df$sender_label, df$receiver_label))) %>%
      left_join(node_types, by = "name") %>%
      mutate(id = name, label = name, group = type)
    
    graph_edges <- df %>%
      rename(from = sender_label, to = receiver_label, weight = value)
    
    g <- graph_from_data_frame(d = graph_edges, vertices = graph_nodes, directed = TRUE)
    
    V(g)$pagerank <- page_rank(g)$vector
    V(g)$betweenness <- betweenness(g)
    V(g)$degree <- degree(g)
    
    # Ego graph for Nadia
    if (!"Nadia Conti" %in% V(g)$name) return(NULL)
    
    ego_graph <- make_ego_graph(g, order = 2, nodes = which(V(g)$name == "Nadia Conti"))[[1]]
    return(ego_graph)
  })
  
  centrality_table_data <- reactive({
    g <- centralitymeasure_df()
    if (is.null(g)) return(NULL)
    
    df <- data.frame(
      label = V(g)$name,
      type = V(g)$group
    )
    
    if (input$centrality_type == "PageRank") {
      df$score <- round(V(g)$pagerank, 4)
    } else if (input$centrality_type == "Betweenness") {
      df$score <- round(V(g)$betweenness, 2)
    } else if (input$centrality_type == "Degree") {
      df$score <- V(g)$degree
    }
    
    df %>% arrange(desc(score))
  })
  
  output$centrality_table <- renderDataTable({
    df <- centrality_table_data()
    if (is.null(df)) return(NULL)
    
    datatable(df, 
              colnames = c("Label", "Type", input$centrality_type),
              caption = paste(input$centrality_type, "Centrality Measure")
    )
  })
  
  # ---- Data Table ----
  output$comm_table <- renderDataTable({
    filtered_data() %>%
      datatable(options = list(pageLength = 10))
  })
  
  # ---- Cronologia ----
  output$cronologia <- renderUI({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(HTML("<p>No messages found for the current filters.</p>"))
    }
    
    df <- df %>%
      mutate(
        timestamp = as.POSIXct(date),  
        timestamp_desc = format(timestamp, "%A, %B %d %Y, %H:%M"),
        content = as.character(content)    # Make sure content is character
      )
    
    create_tml(
      df = df,
      smr = "timestamp_desc",
      dsc = "content"
    )
  })
  
  
}

  
  


# ---- Run App ----
shinyApp(ui = ui, server = server)