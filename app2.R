library(shiny)
library(dplyr)
library(ggplot2)
library(visNetwork)
library(DT)

comm_full <- read.csv("data/communications_full.csv")
print(comm_full)

ui <- fluidPage(
  titlePanel("Temporal Pattern and Communication Network graph"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("week_select", "Select Week(s)", choices = unique(comm_full$week_label), selected = unique(comm_full$week_label), multiple = TRUE),
      dateRangeInput("date_range", "Select Date Range", start = min(comm_full$date), end = max(comm_full$date)),
      sliderInput("hour_range", "Hour of Day", min = 0, max = 23, value = c(6, 18)),
      selectizeInput("sender_entity", "Sender Entity Type", selected = unique(comm_full$sender_type), choices = unique(comm_full$sender_type), multiple = TRUE),
      #checkboxInput("select_all_sender", "Select All Senders", value = TRUE),
      selectizeInput("sender", "Sender", selected = unique(comm_full$sender_label), choices = unique(comm_full$sender_label), multiple = TRUE),
      selectizeInput("receiver_entity", "Receiver Entity Type", selected = unique(comm_full$receiver_type), choices = unique(comm_full$receiver_type), multiple = TRUE),
      selectizeInput("receiver", "Receiver", selected = unique(comm_full$receiver_label), choices = unique(comm_full$receiver_label), multiple = TRUE),
      actionButton("update", "Update View")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Temporal Pattern", plotOutput("time_plot")),
        tabPanel("Heatmap", plotOutput("heatmap_plot")),
        tabPanel("Communication Network", visNetworkOutput("net_plot")),
        tabPanel("Receipts", dataTableOutput("comm_table"))
        
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # Reactive data filtering
  filtered_data <- eventReactive(input$update, {
    comm_full %>%
      filter(
        hour >= input$hour_range[1] & hour <= input$hour_range[2],
        (sender_label %in% input$sender | length(input$sender) == 0),
        (receiver_label %in% input$receiver | length(input$receiver) == 0),
        (sender_type == input$sender_entity | input$sender_entity == ""),
        (receiver_type == input$receiver_entity | input$receiver_entity == ""),
        (week_label %in% input$week_select | length(input$week_select) == 0),
        date >= input$date_range[1] & date <= input$date_range[2]
      )
  })
  
  # ---- Time Series Plot (Daily with Facet by Week) ----
  output$time_plot <- renderPlot({
    df <- filtered_data() %>%
      count(week_label, date)
    
    ggplot(df, aes(x = date, y = n, group = week_label)) +
      geom_line(color = "#0072B2", size = 1) +
      facet_wrap(~ week_label, scales = "free_x", ncol = 1) +
      labs(title = "Daily Communication Volume per Week", x = "Date", y = "Messages") +
      theme_minimal()
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
      theme_minimal()
  })
  
  # ---- Network Graph ----
  output$net_plot <- renderVisNetwork({
    df <- filtered_data() %>%
      filter(!is.na(sender_label), !is.na(receiver_label)) %>%
      count(sender_label, receiver_label, name = "value")  # value used for edge weight
    
    if (nrow(df) == 0) return(NULL)
    
    # Create node list with group for coloring
    graph_nodes <- tibble(name = unique(c(df$sender_label, df$receiver_label))) %>%
      mutate(id = name, label = name, group = name)  # group = name → each node colored uniquely
    
    # Rename edge columns for visNetwork
    graph_edges <- df %>%
      rename(from = sender_label, to = receiver_label)
    
    visNetwork(nodes = graph_nodes, edges = graph_edges) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 123) %>%
      visPhysics(stabilization = TRUE) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visLegend()
  })
  
  # ---- Data Table ----
  output$comm_table <- renderDataTable({
    filtered_data() %>%
      datatable(options = list(pageLength = 10))
  })
}


# ---- Run App ----
shinyApp(ui = ui, server = server)