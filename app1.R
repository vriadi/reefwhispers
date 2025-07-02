library(shiny)
library(shinyWidgets) 
library(dplyr)
library(ggplot2)
library(visNetwork)
library(DT)

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
      # Week selector -- default select-all
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
        tabPanel("Communication Network", visNetworkOutput("net_plot")),
        tabPanel("Receipts", dataTableOutput("comm_table"))
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
      geom_line(color = "#0072B2", size = 1) +
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
      theme_minimal() +
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