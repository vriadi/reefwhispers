# ---- LIBRARIES ----
library(shiny)
library(dplyr)
library(visNetwork)
library(ggplot2)
library(DT)
library(tidyr)
library(stringr)
library(tidytext)
library(igraph)
library(lubridate)

# ---- LOAD & PROCESS DATA ----
comm_full <- read.csv("data/communications_full.csv", stringsAsFactors = FALSE)

comm_full <- comm_full %>%
  mutate(
    datetime = ymd_hms(timestamp),
    date = as.Date(datetime),
    hour = hour(datetime),
    week = paste("Week", isoweek(date) - min(isoweek(date)) + 1)
  )

if (!"cluster" %in% colnames(comm_full)) {
  comm_full$cluster <- sample(1:5, nrow(comm_full), replace = TRUE)
}

nodes <- unique(c(comm_full$sender_label, comm_full$receiver_label)) %>%
  data.frame(id = ., label = .) %>%
  mutate(group = "All")

# Create keyword tableMore actions
keywords <- comm_full %>%
  filter(!is.na(content)) %>%
  unnest_tokens(word, content) %>%
  filter(!word %in% stop_words$word) %>%
  count(cluster, word, sort = TRUE) %>%
  group_by(cluster) %>%
  top_n(10, n)

# ---- UI ----
ui <- fluidPage(
  titlePanel(NULL),
  tags$head(
    tags$style(HTML("
      h2 { font-weight: bold; color: #1c1c1c; }
      .tabbable > .nav > li > a { font-weight: bold; }
      .well { background-color: #f7f7f7; border: 1px solid #ddd; }
    "))
  ),
  fluidRow(
    column(
      width = 3,
      wellPanel(
        h4("Global Filters"),
        checkboxGroupInput("weeks", "Select Week(s)", choices = c("Week 1", "Week 2")),
        dateRangeInput("daterange", "Select Date Range:", start = "2040-10-01", end = "2040-10-14"),
        actionButton("update_global", "Update View")  # <-- Added this button
      ),
      br(),
      wellPanel(
        h4("Local Filters"),
        conditionalPanel(
          condition = "$('ul.nav li.active a').text() === 'Communication Clusters'",
          selectizeInput("node_select", "Select Node ID(s):", choices = NULL, multiple = TRUE),
          selectizeInput("cluster_select", "Select Cluster(s):", choices = NULL, multiple = TRUE)
        ),
        conditionalPanel(
          condition = "$('ul.nav li.active a').text() === 'Pseudonyms General Usage' || $('ul.nav li.active a').text() === 'Pseudonym Specific Mentions'",
          sliderInput("threshold", "Threshold for Pseudonym Frequency", min = 0, max = 100, value = 0)
        ),
        conditionalPanel(
          condition = "$('ul.nav li.active a').text() === 'Predominant Topics'",
          selectInput("cluster", "Select Cluster", choices = NULL)
        ),
        actionButton("update", "Update View")
      )
    ),
    column(
      width = 9,
      h2("Communication Clusters and Pseudonyms"),
      p("Identifying closely-associated groups, their predominant topics, and pseudonym usage & exploration."),
      tabsetPanel(
        tabPanel("Communication Clusters", visNetworkOutput("network")),
        tabPanel("Predominant Topics", dataTableOutput("keyword_table")),
        tabPanel("Pseudonyms General Usage", plotOutput("pseudonym_bar")),
        tabPanel("Pseudonym Specific Mentions", plotOutput("pseudonym_heatmap"))
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    df <- comm_full
    if (!is.null(input$weeks) && length(input$weeks) > 0) {
      df <- df %>% filter(week %in% input$weeks)
    }
    df <- df %>% filter(date >= input$daterange[1], date <= input$daterange[2])
    df
  })
  
  observe({
    updateSelectInput(session, "cluster", choices = sort(unique(filtered_data()$cluster)))
    node_choices <- sort(unique(comm_full$sender_label))
    cluster_choices <- sort(unique(paste0("Cluster ", unique(comm_full$cluster))))
    updateSelectizeInput(session, "node_select", choices = c("All", node_choices), server = TRUE)
    updateSelectizeInput(session, "cluster_select", choices = c("All", cluster_choices), server = TRUE)
  })
  
  output$network <- renderVisNetwork({
    df <- filtered_data()
    edge_list <- df %>%
      filter(sender_type %in% c("Person", "Vessel"), receiver_type %in% c("Person", "Vessel")) %>%
      filter(!is.na(sender_label) & !is.na(receiver_label)) %>%
      count(sender_label, receiver_label, name = "weight")
    if (nrow(edge_list) == 0) return(NULL)
    
    graph_comm <- igraph::graph_from_data_frame(edge_list, directed = TRUE)
    clusters <- cluster_walktrap(graph_comm)
    cluster_map <- setNames(paste0("Cluster ", sort(unique(clusters$membership))), sort(unique(clusters$membership)))
    deg_sent <- degree(graph_comm, mode = "out")
    deg_recv <- degree(graph_comm, mode = "in")
    nodes_df <- data.frame(id = V(graph_comm)$name, label = V(graph_comm)$name,
                           title = paste0("🟢 Sent: ", deg_sent, "<br>🔵 Received: ", deg_recv),
                           group = cluster_map[as.character(clusters$membership)],
                           value = deg_sent + deg_recv)
    edges_df <- data.frame(from = as_edgelist(graph_comm)[, 1], to = as_edgelist(graph_comm)[, 2], arrows = "to")

    visNetwork(nodes_df, edges_df) %>%
      visOptions(highlightNearest = TRUE) %>%
      visLegend() %>%
      visPhysics(solver = "forceAtlas2Based", stabilization = TRUE) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visLayout(randomSeed = 123)
  })
  
  observeEvent(input$node_select, {
    if (!"All" %in% input$node_select) {
      visNetworkProxy("network") %>% visSelectNodes(id = input$node_select)
    }
  })
  
  output$keyword_table <- renderDataTable({
    req(input$cluster)
    
    df <- filtered_data()
    
    # Tokenize words and compute frequency by cluster
    keywords <- df %>%
      unnest_tokens(word, content) %>%
      anti_join(stop_words, by = "word") %>%
      count(cluster, word, sort = TRUE)
    
    req(nrow(keywords) > 0)
    req(input$cluster %in% keywords$cluster)
    
    keywords %>%
      filter(cluster == input$cluster) %>%
      arrange(desc(n)) %>%
      mutate(`No.` = row_number()) %>%
      select(`No.`, Word = word, Frequency = n) %>%
      datatable(
        rownames = FALSE,
        options = list(
          pageLength = 10,
          lengthMenu = list(
            c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
            c('10','20','30','40','50','60','70','80','90','100')
          ),
          autoWidth = TRUE,
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          )
        ),
        class = 'stripe hover compact',
        escape = FALSE
      )
  })
  
  output$pseudonym_bar <- renderPlot({
    df <- filtered_data()
    selected_pseudonyms <- c("Mako", "Neptune", "Remora", "Mrs. Money", "The Lookout", "Boss", "The Intern", "The Middleman",
                             "Serenity", "Marlin", "Knowles", "Seawatch", "Osprey", "The Accountant", "Small Fry", "Defender")
    mention_counts <- df %>% pivot_longer(cols = c(sender_label, receiver_label), names_to = "type", values_to = "pseudonym") %>%
      count(pseudonym, name = "count")
    filtered_counts <- mention_counts %>%
      filter(pseudonym %in% selected_pseudonyms & count >= input$threshold) %>%
      arrange(desc(count)) %>%
      mutate(pseudonym = factor(pseudonym, levels = rev(pseudonym)))
    ggplot(filtered_counts, aes(x = pseudonym, y = count, fill = count)) +
      geom_col() +
      geom_text(aes(label = count), hjust = -0.3, size = 4.2) +
      coord_flip() +
      scale_fill_gradient(low = "#deebf7", high = "#08306b") +
      labs(x = "Pseudonym", y = "Frequency") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none", plot.margin = margin(10, 10, 10, 10))
  }, height = 600, width = 900)
  
  output$pseudonym_heatmap <- renderPlot({
    df <- filtered_data()
    selected_pseudonyms <- c("The Lookout", "Mrs. Money", "Small Fry", "The Intern", "Remora", "Osprey", "Defender", "Mako",
                             "Knowles", "Neptune", "The Accountant", "The Middleman", "Marlin", "Boss", "Serenity", "Rodriguez")
    mention_heatmap <- df %>% count(sender = sender_label, pseudonym = receiver_label) %>% complete(sender, pseudonym, fill = list(n = 0)) %>% rename(count = n)
    df_filtered <- mention_heatmap %>%
      filter(pseudonym %in% selected_pseudonyms & count >= input$threshold)
    df_filtered$pseudonym <- factor(df_filtered$pseudonym, levels = selected_pseudonyms)
    ggplot(df_filtered, aes(x = pseudonym, y = sender, fill = count)) +
      geom_tile(color = "white") +
      geom_text(aes(label = ifelse(count > 0, count, "")), size = 3.5) +
      scale_fill_gradient(low = "#e0ecf4", high = "#0868ac") +
      labs(x = "Pseudonym", y = "Sender") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.margin = margin(10, 10, 10, 10), legend.position = "none")
  }, height = 700, width = 1100)
}

# ---- RUN APP ----
shinyApp(ui = ui, server = server)