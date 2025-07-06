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
library(shinyWidgets)

# ---- LOAD & PROCESS DATA ----
comm_full <- read.csv("data/communications_full.csv") |>
  mutate(date = as.Date(date))             # make sure it’s Date, not character
valid_dates <- sort(unique(comm_full$date))

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

data("stop_words")

# ---- UI ----
ui <- fluidPage(
  titlePanel(NULL),
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
  fluidRow(
    column(
      width = 3,
      wellPanel(
        h4("Global Filters"),
        checkboxGroupInput("weeks", "Select Week(s)", choices = c("Week 1", "Week 2")),
        airDatepickerInput(
          inputId = "daterange",
          label = "Select Date Range",
          range = TRUE,
          value = c(min(valid_dates), max(valid_dates)),
          minDate = min(valid_dates),
          maxDate = max(valid_dates),
          disabledDates = setdiff(
            seq(min(valid_dates), max(valid_dates), by = "day"),
            valid_dates
          )
        ),
        actionButton("update_global", "Update View")  # <-- Added this button
      ),
      br(),
      wellPanel(
        h4("Local Filters"),
        conditionalPanel(
          condition = "$('ul.nav li.active a').text() === 'Communication Clusters'",
          selectInput("cluster_algo", "Select Clustering Algorithm",
                      choices = c("Walktrap", "Louvain", "Infomap", "Label Propagation"),
                      selected = "Walktrap"),
          selectizeInput("cluster_select", "Select Cluster(s)", choices = NULL, multiple = TRUE),
          selectizeInput("node_select", "Select Node ID(s)", choices = NULL, multiple = TRUE)
          
          
        ),
        conditionalPanel(
          condition = "$('ul.nav li.active a').text() === 'Pseudonyms General Usage' || $('ul.nav li.active a').text() === 'Pseudonym Specific Mentions'",
          sliderInput("threshold", "Threshold for Pseudonym Frequency", min = 0, max = 100, value = 0)
        ),
        conditionalPanel(
          condition = "$('ul.nav li.active a').text() === 'Predominant Topics'",
          selectInput("clustering_algo_predtopic", "Select Clustering Algorithm",
                      choices = c("Walktrap", "Louvain", "Infomap", "Label Propagation"),
                      selected = "Walktrap"),
          selectInput("cluster", "Select Cluster", choices = NULL)
        ),
        actionButton("update", "Update View")
      )
    ),
    column(
      width = 9,
      h2(""),
      p(""),
      tabsetPanel(
        id = "tabs", 
        tabPanel("Communication Clusters", 
                 h4("Cluster Graph"),
                 textOutput("selected_algo"),
                 visNetworkOutput("network"),
                 br(),
                 h4("Cluster Members"),
                 dataTableOutput("cluster_members_table")),
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
    req(filtered_data())
    
    updateSelectInput(session, "cluster", choices = sort(unique(filtered_data()$cluster)))
    
    req(input$tabs)  # <- Add this line to guard the if-statement
    
    if (input$tabs == "Communication Clusters") {
      node_choices <- sort(unique(filtered_data()$sender_label))
      cluster_choices <- sort(unique(paste0("Cluster ", filtered_data()$cluster)))
      updateSelectizeInput(session, "node_select", choices = c("All", node_choices), server = TRUE)
      updateSelectizeInput(session, "cluster_select", choices = c("All", cluster_choices), server = TRUE)}
      
    })
  
  observeEvent({
    input$tabs
    filtered_data()
  }, {
    if (input$tabs == "Predominant Topics") {
      cluster_choices <- sort(unique(filtered_data()$cluster))
      updateSelectInput(session, "cluster", choices = cluster_choices)
    }
  })
  
  compute_clusters <- function(graph, algorithm = "Walktrap") {
    switch(algorithm,
           "Walktrap" = cluster_walktrap(graph),
           "Louvain" = cluster_louvain(as.undirected(graph)),
           "Infomap" = cluster_infomap(graph),
           "Label Propagation" = cluster_label_prop(graph),
           cluster_walktrap(graph)  # default
    )
  }
  
  output$network <- renderVisNetwork({
    df <- filtered_data()
    req(df)
    algo <- input$cluster_algo %||% "Walktrap"  
    edge_list <- df %>%
      filter(sender_type %in% c("Person", "Vessel"), receiver_type %in% c("Person", "Vessel")) %>%
      filter(!is.na(sender_label) & !is.na(receiver_label)) %>%
      count(sender_label, receiver_label, name = "weight")
    
    if (nrow(edge_list) == 0) return(NULL)
    
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
    
    graph_comm <- igraph::graph_from_data_frame(edge_list, directed = TRUE)
    if (vcount(graph_comm) == 0) return(NULL)  # stop here if graph has no nodes
    
    clusters <- compute_clusters(graph_comm, algo)
    
    node_ids <- V(graph_comm)$name
    cluster_membership <- clusters$membership
    names(cluster_membership) <- V(graph_comm)$name
    
    #print(node_ids)
    #print(head(cluster_membership))
    #print(names(cluster_membership))
    
    deg_sent_full <- degree(graph_comm, mode = "out")
    deg_recv_full <- degree(graph_comm, mode = "in")
    
    # Safe degrees
    deg_sent_all <- degree(graph_comm, mode = "out")
    deg_recv_all <- degree(graph_comm, mode = "in")
    
    # Align degrees to node_ids
    deg_sent <- deg_sent_all[node_ids]
    deg_recv <- deg_recv_all[node_ids]
    
    # Replace NAs with 0
    deg_sent[is.na(deg_sent)] <- 0
    deg_recv[is.na(deg_recv)] <- 0
    
    # Cluster group labels
    cluster_labels <- cluster_membership[node_ids]
    group_labels <- paste0("Cluster ", cluster_labels)
    
    #print(group_labels)
    #print(cluster_membership)
    # FOR DEBUGGING
    #cat("Lengths - node_ids:", length(node_ids), 
    #    "deg_sent:", length(deg_sent), 
    #    "deg_recv:", length(deg_recv), 
    #    "cluster_membership:", length(cluster_membership), "\n")
    
    
    # Create nodes_df safely
    nodes_df <- tibble(
      id = node_ids,
      label = node_ids,
      title = paste0("🟢 Sent: ", deg_sent, "<br>🔵 Received: ", deg_recv),
      group = group_labels,
      value = deg_sent + deg_recv
    ) %>%
      left_join(node_types, by = c("id" = "name")) %>%
      mutate(
        shape = shape_map[type] %||% "dot"
      )
    
    selected_clusters <- input$cluster_select
    selected_nodes <- input$node_select
    
    if (!is.null(selected_clusters) && !("All" %in% selected_clusters)) {
      nodes_df <- nodes_df %>% filter(group %in% selected_clusters)
    }
    
    if (!is.null(selected_nodes) && !("All" %in% selected_nodes)) {
      nodes_df <- nodes_df %>% filter(id %in% selected_nodes)
    }
    
    edges_df <- edge_list %>%
      filter(sender_label %in% nodes_df$id & receiver_label %in% nodes_df$id) %>%
      rename(from = sender_label, to = receiver_label)
  
    # Define legend
    type_legend_nodes <- nodes_df %>%
      filter(!is.na(shape), !is.na(type)) %>%
      distinct(type, shape) %>%
      mutate(
        label = type,                  # e.g. "Person", "Vessel"
        color = "gray"                 # color for shapes in legend (neutral)
      ) %>%
      select(label, shape, color) %>%
      purrr::transpose()

    # visNetwork
    visNetwork(nodes_df, edges_df) %>%
      visOptions(highlightNearest = TRUE) %>%
      visLegend(addNodes = type_legend_nodes, useGroups = TRUE, position = "left") %>%
      visPhysics(solver = "forceAtlas2Based", stabilization = TRUE) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visLayout(randomSeed = 123)
  })
  
  output$cluster_members_table <- renderDataTable({
    df <- filtered_data()
    req(df)
    algo <- input$cluster_algo %||% "Walktrap"  
    edge_list <- df %>%
      filter(sender_type %in% c("Person", "Vessel"), receiver_type %in% c("Person", "Vessel")) %>%
      filter(!is.na(sender_label) & !is.na(receiver_label)) %>%
      count(sender_label, receiver_label, name = "weight")
    
    if (nrow(edge_list) == 0) return(NULL)
    
    # Get entity types from filtered data
    sender_types <- filtered_data() %>%
      select(name = sender_label, type = sender_type)
    
    receiver_types <- filtered_data() %>%
      select(name = receiver_label, type = receiver_type)
    
    node_types <- bind_rows(sender_types, receiver_types) %>%
      distinct(name, .keep_all = TRUE)

    
    graph_comm <- igraph::graph_from_data_frame(edge_list, directed = TRUE)
    if (vcount(graph_comm) == 0) return(NULL)  # stop here if graph has no nodes
    
    clusters <- compute_clusters(graph_comm, algo)
    
    node_ids <- V(graph_comm)$name
    cluster_membership <- clusters$membership
    names(cluster_membership) <- V(graph_comm)$name
    
    #print(node_ids)
    #print(head(cluster_membership))
    #print(names(cluster_membership))
    
    deg_sent_full <- degree(graph_comm, mode = "out")
    deg_recv_full <- degree(graph_comm, mode = "in")
    
    # Safe degrees
    deg_sent_all <- degree(graph_comm, mode = "out")
    deg_recv_all <- degree(graph_comm, mode = "in")
    
    # Align degrees to node_ids
    deg_sent <- deg_sent_all[node_ids]
    deg_recv <- deg_recv_all[node_ids]
    
    # Replace NAs with 0
    deg_sent[is.na(deg_sent)] <- 0
    deg_recv[is.na(deg_recv)] <- 0
    
    # Cluster group labels
    cluster_labels <- cluster_membership[node_ids]
    group_labels <- paste0("Cluster ", cluster_labels)
    
    #print(group_labels)
    #print(cluster_membership)
    # FOR DEBUGGING
    #cat("Lengths - node_ids:", length(node_ids), 
    #    "deg_sent:", length(deg_sent), 
    #    "deg_recv:", length(deg_recv), 
    #    "cluster_membership:", length(cluster_membership), "\n")
    
    
    # Create nodes_df safely
    nodes_df <- tibble(
      id = node_ids,
      label = node_ids,
      title = paste0("🟢 Sent: ", deg_sent, "<br>🔵 Received: ", deg_recv),
      group = group_labels,
      value = deg_sent + deg_recv
    ) %>%
      left_join(node_types, by = c("id" = "name"))
    
    selected_clusters <- input$cluster_select
    selected_nodes <- input$node_select
    
    if (!is.null(selected_clusters) && !("All" %in% selected_clusters)) {
      nodes_df <- nodes_df %>% filter(group %in% selected_clusters)
    }
    
    if (!is.null(selected_nodes) && !("All" %in% selected_nodes)) {
      nodes_df <- nodes_df %>% filter(id %in% selected_nodes)
    }
    
    # print(nodes_df)
    
    nodes_df %>%
      group_by(Cluster = group) %>%
      summarise(Members = paste(sort(unique(id)), collapse = ", ")) %>%
      arrange(Cluster) %>%
      datatable(
        rownames = FALSE,
        options = list(
          pageLength = 5,
          autoWidth = TRUE,
          columnDefs = list(list(className = 'dt-left', targets = "_all"))
        ),
        class = 'stripe hover compact',
        escape = FALSE
      )
  })
  
  observe({
    #cat("DEBUG >>> Running observe()\n")
    df <- filtered_data()
    #cat("DEBUG >>> Filtered rows: ", nrow(df), "\n")
    if (nrow(df) > 0) {
      #cat("DEBUG >>> Sample content:\n")
      #print(head(df$content, 3))
    }
  })
  
  output$keyword_table <- renderDataTable({
    #cat("DEBUG >>> Starting keyword_table render\n")
    
    df <- filtered_data()
    if (nrow(df) == 0) {
      #cat("DEBUG >>> filtered_data() is empty\n")
      return(NULL)
    }
    
    # Create edge list
    edges <- df %>%
      filter(!is.na(sender_label) & !is.na(receiver_label)) %>%
      count(sender_label, receiver_label, name = "weight")
    #cat("DEBUG >>> Number of edges: ", nrow(edges), "\n")
    if (nrow(edges) == 0) return(NULL)
    
    # Create graph
    g <- igraph::graph_from_data_frame(edges, directed = TRUE)
    
    # Ensure sender_label and receiver_label are characters
    edges <- edges %>%
      mutate(sender_label = as.character(sender_label),
             receiver_label = as.character(receiver_label))
    
    # Ensure names are correctly set
    V(g)$name <- V(g)$name  # This should already be correct, but ensure it's set
    #cat("DEBUG >>> Vertex names:\n")
    #print(V(g)$name)
    
    # Ensure names are correctly set
    V(g)$name <- V(g)$name  # This should already be correct, but ensure it's set
    #cat("DEBUG >>> Vertex names:\n")
    #print(V(g)$name)
    
    # Select clustering algorithm
    algo <- input$clustering_algo_predtopic
    #cat("DEBUG >>> Selected algo: ", algo, "\n")
    
    clusters <- switch(algo,
                       "Walktrap" = cluster_walktrap(g),
                       "Louvain" = cluster_louvain(as.undirected(g)),
                       "Infomap" = cluster_infomap(g),
                       "Label Propagation" = cluster_label_prop(g),
                       cluster_walktrap(g)
    )
    
    cluster_membership <- clusters$membership
    if (is.null(cluster_membership)) {
      #cat("DEBUG >>> Clustering failed: membership is NULL\n")
      return(NULL)
    }
    
    #cat("DEBUG >>> Cluster count: ", length(unique(cluster_membership)), "\n")
    
    # Group text by sender
    df_grouped <- df %>%
      filter(!is.na(content)) %>%
      group_by(sender_label) %>%
      summarise(text = paste(content, collapse = " "), .groups = "drop")
    
    # Assign cluster membership
    name_map <- cluster_membership
    names(name_map) <- V(g)$name
    
    df_grouped$cluster <- name_map[df_grouped$sender_label]
    
    # Check for NAs here again
    #cat("DEBUG >>> After mapping cluster membership:\n")
    #print(df_grouped %>% select(sender_label, cluster))
    
    missing <- df_grouped$sender_label[is.na(df_grouped$cluster)]
    #cat("DEBUG >>> Sender labels missing cluster assignment:\n")
    #print(missing)
    
    # Tokenize and filter
    keywords <- df_grouped %>%
      unnest_tokens(word, text) %>%
      filter(!word %in% stop_words$word) %>%
      count(cluster, word, sort = TRUE) %>%
      group_by(cluster) %>%
      #slice_max(order_by = n, n = 100) %>%
      ungroup()
    
    #print(keywords)
    
    #cat("DEBUG >>> Keywords found: ", nrow(keywords), "\n")
    
    # Check selected cluster input
    selected_cluster <- as.numeric(input$cluster)
    
    if (is.na(selected_cluster)) {
      cat("DEBUG >>> No cluster selected\n")
      return(NULL)
    }
    
    final_df <- keywords %>%
      filter(cluster == selected_cluster) %>%
      rename(
        Cluster = cluster,
        Word = word,
        Frequency = n
      )
    
    if (nrow(final_df) == 0) {
      cat("DEBUG >>> No rows for selected cluster\n")
      return(NULL)
    }
    
    #cat("DEBUG >>> Final table rows: ", nrow(final_df), "\n")
    if (nrow(final_df) == 0) return(NULL)
    
    # Render
    datatable(
      final_df,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")  # Center-align all columns
        )
      ),
      class = 'stripe hover compact',
      escape = FALSE
    )
  })
  
  
  
  
  
  output$pseudonym_bar <- renderPlot({
    df <- filtered_data()
    req(df)
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














