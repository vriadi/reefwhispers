# app.R
library(shiny)
library(tidyverse)
library(lubridate)
library(stringr)
library(readxl)
library(janitor)
library(tidytext)
library(stopwords)
library(visNetwork)
library(plotly)

# Load your data
messages <- read_csv("data/communications_full.csv")
df <- read_excel("data/Nadia Conti_copy.xlsx") %>%
  clean_names()

# UI
# Enhanced Shiny App UI & UX - Time Series and Social Network Graph

# UI
ui <- navbarPage(
  "",
  
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
  
  tabPanel("Overview",
           fluidPage(
             fluidRow(
               column(3,
                      wellPanel(
                        checkboxGroupInput("weeks", "Select Week(s)", choices = c("Week 1", "Week 2"), selected = c("Week 1", "Week 2")),
                        dateRangeInput("date_range", "Select Date Range", start = "2040-10-01", end = "2040-10-14"),
                        sliderInput("hour_range", "Hour of Day", min = 0, max = 23, value = c(8, 18)),
                        textInput("sender", "Sender", placeholder = "e.g., Nadia Conti"),
                        textInput("receiver", "Receiver", placeholder = "e.g., Elise"),
                        actionButton("update", "Update View"),      # ✅ <- THIS LINE NEEDS A COMMA if followed by more
                        actionButton("reset", "Reset Filters")      # ✅ <- Make sure this is inside the same function
                      )
               ),
               column(9,
                      tabsetPanel(
                        tabPanel("Temporal Pattern",
                                 fluidRow(
                                   column(6,
                                          div(class = "card",
                                              plotlyOutput("nadiaPie")
                                          )),
                                   column(6,
                                          div(class = "card",
                                              uiOutput("noteBox")
                                          ))
                                 ),
                                 fluidRow(
                                   column(6,
                                          div(class = "card",
                                              plotlyOutput("barHourDay")
                                          )),
                                   column(6,
                                          div(class = "card",
                                              plotlyOutput("mentionPattern")
                                          ))
                                 )
                        ),
                        tabPanel("Direct Relationship Network",
                                 selectInput("selected_node", "Select by ID",
                                             choices = NULL, selected = "Nadia Conti"),
                                 visNetworkOutput("directNet", height = "600px")
                        ),
                        tabPanel("Relationship Network",
                                 visNetworkOutput("relationshipNet", height = "700px")
                        )
                      )
               )
             )
           )
  ),
  
  tabPanel("Keyword Insights",
           fluidPage(
             h4("Top Keywords in Nadia-Related Messages"),
             radioButtons("selected_date", NULL,
                          choices = c("October 8th" = "2040-10-08",
                                      "October 9th" = "2040-10-09",
                                      "October 10th" = "2040-10-10",
                                      "October 11th" = "2040-10-11",
                                      "October 12th" = "2040-10-12"),
                          inline = TRUE),
             selectInput("keyword_category", "Filter by Category",
                         choices = c("All", "Person", "Location", "Construction", "Object", "Time", "Action", "Other"),
                         selected = "All"),
             plotOutput("keywordPlot", height = "600px")
           )
  ),

  tabPanel("Nadia’s Timeline of Events and Operational Focus",
           tabsetPanel(
             tabPanel("Operational focus",
                      plotOutput("focusTimelinePlot"),
                      fluidRow(
                        column(6, plotOutput("eventTypeBarPlot")),
                        column(6, plotOutput("actorHeatmapPlot"))
                      )
             ),
             tabPanel("Evidence Events",
                      plotOutput("evidenceEventPlot", height = "650px")
             )
           )
  )
)

# Server
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    req(input$date_range)
    df <- messages %>%
      mutate(time = ymd_hms(timestamp)) %>%
      filter(between(hour(time), input$hour_range[1], input$hour_range[2])) %>%
      filter(date(time) >= input$date_range[1] & date(time) <= input$date_range[2]) %>%
      filter(sender == "Nadia Conti" | receiver == "Nadia Conti")
    df
  
  })
  
  output$nadiaPie <- renderPlotly({
    nadia_msgs <- filtered_data() %>% filter(sender == "Nadia Conti" | receiver == "Nadia Conti")
    nadia_counts <- nadia_msgs %>%
      summarise(Sent = sum(sender == "Nadia Conti"), Received = sum(receiver == "Nadia Conti")) %>%
      pivot_longer(everything(), names_to = "Type", values_to = "Count") %>%
      mutate(Percent = Count / sum(Count), Label = paste0(round(Percent * 100), "%\n(", Count, " msgs)"))
    plot_ly(nadia_counts, labels = ~Type, values = ~Count, type = 'pie', textinfo = 'label+percent',
            marker = list(colors = c("black", "pink"))) %>%
      layout(title = "Nadia Conti's Messages")
  })
  
  output$noteBox <- renderUI({
    HTML(
      '<div style="background-color:#eadbf7; padding:15px;">
      <h4>\ud83d\udd75\ufe0f Investigation Note</h4>
      <ul>
        <li>Nadia Conti received more than twice the number of messages she sent.</li>
        <li>This suggests she was a central point of contact.</li>
        <li>This may imply influence, authority, or involvement in sensitive operations.</li>
        <li><b>12 messages</b> mention Nadia but are neither sent nor received by her. She is a <b>subject of conversation</b>.</li>
      </ul>
      <p>Continue examining timing and content of these indirect mentions.</p>
    </div>'
    )
  })
  
  output$barHourDay <- renderPlotly({
    nadia_msgs <- filtered_data() %>% filter(sender == "Nadia Conti" | receiver == "Nadia Conti") %>%
      mutate(hour = hour(ymd_hms(timestamp)), day = as.Date(timestamp)) %>% count(day, hour)
    gg <- ggplot(nadia_msgs, aes(x = hour, y = n, fill = factor(day))) +
      geom_col(position = "dodge") + labs(title = "Hourly Activity", x = "Hour", y = "Messages") +
      theme_minimal()
    ggplotly(gg)
  })
  
  output$mentionPattern <- renderPlotly({
    mentions <- filtered_data() %>% filter(str_detect(content, "Nadia")) %>%
      mutate(hour = hour(ymd_hms(timestamp)), day = as.Date(timestamp)) %>% count(day, hour)
    gg <- ggplot(mentions, aes(x = hour, y = n, fill = factor(day))) +
      geom_col(position = "dodge") + labs(title = "Mentions of Nadia by Hour", x = "Hour", y = "Mentions") +
      theme_minimal()
    ggplotly(gg)
  })
  
  observe({
    node_choices <- sort(unique(c(messages$sender_label, messages$receiver_label)))
    updateSelectInput(session, "selected_node", choices = node_choices)
  })
  
# Enhancements included:
# - Tooltips on nodes and edges
# - Node sizing by message volume
# - Edge color mapping by strength
# - Node drag and layout freeze option
# - Better selection filtering in direct network

  # Enhancements included:
  # - Tooltips on nodes and edges
  # - Node sizing by message volume
  # - Edge color mapping by strength
  # - Node drag and layout freeze option
  # - Better selection filtering in direct network
  
  output$directNet <- renderVisNetwork({
    nadia_msgs <- filtered_data()
    edges <- nadia_msgs %>% 
      count(sender_label, receiver_label) %>%
      filter(!is.na(sender_label), !is.na(receiver_label)) %>%
      rename(from = sender_label, to = receiver_label, value = n)
    
    node_info <- nadia_msgs %>%
      pivot_longer(cols = c(sender_label, receiver_label), names_to = "role", values_to = "name") %>%
      count(name, name = "message_count")
    
    nodes <- tibble(name = unique(c(edges$from, edges$to))) %>%
      left_join(node_info, by = "name") %>%
      mutate(id = name, 
             label = name, 
             size = pmin(60, 10 + message_count),
             group = ifelse(name == "Nadia Conti", "Nadia", "Other"),
             title = paste("ID:", name, "<br>Messages:", message_count))
    
    edges <- edges %>% mutate(
      title = paste("Messages:", value),
      color = case_when(
        value > 20 ~ "#08519c",
        value > 10 ~ "#3182bd",
        TRUE ~ "#bdd7e7"
      ),
      width = log1p(value) + 1
    )
    
    visNetwork(nodes, edges) %>%
      visEdges(arrows = "to", smooth = TRUE) %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
        nodesIdSelection = list(enabled = TRUE, selected = input$selected_node)
      ) %>%
      visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE) %>%
      visLayout(randomSeed = 11) %>%
      visLegend()
  })
  
  output$relationshipNet <- renderVisNetwork({
    df2 <- df %>%
      mutate(
        rel_label = str_remove(relationship, "^Relationship_"),
        rel_label = str_remove(rel_label, "_\\d+$")
      )
    
    sender_nodes <- df2 %>%
      distinct(id = sender, label = sender, group = sender_type)
    
    receiver_nodes <- df2 %>%
      distinct(id = receiver, label = receiver, group = receiver_type)
    
    rel_nodes <- df2 %>%
      distinct(id = relationship, label = rel_label, group = "Relationship")
    
    nodes <- bind_rows(sender_nodes, receiver_nodes, rel_nodes) %>%
      distinct(id, .keep_all = TRUE) %>%
      mutate(
        shape = case_when(
          group == "Person" ~ "dot",
          group == "Organization" ~ "ellipse",
          group == "Vessel" ~ "diamond",
          group == "Location" ~ "triangle",
          group == "Relationship" ~ "box",
          TRUE ~ "circle"
        ),
        color = case_when(
          group == "Person" ~ "#6baed6",
          group == "Organization" ~ "#ffd700",
          group == "Vessel" ~ "#fb6a4a",
          group == "Location" ~ "#74c476",
          group == "Relationship" ~ "#d07be5",
          TRUE ~ "#c0c0c0"
        ),
        title = paste0("<b>", label, "</b><br>Type: ", group)
      )
    
    edges <- df2 %>%
      transmute(from = sender, to = relationship) %>%
      bind_rows(df2 %>% transmute(from = relationship, to = receiver))
    
    visNetwork(nodes, edges, height = "700px", width = "100%") %>%
      visEdges(arrows = "to", color = list(color = "#aaa", highlight = "red")) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visPhysics(solver = "forceAtlas2Based", stabilization = TRUE) %>%
      visLayout(randomSeed = 11)
  })
  

  
  # --- Keyword Insights ---
  keyword_categories <- tibble::tibble(
    word = c("nadia", "davis", "elise", "reef", "nemo", "neptune", "marina", "mako", 
             "equipment", "foundation", "scope", "payment", "tonight", "meeting", "modified", "finalize", 
             "documentation", "eastern", "bring"),
    category = c("Person", "Person", "Person", "Location", "Location", "Location", "Location", "Location",
                 "Construction", "Construction", "Construction", "Action", "Time", "Action", "Action", "Action",
                 "Object", "Location", "Action")
  )
  
  output$keywordPlot <- renderPlot({
    req(input$selected_date)
    
    related_msgs <- messages %>%
      filter(str_detect(content, "Nadia", negate = FALSE)) %>%
      filter(as.Date(timestamp) == input$selected_date) %>%
      filter(!is.na(content) & content != "")
    
    keywords <- related_msgs %>%
      unnest_tokens(word, content) %>%
      filter(!word %in% stop_words$word) %>%
      count(word, sort = TRUE) %>%
      slice_max(n, n = 20) %>%
      left_join(keyword_categories, by = "word") %>%
      mutate(category = ifelse(is.na(category), "Other", category)) %>%
      filter(input$keyword_category == "All" | category == input$keyword_category)
    
    ggplot(keywords, aes(x = reorder(word, n), y = n, fill = category)) +
      geom_col() +
      coord_flip() +
      labs(
        title = paste("Top Keywords in Nadia-Related Messages (", input$selected_date, ")"),
        x = "Keyword", y = "Frequency"
      ) +
      theme_minimal()
  })
  
  
  # Timeline of Operational Focus
  output$focusTimelinePlot <- renderPlot({
    df_focus <- data.frame(
      Date = factor(c("Oct 8", "Oct 9", "Oct 10", "Oct 11", "Oct 12"),
                    levels = c("Oct 8", "Oct 9", "Oct 10", "Oct 11", "Oct 12")),
      Focus = c("Execution Planning", "Escalation Response", "Surveillance & Legal Framing",
                "Disruption & Realignment", "Administrative Closure")
    )
    ggplot(df_focus, aes(x = Date, y = 1, text = Focus)) +
      geom_line(linewidth = 1.2, color = "steelblue") +
      geom_point(size = 4, color = "steelblue") +
      ylim(0.95, 1.05) +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()
      ) +
      geom_text(aes(label = Focus), vjust = -1.5, angle = 20, size = 3.5) +
      labs(title = "Timeline of Nadia-Linked Operational Focus (Oct 8–12, 2040)")
  })
  
  # Nadia Event Classification
  event_keywords <- list(
    Permit = c("permit", "approval", "cr-7844", "documentation", "expedite", "sign[- ]?off"),
    Construction = c("underwater foundation", "equipment", "build", "structure", "concrete"),
    Patrol = c("patrol", "reroute", "surveillance", "harbor master", "schedule", "redirect"),
    Coordination = c("meet", "confirm", "setup", "staff", "shift", "manifest"),
    Bribery = c("payment", "fee", "double", "favor", "compensation"),
    CoverUp = c("destroy", "cancel", "unaware", "discreet", "nothing to be concerned")
  )
  
  classify_event_primary <- function(text) {
    for (type in names(event_keywords)) {
      pattern <- paste(event_keywords[[type]], collapse = "|")
      if (str_detect(tolower(text), pattern)) return(type)
    }
    return("General")
  }
  
  # Bar plot
  output$eventTypeBarPlot <- renderPlot({
    nadia_related <- messages %>%
      filter(str_detect(content, "Nadia", negate = FALSE)) %>%
      mutate(Primary_Event = sapply(content, classify_event_primary),
             timestamp = ymd_hms(timestamp))
    
    ggplot(nadia_related, aes(x = as.Date(timestamp), fill = Primary_Event)) +
      geom_histogram(binwidth = 1, color = "black", position = "stack") +
      scale_fill_brewer(palette = "Set2") +
      labs(title = "Nadia Conti’s Timeline of Event Types",
           x = "Time", y = "Number of Messages") +
      theme_minimal() +
      theme(
        legend.title = element_blank(),
        legend.position = "right",
        plot.title = element_text(size = 14, face = "bold")
      )
  })
  
  # Heatmap
  output$actorHeatmapPlot <- renderPlot({
    nadia_related <- messages %>%
      filter(str_detect(content, "Nadia", negate = FALSE)) %>%
      mutate(Primary_Event = sapply(content, classify_event_primary))
    
    nadia_actor_events <- nadia_related %>%
      select(sender, receiver, Primary_Event) %>%
      pivot_longer(cols = c(sender, receiver), names_to = "role", values_to = "actor") %>%
      group_by(actor, Primary_Event) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(actor) %>%
      filter(sum(n) >= 2) %>%
      ungroup()
    
    ggplot(nadia_actor_events, aes(x = Primary_Event, y = fct_reorder(actor, -n), fill = n)) +
      geom_tile(color = "white", linewidth = 0.3) +
      scale_fill_gradient(low = "white", high = "#cc0000") +
      labs(
        title = "Actors Involved in Nadia-Related Messages by Event Type",
        x = "Event Type", y = "Actor", fill = "Message Count"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold")
      )
  })
  
  output$evidenceEventPlot <- renderPlot({
    event_data <- tibble::tibble(
      Date = as.Date(c("2040-10-04", "2040-10-05", "2040-10-05", "2040-10-08",
                       "2040-10-09", "2040-10-09", NA, NA)),
      Event_ID = c("Monitoring_266", "Assessment_337", "Access_Cancellation",
                   "VesselMovement_549", "Monitoring_631", "Assessment_600",
                   "Monitoring_534", "Monitoring_722"),
      Evidence_Type = c("Surveillance", "Assessment", "Administrative",
                        "Movement", "Surveillance", "Assessment",
                        "Patrol", "Patrol"),
      Involved = c("Sentinel & Nadia", "Davis & Rodriguez", "Nadia",
                   "Elise & Nadia", "Elise & Nadia", "Elise & Nadia",
                   "Liam & Nadia", "Elise & Nadia"),
      Findings = c(
        "Vessel 'Mako' detected near protected boundary before signal loss",
        "Unusual vessel traffic near Nemo Reef",
        "Nadia canceled corridor access for Nemo Reef",
        "Movement to Nemo Reef",
        "Increased vessel activity at Nemo Reef",
        "Underwater construction confirmed with concrete forms",
        "Patrol schedules modified in favor of sender/recipient",
        "Nothing found at Nemo Reef"
      )
    ) %>%
      mutate(Date_Label = ifelse(is.na(Date), "Timestamp: NULL", as.character(Date)),
             Date_Factor = forcats::fct_inorder(Date_Label))
    
    ggplot(event_data, aes(x = Date_Factor, y = forcats::fct_rev(Evidence_Type), color = Involved)) +
      geom_point(size = 4) +
      geom_text(aes(label = Event_ID), hjust = -0.1, size = 3.2) +
      ggrepel::geom_text_repel(aes(label = Findings),
                               size = 3, max.overlaps = 20,
                               direction = "y", box.padding = 0.5,
                               segment.color = "gray60", segment.size = 0.3) +
      labs(
        title = "Timeline of Evidence Events Involving Nadia",
        x = "Date / Timestamp", y = "Evidence Type"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  
}

# Run the app
shinyApp(ui, server)









#------------------------ APP 2 ---------------------------
#------------------------ APP 2 ---------------------------
#------------------------ APP 2 ---------------------------
#------------------------ APP 2 ---------------------------
#------------------------ APP 2 ---------------------------
#------------------------ APP 2 ---------------------------
#------------------------ APP 2 ---------------------------



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
      h2(""),
      p(""),
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
    keywords %>%
      filter(cluster == input$cluster) %>%
      arrange(desc(n)) %>%
      mutate(`No.` = row_number()) %>%
      select(`No.`, Word = word, Frequency = n) %>%
      datatable(
        rownames = FALSE,
        options = list(
          pageLength = 10,
          lengthMenu = list(c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100), c('10','20','30','40','50','60','70','80','90','100')),
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