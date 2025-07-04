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
