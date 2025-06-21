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

# Load your data
messages <- read_csv("/Users/summernguyen/Documents/summernguyenn/reefwhispers/data/communications_full.csv")
df <- read_excel("/Users/summernguyen/Documents/summernguyenn/ISSS608-VAA/Take-home_Ex/Take-home_Ex02/data/Nadia Conti_copy.xlsx") %>%
  clean_names()

# UI
ui <- navbarPage(
  "Time Series Analysis and Social Network Graph",
  
  tabPanel("Overview",
           sidebarLayout(
             sidebarPanel(
               h5("Select Week(s)"),
               checkboxGroupInput("weeks", NULL, choices = c("Week 1", "Week 2"), selected = c("Week 1", "Week 2")),
               
               h5("Select Date Range"),
               dateRangeInput("date_range", NULL, start = "2040-10-01", end = "2040-10-14"),
               
               h5("Hour of Day"),
               sliderInput("hour_range", NULL, min = 0, max = 23, value = c(8, 18)),
               
               textInput("sender_type", "Sender Entity Type", ""),
               textInput("sender", "Sender", ""),
               textInput("receiver_type", "Receiver Entity Type", ""),
               textInput("receiver", "Receiver", ""),
               
               actionButton("update", "Update View")
             ),
             
             mainPanel(
               tabsetPanel(
                 tabPanel("Temporal Pattern",
                          fluidRow(
                            column(6, plotOutput("nadiaPie")),
                            column(6, div(style = "background-color:#eadbf7; padding: 15px;",
                                          h4("🕵️ Investigation Note"),
                                          tags$ul(
                                            tags$li("Nadia Conti received more than twice the number of messages she sent."),
                                            tags$li("This suggests she was a central point of contact."),
                                            tags$li("This may imply influence, authority, or involvement in sensitive operations.")
                                          ),
                                          tags$ul(
                                            tags$li(HTML("12 messages mention Nadia but are neither sent nor received by her. This means she is a <b>subject of conversation</b> even when not directly involved."))
                                          ),
                                          p("Continue examining timing and content of these indirect mentions.")
                            ))
                          ),
                          fluidRow(
                            column(6, plotOutput("barHourDay")),
                            column(6, plotOutput("mentionPattern"))
                          )
                 ),
                 
                 tabPanel("Direct Relationship Network",
                          h4("Nadia’s Relationship Network Graph"),
                          selectInput("selected_node", "Select by ID", choices = NULL),
                          visNetworkOutput("directNet", height = "600px")
                 ),
                 
                 tabPanel("Relationship Network",
                          h4("Nadia’s entire structure of interactions"),
                          visNetworkOutput("relationshipNet", height = "700px")
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
                          inline = TRUE
             ),
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
      filter(date(time) >= input$date_range[1] & date(time) <= input$date_range[2])
    
    if (input$sender != "") df <- df %>% filter(sender == input$sender)
    if (input$receiver != "") df <- df %>% filter(receiver == input$receiver)
    
    df
  })
  
  output$nadiaPie <- renderPlot({
    nadia_msgs <- filtered_data() %>%
      filter(sender == "Nadia Conti" | receiver == "Nadia Conti")
    
    nadia_counts <- nadia_msgs %>%
      summarise(Sent = sum(sender == "Nadia Conti"), Received = sum(receiver == "Nadia Conti")) %>%
      pivot_longer(everything(), names_to = "Type", values_to = "Count") %>%
      mutate(Percent = Count / sum(Count),
             Label = paste0(round(Percent * 100), "%\n(", Count, " msgs)"))
    
    ggplot(nadia_counts, aes(x = "", y = Count, fill = Type)) +
      geom_col(width = 1, color = "black") +
      coord_polar(theta = "y") +
      geom_text(aes(label = Label), position = position_stack(vjust = 0.5)) +
      labs(title = "Nadia Conti's Messages") +
      scale_fill_manual(values = c("Sent" = "yellow", "Received" = "green")) +
      theme_void()
  })
  
  output$barHourDay <- renderPlot({
    nadia_msgs <- filtered_data() %>%
      filter(sender == "Nadia Conti" | receiver == "Nadia Conti") %>%
      mutate(hour = hour(ymd_hms(timestamp)), day = as.Date(timestamp))
    
    nadia_msgs %>%
      count(day, hour) %>%
      ggplot(aes(x = hour, y = n, fill = factor(day))) +
      geom_col(position = "dodge") +
      labs(title = "Hourly Activity", x = "Hour", y = "Messages") +
      theme_minimal()
  })
  
  output$mentionPattern <- renderPlot({
    mentions_nadia <- filtered_data() %>%
      filter(str_detect(content, "Nadia", negate = FALSE)) %>%
      mutate(hour = hour(ymd_hms(timestamp)), day = as.Date(timestamp))
    
    mentions_nadia %>%
      count(day, hour) %>%
      ggplot(aes(x = hour, y = n, fill = factor(day))) +
      geom_col(position = "dodge") +
      labs(title = "Mentions of Nadia by Hour", x = "Hour", y = "Mentions") +
      theme_minimal()
  })
  
  # Update Direct Network Selection
  observe({
    node_choices <- sort(unique(c(messages$sender_label, messages$receiver_label)))
    updateSelectInput(session, "selected_node", choices = node_choices)
  })
  
  output$directNet <- renderVisNetwork({
    nadia_msgs <- filtered_data()
    edges <- nadia_msgs %>%
      count(sender_label, receiver_label) %>%
      filter(!is.na(sender_label), !is.na(receiver_label)) %>%
      rename(from = sender_label, to = receiver_label, value = n)
    
    nodes <- tibble(name = unique(c(edges$from, edges$to))) %>%
      mutate(id = name, label = name, group = ifelse(name == "Nadia Conti", "Nadia", "Other"))
    
    visNetwork(nodes, edges) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = list(enabled = TRUE, selected = input$selected_node)) %>%
      visLegend()
  })
  
  output$relationshipNet <- renderVisNetwork({
    df2 <- df %>%
      mutate(rel_label = str_remove(relationship, "^Relationship_"),
             rel_label = str_remove(rel_label, "_\\d+$"))
    
    sender_nodes <- df2 %>% distinct(id = sender, label = sender, group = sender_type)
    receiver_nodes <- df2 %>% distinct(id = receiver, label = receiver, group = receiver_type)
    rel_nodes <- df2 %>% distinct(id = relationship, label = rel_label, group = "Relationship")
    
    nodes <- bind_rows(sender_nodes, receiver_nodes, rel_nodes) %>% distinct(id, .keep_all = TRUE)
    edges <- df2 %>%
      transmute(from = sender, to = relationship) %>%
      bind_rows(df2 %>% transmute(from = relationship, to = receiver))
    
    visNetwork(nodes, edges) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLegend()
  })
  
  # --- Keyword Insights ---
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
      slice_max(n, n = 20)
    
    ggplot(keywords, aes(x = reorder(word, n), y = n)) +
      geom_col(fill = "#2171b5") +
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
    
    ggplot(df_focus, aes(x = Date, y = 1)) +
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
      ggtitle("Timeline of Nadia-Linked Operational Focus (Oct 8–12, 2040)") +
      geom_text(aes(label = Focus, vjust = -1.5, angle = 20, size = 3.5))
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
      labs(title = "Nadia Conti’s Timeline of Event Types",
           x = "Time", y = "Number of Messages") +
      theme_minimal() +
      theme(legend.position = "right")
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
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "#cc0000") +
      labs(
        title = "Actors Involved in Nadia-Related Messages by Event Type",
        x = "Event Type", y = "Actor", fill = "Message Count"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
