library(shiny)
library(tidyverse)
library(lubridate)
library(stringr)
library(readxl)
library(janitor)
library(visNetwork)
library(plotly)

# Load data
messages <- read_csv("data/communications_full.csv")
df <- read_excel("data/Nadia Conti_copy.xlsx") %>% clean_names()

# UI
ui <- fluidPage(
  titlePanel("Overview - Nadia's Messages Activity and Relationship Network"),
  fluidRow(
    column(3, wellPanel(
      checkboxGroupInput("weeks", "Select Week(s)",
                         choices = c("Week 1", "Week 2"),
                         selected = c("Week 1", "Week 2")),
      dateRangeInput("date_range", "Select Date Range",
                     start = "2040-10-01", end = "2040-10-14"),
      sliderInput("hour_range", "Hour of Day",
                  min = 0, max = 23, value = c(8, 18)),
      actionButton("update", "Update View"),
      actionButton("reset", "Reset Filters")
    )),
    column(9, tabsetPanel(
      tabPanel("Temporal Pattern", 
               fluidRow(
                 column(6, plotlyOutput("nadiaPie")),
                 column(6, uiOutput("noteBox"))
               ),
               fluidRow(
                 column(6, plotlyOutput("barHourDay")),
                 column(6, plotlyOutput("mentionPattern"))
               )
      ),
      tabPanel("Direct Relationship Network", 
               visNetworkOutput("directNet", height = "600px")
      ),
      tabPanel("Relationship Network",
               selectInput("selected_node", NULL, choices = NULL, width = "200px"),
               visNetworkOutput("relationshipNet", height = "700px")
      )
    ))
  )
)

# Server
server <- function(input, output, session) {
  
  # Data filtering reactive
  filtered_data <- reactive({
    req(input$date_range)
    messages %>%
      mutate(time = ymd_hms(timestamp)) %>%
      filter(between(hour(time), input$hour_range[1], input$hour_range[2])) %>%
      filter(date(time) >= input$date_range[1] & date(time) <= input$date_range[2]) %>%
      filter(sender == "Nadia Conti" | receiver == "Nadia Conti")
  })
  
  # Temporal Pattern Plots
  output$nadiaPie <- renderPlotly({
    nadia_msgs <- filtered_data()
    nadia_counts <- nadia_msgs %>%
      summarise(Sent = sum(sender == "Nadia Conti"), 
                Received = sum(receiver == "Nadia Conti")) %>%
      pivot_longer(everything(), names_to = "Type", values_to = "Count") %>%
      mutate(Percent = Count / sum(Count), 
             Label = paste0(round(Percent * 100), "%\n(", Count, " msgs)"))
    
    plot_ly(nadia_counts, labels = ~Type, values = ~Count, type = 'pie',
            textinfo = 'label+percent',
            marker = list(colors = c("black", "pink"))) %>%
      layout(title = "Nadia Conti's Messages")
  })
  
  output$noteBox <- renderUI({
    HTML(
      '<div style="background-color:#eadbf7; padding:15px;">
        <h4>🕵️ Investigation Note</h4>
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
    nadia_msgs <- filtered_data() %>%
      mutate(hour = hour(ymd_hms(timestamp)), day = as.Date(timestamp)) %>%
      count(day, hour)
    
    gg <- ggplot(nadia_msgs, aes(x = hour, y = n, fill = factor(day))) +
      geom_col(position = "dodge") +
      labs(title = "Hourly Activity", x = "Hour", y = "Messages") +
      theme_minimal()
    
    ggplotly(gg)
  })
  
  output$mentionPattern <- renderPlotly({
    mentions <- filtered_data() %>%
      filter(str_detect(content, "Nadia")) %>%
      mutate(hour = hour(ymd_hms(timestamp)), day = as.Date(timestamp)) %>%
      count(day, hour)
    
    gg <- ggplot(mentions, aes(x = hour, y = n, fill = factor(day))) +
      geom_col(position = "dodge") +
      labs(title = "Mentions of Nadia by Hour", x = "Hour", y = "Mentions") +
      theme_minimal()
    
    ggplotly(gg)
  })
  
  # Direct Relationship Network Tab
  output$directNet <- renderVisNetwork({
    nadia_msgs <- filtered_data()
    
    nadia_edges <- nadia_msgs %>%
      count(sender_label, receiver_label) %>%
      filter(!is.na(sender_label), !is.na(receiver_label)) %>%
      rename(from = sender_label, to = receiver_label, value = n)
    
    entity_info <- nadia_msgs %>%
      select(name = sender_label, type = sender_type) %>%
      bind_rows(nadia_msgs %>% select(name = receiver_label, type = receiver_type)) %>%
      distinct()
    
    nadia_nodes <- tibble(name = unique(c(nadia_edges$from, nadia_edges$to))) %>%
      left_join(entity_info, by = "name") %>% 
      mutate(
        group = ifelse(name == "Nadia Conti", "Nadia Conti", type),
        id = name,
        label = name
      ) %>%
      replace_na(list(group = "Unknown"))
    
    visNetwork(nodes = nadia_nodes, edges = nadia_edges) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE) %>%
      visLayout(randomSeed = 123) %>%
      visPhysics(stabilization = TRUE) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visLegend()
  })
  
  # Relationship Network Tab
  output$relationshipNet <- renderVisNetwork({
    df2 <- df %>%
      mutate(
        rel_label = str_remove(relationship, "^Relationship_"),
        rel_label = str_remove(rel_label, "_\\d+$")
      )
    
    sender_nodes <- df2 %>% distinct(id = sender, label = sender, group = sender_type)
    receiver_nodes <- df2 %>% distinct(id = receiver, label = receiver, group = receiver_type)
    rel_nodes <- df2 %>% distinct(id = relationship, label = rel_label, group = "Relationship")
    
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
    
    # Update dropdown with unique node labels
    updateSelectInput(session, "selected_node", choices = sort(unique(nodes$label)))
  
    
    edges <- df2 %>%
      transmute(from = sender, to = relationship) %>%
      bind_rows(df2 %>% transmute(from = relationship, to = receiver))
    
    # Get selected ID
    selected_id <- input$selected_node
    
    # Filter edges connected to selected node (2-hop)
    selected_edges <- edges %>%
      filter(from == selected_id | to == selected_id)
    
    # Get connected node IDs
    connected_ids <- unique(c(selected_edges$from, selected_edges$to))
    
    # Filter nodes to only show relevant ones
    filtered_nodes <- nodes %>% filter(id %in% connected_ids)
    filtered_edges <- selected_edges
    
    visNetwork(nodes, edges, height = "700px", width = "100%") %>%
      visEdges(arrows = "to", color = list(color = "#aaa", highlight = "red")) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE) %>%
      visPhysics(solver = "forceAtlas2Based", stabilization = TRUE) %>%
      visLayout(randomSeed = 11) %>%
      visLegend(addNodes = data.frame(
        label = c("Person", "Organization", "Vessel", "Location", "Relationship"),
        shape = c("dot", "ellipse", "diamond", "triangle", "box"),
        color = c("#6baed6", "#ffd700", "#fb6a4a", "#74c476", "#d07be5")
      ))
  })
  
}

# Run the app
shinyApp(ui, server)
