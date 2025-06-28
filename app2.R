library(shiny)
library(dplyr)
library(visNetwork)
library(ggplot2)
library(DT)
library(tidyr)
library(stringr)
library(tidytext)
library(igraph)

# ---- Load and process data ----
comm_full <- read.csv("data/communications_full.csv", stringsAsFactors = FALSE)

# Add dummy 'cluster' if missing
if (!"cluster" %in% colnames(comm_full)) {
  comm_full$cluster <- sample(1:2, nrow(comm_full), replace = TRUE)
}

# Create nodes
nodes <- unique(c(comm_full$sender_label, comm_full$receiver_label)) %>%
  data.frame(id = ., label = .) %>%
  mutate(group = "All")

# Create edges with weights
edges <- comm_full %>%
  group_by(from = sender_label, to = receiver_label) %>%
  summarise(weight = n(), .groups = "drop")

# Create keyword table
keywords <- comm_full %>%
  filter(!is.na(content)) %>%
  unnest_tokens(word, content) %>%
  filter(!word %in% stop_words$word) %>%
  count(cluster, word, sort = TRUE) %>%
  group_by(cluster) %>%
  top_n(10, n)

# Create pseudonym mention counts
mention_counts <- comm_full %>%
  pivot_longer(cols = c(sender_label, receiver_label), names_to = "type", values_to = "pseudonym") %>%
  count(pseudonym, name = "count") %>%
  arrange(desc(count))

# Create heatmap data
mention_heatmap <- comm_full %>%
  count(sender = sender_label, pseudonym = receiver_label) %>%
  complete(sender, pseudonym, fill = list(n = 0)) %>%
  rename(count = n)

# ---- UI ----
ui <- fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("weeks", "Select Week(s):", choices = c("Week 1", "Week 2")),
      dateRangeInput("daterange", "Select Date Range:", start = "2040-10-01", end = "2040-10-14"),
      sliderInput("hour", "Hour of Day", min = 0, max = 23, value = c(0, 23)),
      selectInput("individual", "Individual ID Selection", choices = c("", nodes$id)),
      selectInput("cluster", "Cluster Selection", choices = sort(unique(keywords$cluster)))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Chart 1: Network",
                 visNetworkOutput("network")
        ),
        tabPanel("Chart 2: Keywords",
                 dataTableOutput("keyword_table")
        ),
        tabPanel("Pseudonyms General Usage",
                 plotOutput("pseudonym_bar")
        ),
        tabPanel("Pseudonym Mentions",
                 plotOutput("pseudonym_heatmap")
        )
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  output$network <- renderVisNetwork({
    req(nodes, edges)
    sel <- input$individual
    highlight_nodes <- if (sel != "") unique(c(sel, edges$to[edges$from == sel], edges$from[edges$to == sel])) else NULL
    
    visNetwork(nodes, edges) %>%
      visNodes(color = list(highlight = "red")) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visIgraphLayout()
  })
  
  output$keyword_table <- renderDataTable({
    req(input$cluster)
    keywords %>%
      filter(cluster == input$cluster)
  })
  
  output$pseudonym_bar <- renderPlot({
    req(mention_counts)
    ggplot(mention_counts, aes(x = reorder(pseudonym, count), y = count)) +
      geom_col(fill = "#2c7fb8") +
      coord_flip() +
      labs(title = "Total Mentions of Identified and Suspected Pseudonyms",
           x = "Pseudonym", y = "Frequency") +
      theme_minimal()
  })
  
  output$pseudonym_heatmap <- renderPlot({
    req(mention_heatmap)
    ggplot(mention_heatmap, aes(x = pseudonym, y = sender, fill = count)) +
      geom_tile(color = "white") +
      geom_text(aes(label = ifelse(count > 0, count, "")), size = 3) +
      scale_fill_gradient(low = "#e0ecf4", high = "#0868ac") +
      labs(title = "Pseudonym Mentions by Sender", x = "Pseudonym", y = "Sender") +
      theme_minimal()
  })
}

# ---- RUN APP ----
shinyApp(ui = ui, server = server)
