library(shiny)
library(dplyr)
library(visNetwork)
library(ggplot2)
library(DT)
library(tidyr)
library(stringr)

# ---- Simulated placeholder data ----
nodes <- data.frame(id = c("Mako", "Neptune", "Remora", "The Intern", "Mrs. Money"),
                    label = c("Mako", "Neptune", "Remora", "The Intern", "Mrs. Money"),
                    group = c("A", "A", "B", "B", "C"))

edges <- data.frame(from = c("Mako", "Neptune", "Remora", "Remora"),
                    to = c("Neptune", "Remora", "The Intern", "Mrs. Money"))

keywords <- data.frame(cluster = c(1, 1, 1, 2, 2),
                       word = c("reef", "money", "lookout", "intern", "middleman"),
                       n = c(123, 91, 80, 90, 50))

pseudonyms <- c("Mako", "Neptune", "Remora", "The Intern", "Mrs. Money")
mention_counts <- data.frame(
  pseudonym = pseudonyms,
  count = c(112, 68, 58, 38, 49)
)

mention_heatmap <- expand.grid(sender = pseudonyms, pseudonym = pseudonyms) %>%
  mutate(count = sample(0:5, n(), replace = TRUE))

# ---- UI ----
ui <- fluidPage(
  titlePanel("Communication Clusters and Pseudonyms"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("weeks", "Select Week(s):", choices = c("Week 1", "Week 2")),
      dateRangeInput("daterange", "Select Date Range:", start = "2040-10-01", end = "2040-10-14"),
      sliderInput("hour", "Hour of Day", min = 0, max = 23, value = c(8, 18)),
      selectInput("individual", "Individual ID Selection", choices = c("", nodes$id)),
      selectInput("cluster", "Cluster Selection", choices = c(1, 2))
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
  
  # Chart 1: visNetwork
  output$network <- renderVisNetwork({
    sel <- input$individual
    highlight_nodes <- if (sel != "") unique(c(sel, edges$to[edges$from == sel], edges$from[edges$to == sel])) else NULL
    
    visNetwork(nodes, edges) %>%
      visNodes(color = list(highlight = "red")) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visIgraphLayout()
  })
  
  # Chart 2: Table of top keywords
  output$keyword_table <- renderDataTable({
    keywords %>%
      filter(cluster == input$cluster)
  })
  
  # Pseudonym bar chart
  output$pseudonym_bar <- renderPlot({
    ggplot(mention_counts, aes(x = reorder(pseudonym, count), y = count)) +
      geom_col(fill = "#2c7fb8") +
      coord_flip() +
      labs(title = "Total Mentions of Identified and Suspected Pseudonyms",
           x = "Pseudonym", y = "Frequency") +
      theme_minimal()
  })
  
  # Pseudonym mentions heatmap
  output$pseudonym_heatmap <- renderPlot({
    mention_heatmap %>%
      ggplot(aes(x = pseudonym, y = sender, fill = count)) +
      geom_tile(color = "white") +
      geom_text(aes(label = ifelse(count > 0, count, "")), size = 3) +
      scale_fill_gradient(low = "#e0ecf4", high = "#0868ac") +
      labs(title = "Pseudonym Mentions by Sender", x = "Pseudonym", y = "Sender") +
      theme_minimal()
  })
}

# ---- RUN APP ----
shinyApp(ui = ui, server = server)