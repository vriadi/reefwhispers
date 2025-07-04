# keyword_insights_app.R
library(shiny)
library(tidyverse)
library(tidytext)
library(stopwords)

# Load data
messages <- read_csv("data/communications_full.csv")

# Predefined keyword categories
keyword_categories <- tibble(
  word = c("nadia", "davis", "elise", "reef", "nemo", "neptune", "marina", "mako", 
           "equipment", "foundation", "scope", "payment", "tonight", "meeting", 
           "modified", "finalize", "documentation", "eastern", "bring"),
  category = c("Person", "Person", "Person", "Location", "Location", "Location", 
               "Location", "Location", "Construction", "Construction", "Construction", 
               "Action", "Time", "Action", "Action", "Action", "Object", "Location", 
               "Action")
)

# UI
ui <- fluidPage(
  titlePanel("Keyword Insights - Top Words in Nadia-Related Messages"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("selected_date", "Select Date",
                   choices = c("Oct 8" = "2040-10-08",
                               "Oct 9" = "2040-10-09",
                               "Oct 10" = "2040-10-10",
                               "Oct 11" = "2040-10-11",
                               "Oct 12" = "2040-10-12"),
                   selected = "2040-10-08"),
      selectInput("keyword_category", "Filter by Category",
                  choices = c("All", unique(keyword_categories$category)),
                  selected = "All")
    ),
    mainPanel(
      plotOutput("keywordPlot", height = "600px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  output$keywordPlot <- renderPlot({
    req(input$selected_date)
    
    related_msgs <- messages %>%
      filter(str_detect(content, "Nadia")) %>%
      filter(as.Date(timestamp) == input$selected_date) %>%
      filter(!is.na(content) & content != "")
    
    keywords <- related_msgs %>%
      unnest_tokens(word, content) %>%
      filter(!word %in% stopwords::stopwords("en")) %>%
      count(word, sort = TRUE) %>%
      slice_max(n, n = 20) %>%
      left_join(keyword_categories, by = "word") %>%
      mutate(category = ifelse(is.na(category), "Other", category)) %>%
      filter(input$keyword_category == "All" | category == input$keyword_category)
    
    ggplot(keywords, aes(x = reorder(word, n), y = n, fill = category)) +
      geom_col() +
      coord_flip() +
      labs(
        title = paste("Top Keywords in Nadia-Related Messages on", input$selected_date),
        x = "Keyword", y = "Frequency"
      ) +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui, server)