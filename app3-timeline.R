# Load libraries
library(shiny)
library(tidyverse)
library(lubridate)
library(stringr)
library(janitor)
library(tidytext)
library(stopwords)
library(ggrepel)
library(shinycssloaders)
library(plotly)

# Load your data
messages <- read_csv("data/communications_full.csv")

# Event keyword definitions
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

# UI
ui <- fluidPage(
  titlePanel("Nadia's Timeline of Events & Operational Focus"),
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
  tabsetPanel(
    tabPanel("Operational Focus Timeline",
             fluidPage(
               tags$style(HTML("
                 .shiny-plot-output { margin-bottom: 30px; }
                 h3 { margin-top: 20px; font-weight: bold; }
               ")),
               tags$h3("📆 Timeline of Operational Focus"),
               withSpinner(plotlyOutput("focusTimelinePlot")),
               
               tags$h3("📊 Event Types Involving Nadia"),
               fluidRow(
                 column(6, withSpinner(plotOutput("eventTypeBarPlot"))),
                 column(6, withSpinner(plotOutput("actorHeatmapPlot")))
               )
             )
    ),
    
    tabPanel("Evidence Events",
             fluidPage(
               tags$h3("🔎 Timeline of Evidence Events Involving Nadia"),
               withSpinner(plotlyOutput("evidenceEventPlot", height = "650px"))
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Plot 1: Interactive timeline
  output$focusTimelinePlot <- renderPlotly({
    df_focus <- tibble::tibble(
      Date = factor(c("Oct 8", "Oct 9", "Oct 10", "Oct 11", "Oct 12"),
                    levels = c("Oct 8", "Oct 9", "Oct 10", "Oct 11", "Oct 12")),
      Focus = c("Execution\nPlanning", "Escalation\nResponse", "Surveillance &\nLegal Framing",
                "Disruption &\nRealignment", "Administrative\nClosure"),
      Description = c(
        "🔧 Oct 8: Execution planning started with preparation and set-up actions. Not much complexity yet.",
        "💬 Oct 9: Multi-faceted coordination with urgency, contingency, engineering work & admin verification.",
        "🎯 Oct 10: Surveillance, motives, legal defense. Keywords suggest risk management & internal monitoring.",
        "🚨 Oct 11: Scrutiny triggers disruption. Leadership coordination for crisis planning & strategic pivoting.",
        "📋 Oct 12: Final coordination day. Structured scheduling, documentation & media-sensitive wrap-up."
      ),
      y_text = 1.03
    )
    
    
    p <- ggplot(df_focus, aes(x = Date, y = 1, text = Description)) +
      geom_line(linewidth = 1.2, color = "#ECAEDC") +
      geom_point(size = 6, color = "#ECAEDC") +
      ylim(0.95, 1.05) +
      theme_light(base_size = 13) +
      theme(
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()
      ) +
      geom_text(aes(y = y_text, label = Focus), angle = 20, size = 4.2) +
      labs(title = "Timeline of Nadia-Linked Operational Focus (Oct 8–12, 2040)")
    
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # Plot 2: Bar chart of event types
  output$eventTypeBarPlot <- renderPlot({
    nadia_related <- messages %>%
      filter(str_detect(content, "Nadia", negate = FALSE)) %>%
      mutate(
        Primary_Event = sapply(content, classify_event_primary),
        timestamp = ymd_hms(timestamp)
      )
    
    ggplot(nadia_related, aes(x = as.Date(timestamp), fill = Primary_Event)) +
      geom_histogram(binwidth = 1, color = "black", position = "stack") +
      scale_fill_brewer(palette = "Set2") +
      labs(
        title = "Event Types Involving Nadia",
        x = "Date", y = "Messages", fill = "Event Type"
      ) +
      theme_light(base_size = 12) +
      theme(plot.title = element_text(face = "bold"))
  })
  
  # Plot 3: Heatmap of actors
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
        title = "Actor Involvement by Event Type",
        x = "Event Type", y = "Actor", fill = "Count"
      ) +
      theme_light(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Plot 4: Evidence timeline
  output$evidenceEventPlot <- renderPlotly({
    
    # Create dataset
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
        "Mako detected near protected boundary before signal loss",
        "Unusual vessel traffic near Nemo Reef",
        "Nadia canceled corridor access for Nemo Reef",
        "Movement to Nemo Reef",
        "Nothing found at Nemo Reef",
        "Underwater construction with concrete forms",
        "Patrol schedule altered in sender/recipient favor",
        "Nothing found at Nemo Reef"
      ),
      Theme = c(
        "Suspicious Movement", "Suspicious Movement", "Command Control",
        "Suspicious Movement", "Cover-up / Concealment", "Construction",
        "Command Control", "Cover-up / Concealment"
      )
    ) %>%
      mutate(
        Date = if_else(is.na(Date), as.Date("2040-10-13"), Date),
        Tooltip = paste0(
          "<b>", Event_ID, "</b><br>",
          "Type: ", Evidence_Type, "<br>",
          "Date: ", format(Date, "%b %d"), "<br>",
          "Involved: ", Involved, "<br>",
          "Details: ", Findings
        )
      )
    
    # Define colors for themes
    theme_colors <- c(
      "Suspicious Movement" = "#fcd5ce",
      "Cover-up / Concealment" = "#D97BC0",
      "Command Control" = "#d0bfff",
      "Construction" = "#b5ead7"
    )
    
    # Create ggplot
    p <- ggplot(event_data, aes(x = Date, y = Evidence_Type, color = Theme, text = Tooltip)) +
      geom_point(size = 5, alpha = 0.9) +
      scale_color_manual(values = theme_colors) +
      theme_minimal(base_size = 13) +
      labs(
        title = "Suspicious Patterns Suggesting Nadia’s Role in Coordinated Concealment",
        x = "Date / Timestamp", y = "Evidence Type", color = "Theme"
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(margin = list(t = 70))
    
  })
  
  
}

# Run the app
shinyApp(ui, server)
