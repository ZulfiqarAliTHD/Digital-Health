# ==============================================================================
# App Name: CovidVaxDeath_GlobalTracker
# Purpose: Epidemiological analysis of COVID-19 Vaccinations vs. Mortality
# Workflow: Epi-R (Clean -> Inspect -> Analyze -> Visualize)
# ==============================================================================

library(shiny)
library(tidyverse)
library(janitor)
library(plotly)
library(DT)
library(bslib)

# 1. DATA PREPARATION & CLEANING (Epi-R Workflow)
# ------------------------------------------------------------------------------
# We encapsulate the cleaning logic to ensure a 'Line List' format
prepare_epi_data <- function() {
  # Read data
  raw_data <- read.csv("covid-vaccination-vs-death_ratio.csv", stringsAsFactors = FALSE) %>%
    clean_names() # Automated cleaning of column names to snake_case
  
  # Data Cleaning & Transformation
  clean_data <- raw_data %>%
    # Filtering for the 4 target countries
    filter(country %in% c("Germany", "United States of America", "The United Kingdom", "Pakistan")) %>%
    
    # Cleaning dates (Converting from string to Date class)
    mutate(date = as.Date(date)) %>%
    
    # Factor management: crucial for consistent ordering and colors
    mutate(country = factor(country, 
                            levels = c("Germany", "United States of America", 
                                       "The United Kingdom", "Pakistan"))) %>%
    
    # NA Handling: Removing rows where critical data is missing
    drop_na(total_vaccinations, new_deaths) %>%
    
    # Descriptive Epidemiology: Calculating Mortality Rate per 100,000 population
    mutate(mortality_rate_100k = (new_deaths / population) * 100000)
  
  return(clean_data)
}

# Initialize data
data_master <- prepare_epi_data()

# 2. USER INTERFACE (UI)
# ------------------------------------------------------------------------------
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  titlePanel("COVID-19 Analysis: Vaccination Progress vs. Mortality Trends"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h5("Analysis Filters"),
      hr(),
      # Multi-select countries
      checkboxGroupInput("countries", "Select Countries:",
                         choices = levels(data_master$country),
                         selected = c("Germany", "Pakistan")),
      
      # Variable selection for trend analysis
      selectInput("variable", "Select Metric (Pilot):",
                  choices = c("Total Vaccinations" = "total_vaccinations",
                              "New Deaths" = "new_deaths",
                              "Vaccination Ratio (%)" = "ratio",
                              "Mortality per 100k" = "mortality_rate_100k")),
      
      hr(),
      helpText("This app follows standard Epi-R guidelines for academic submissions."),
      downloadButton("downloadData", "Export Cleaned Line-List")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        # Tab 1: Trend Analysis (Descriptive Epi)
        tabPanel("Trend Analysis", 
                 plotlyOutput("linePlot", height = "500px"),
                 p(style = "margin-top: 10px;", 
                   em("Analysis: The line graph shows the temporal distribution of the selected variable across regions."))
        ),
        
        # Tab 2: Comparison (Cumulative Impact)
        tabPanel("Comparative Analysis", 
                 plotlyOutput("barChart", height = "500px"),
                 p(style = "margin-top: 10px;", 
                   em("Analysis: This bar chart visualizes cumulative totals or rates for direct comparison between selected nations."))
        ),
        
        # Tab 3: Inspection & Line List
        tabPanel("Data Inspection", 
                 h4("Cleaned Line-List"),
                 DTOutput("dataTable"),
                 hr(),
                 h4("Epidemiological Summary"),
                 verbatimTextOutput("summaryStats")
        )
      )
    )
  )
)

# 3. SERVER LOGIC
# ------------------------------------------------------------------------------
server <- function(input, output) {
  
  # Reactive subsetting based on user input
  filtered_df <- reactive({
    data_master %>%
      filter(country %in% input$countries)
  })
  
  # 1. Line Plot (Trend Analysis)
  output$linePlot <- renderPlotly({
    p <- ggplot(filtered_df(), aes(x = date, y = .data[[input$variable]], color = country)) +
      geom_line(size = 1) +
      theme_minimal() +
      labs(title = paste("Trend of", input$variable, "over time"),
           x = "Date", y = input$variable) +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # 2. Bar Chart (Comparative Visualization)
  output$barChart <- renderPlotly({
    # Aggregate data to find the maximum value reached for the selected metric
    summary_data <- filtered_df() %>%
      group_by(country) %>%
      summarise(value = max(.data[[input$variable]], na.rm = TRUE))
    
    p <- ggplot(summary_data, aes(x = reorder(country, -value), y = value, fill = country)) +
      geom_col() +
      theme_minimal() +
      labs(title = paste("Peak Comparison:", input$variable),
           x = "Country", y = "Maximum Value") +
      scale_fill_brewer(palette = "Set1")
    
    ggplotly(p)
  })
  
  # 3. Interactive Data Table (Line List)
  output$dataTable <- renderDT({
    datatable(filtered_df(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # 4. Statistical Summary for Inspection
  output$summaryStats <- renderPrint({
    summary(filtered_df() %>% select(country, total_vaccinations, new_deaths, ratio, mortality_rate_100k))
  })
  
  # 5. Download Data functionality
  output$downloadData <- downloadHandler(
    filename = function() { paste("cleaned_covid_data_", Sys.Date(), ".csv", sep="") },
    content = function(file) { write.csv(filtered_df(), file, row.names = FALSE) }
  )
}

# 4. RUN THE APP
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)