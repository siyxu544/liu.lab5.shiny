library(shiny)
library(DT)
# Define the user interface
fluidPage(
  # Application title
  titlePanel("Kolada API Data Browser"),

  # Sidebar layout
  sidebarLayout(
    # Sidebar panel for input controls
    sidebarPanel(
      h4("Query Parameters"),

      # KPI selector
      # Using selectizeInput for its search support with long lists
      selectizeInput(
        inputId = "kpi_selector",
        label = "Select KPI(s):",
        choices = NULL, # Choices are loaded dynamically on the server side
        multiple = TRUE,
        options = list(placeholder = 'Type to search for KPIs...')
      ),

      # Municipality selector
      selectizeInput(
        inputId = "municipality_selector",
        label = "Select Municipality(ies):",
        choices = NULL, # Choices are loaded dynamically on the server side
        multiple = TRUE,
        options = list(placeholder = 'Type to search for municipalities...')
      ),

      # Year range selector
      sliderInput(
        inputId = "year_slider",
        label = "Select Year Range:",
        min = 1970,
        max = as.integer(format(Sys.Date(), "%Y")), # Default max year is the current year
        value = c(2018, 2024), # Default selected range
        sep = "", # Remove the thousand separator
        step = 1
      ),

      # Action button to trigger the query
      actionButton("run_query", "Run Query", icon = icon("search"), class = "btn-primary")
    ),

    # Main panel for displaying output
    mainPanel(
      # Use a tabsetPanel to separate the plot and the table
      tabsetPanel(
        type = "tabs",
        tabPanel("Trend Plot", plotOutput("time_series_plot")),
        tabPanel("Raw Data", DT::dataTableOutput("results_table"))
      )
    )
  )
)
