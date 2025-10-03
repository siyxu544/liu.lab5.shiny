# --- Memoized Data Loading Functions ---
# This code defines the functions but does NOT call them at startup.
mem_get_kpis <- memoise::memoise(get_kpis)
mem_get_municipalities <- memoise::memoise(get_municipalities)

# Define server logic
function(input, output, session) {
  # These are "data dictionaries" for KPIs and Municipalities
  cat("Loading KPI and Municipality dictionaries (from cache if available)...\n")
  kpi_dict <- mem_get_kpis()
  municipality_dict <- mem_get_municipalities()
  cat("Dictionaries loaded!\n")

  # Prepare the choices for selectizeInput in the required named list format.
  # Format: list("Label to show" = "value_to_return")
  kpi_choices <- setNames(kpi_dict$id, kpi_dict$title)
  municipality_choices <- setNames(municipality_dict$id, municipality_dict$title)

  # --- Dynamically update UI controls ---
  # Use server-side selectize to handle very long lists (5000+ KPIs) efficiently.
  # This keeps the full list on the server and only sends matching options to the browser.
  updateSelectizeInput(session,
                       "kpi_selector",
                       choices = kpi_choices,
                       server = TRUE)
  updateSelectizeInput(session,
                       "municipality_selector",
                       choices = municipality_choices,
                       server = TRUE)

  # --- Reactive data fetching ---
  # eventReactive executes only when its dependency (the button click) occurs.
  fetched_data <- eventReactive(input$run_query, {
    # Check if either of the required inputs is empty
    if (is.null(input$kpi_selector) ||
        is.null(input$municipality_selector)) {
      showNotification("Please select at least one KPI and one Municipality.", type = "warning")
      # Return NULL to ensure downstream reactives don't error
      return(NULL)
    }
    # --- Validation Check for Year Range ---
    start_year <- input$year_slider[1]
    end_year <- input$year_slider[2]
    validate(
      need(
        end_year - start_year <= 24,
        "Error: The selected year range cannot exceed 24 years. Please select a shorter period."
      )
    )
    # Prepare parameters from inputs
    kpi_ids <- input$kpi_selector
    municipality_ids <- input$municipality_selector
    years <- seq(input$year_slider[1], input$year_slider[2])
    # Display a loading message to improve the user experience
    withProgress(message = 'Fetching data from Kolada API...', value = 0.5, {
      # Call the function from liu.lab5.koladar package
      get_data(kpi_id = kpi_ids,
               municipality_id = municipality_ids,
               year = years)

    })
  })

  # --- Render outputs ---
  output$time_series_plot <- renderPlot({
    plot_data_raw <- fetched_data()
    validate(need(
      nrow(plot_data_raw) > 0,
      "No data found for the selected criteria."
    ))
    plot_data_with_names <- plot_data_raw %>%
      left_join(kpi_dict, by = c("kpi" = "id")) %>%
      rename(kpi_title = title) %>%
      left_join(municipality_dict, by = c("municipality" = "id")) %>%
      rename(municipality_title = title) %>%

      tidyr::unite("facet_title", kpi_title, municipality_title, sep = "\n")

    ggplot(plot_data_with_names,
           aes(
             x = period,
             y = value,
             group = gender,
             color = gender
           )) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      facet_wrap(~ facet_title, scales = "free_y") +
      # scale_x_continuous(breaks = scales::integer_breaks()) +
      scale_x_continuous(breaks = function(x) unique(floor(pretty(x)))) +
      labs(
        title = "KPI Time Series Trend",
        x = "Year",
        y = "Value",
        color = "Gender"
      ) +
      theme_minimal(base_size = 15) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })


  # Render the interactive data table
  output$results_table <- DT::renderDataTable({
    table_data <- fetched_data()
    validate(need(
      nrow(table_data) > 0,
      "No data found for the selected criteria."
    ))
    # Join with the dictionaries to show user-friendly names instead of IDs
    table_data %>%
      left_join(kpi_dict, by = c("kpi" = "id")) %>%
      left_join(municipality_dict, by = c("municipality" = "id")) %>%
      # Select and rename columns for a cleaner table display
      select(
        KPI = title.x,
        Municipality = title.y,
        Year = period,
        Gender = gender,
        Value = value
      )
  })
}
