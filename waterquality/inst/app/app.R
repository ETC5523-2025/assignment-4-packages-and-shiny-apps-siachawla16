library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(lubridate)



# Load packaged data
data("yarra_wq", package = "waterquality")

# Parameter choices & date limits
param_choices <- sort(unique(yarra_wq$parameter))
date_min <- min(yarra_wq$date, na.rm = TRUE)
date_max <- max(yarra_wq$date, na.rm = TRUE)

# Known data gap
gap_start <- as.Date("1995-01-01")
gap_end   <- as.Date("2019-12-31")

ui <- page_sidebar(
  theme = bs_theme(bootswatch = "flatly"),
  title = "Yarra River water quality: multi-parameter overlay",
  sidebar = sidebar(
    h4("Filters"),
    checkboxGroupInput(
      "params", "Parameters",
      choices = param_choices,
      selected = head(param_choices, 3)
    ),
    div(
      style = "display:flex; gap:8px; margin-top:-8px; margin-bottom:6px;",
      actionButton("select_all", "Select all"),
      actionButton("clear_all", "Clear all")
    ),
    sliderInput(
      "dates", "Date range",
      min = date_min, max = date_max,
      value = c(date_min, date_max),
      timeFormat = "%Y-%m-%d", width = "100%"
    ),
    checkboxInput(
      "normalize", "Standardize per parameter (z-score) to compare scales", value = FALSE
    ),
    helpText("Note: No WMIS records between ",
             format(gap_start, "%Y"), " and ", format(gap_end, "%Y"),
             ". If lines disappear there, it’s missing data, not zeros."),
    hr(),
    h4("How to interpret"),
    helpText("• Overlay multiple parameters on one plot (colour = parameter).",
             "• Leave standardization OFF to see raw magnitudes (units may differ).",
             "• Turn standardization ON to compare patterns across different units/ranges.")
  ),
  layout_columns(
    col_widths = c(7,5),
    card(
      card_header("Time series (overlayed)"),
      plotOutput("ts_plot", height = 460),
      uiOutput("gap_note")
    ),
    card(
      card_header("Summary"),
      verbatimTextOutput("summary_txt")
    )
  )
)

server <- function(input, output, session) {

  # Quick select/clear helpers
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "params", selected = param_choices)
  })
  observeEvent(input$clear_all, {
    updateCheckboxGroupInput(session, "params", selected = character(0))
  })

  # Filtered data
  filtered <- reactive({
    req(length(input$params) > 0)
    yarra_wq |>
      filter(
        parameter %in% input$params,
        date >= input$dates[1],
        date <= input$dates[2]
      )
  })

  output$ts_plot <- renderPlot({
    df <- filtered(); req(nrow(df) > 0)

    # Standardize per parameter if requested
    if (isTRUE(input$normalize)) {
      df <- df |>
        group_by(parameter) |>
        mutate(value_plot = (value - mean(value, na.rm = TRUE)) / sd(value, na.rm = TRUE)) |>
        ungroup()
      ylab <- "Standardized value (z)"
    } else {
      df <- df |> mutate(value_plot = value)
      units_sel <- unique(na.omit(df$unit_of_measurement))
      ylab <- if (length(units_sel) == 1) paste0("Value (", units_sel, ")") else "Value (mixed units)"
    }

    p <- ggplot(df, aes(x = date, y = value_plot, colour = parameter)) +
      geom_line(alpha = 0.7) +
      geom_point(size = 0.8, alpha = 0.7) +
      labs(x = NULL, y = ylab, colour = "Parameter") +
      theme(legend.position = "top")

    # Shade the known data gap if the selected window overlaps it
    if (input$dates[1] <= gap_end && input$dates[2] >= gap_start) {
      xmin <- max(input$dates[1], gap_start)
      xmax <- min(input$dates[2], gap_end)
      p <- p + annotate("rect", xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, alpha = 0.08)
    }

    p
  })

  output$gap_note <- renderUI({
    if (input$dates[1] <= gap_end && input$dates[2] >= gap_start) {
      div(style = "margin-top:6px; font-size:0.9em; color:#6c757d;",
          paste0("Selected window includes ", format(gap_start, "%Y"), "–", format(gap_end, "%Y"),
                 " when WMIS has no records. Blank sections reflect missing data."))
    } else NULL
  })

  output$summary_txt <- renderText({
    df <- filtered(); if (!nrow(df)) return("No data for this selection.")
    yrs   <- range(df$year, na.rm = TRUE)
    rng   <- range(df$value, na.rm = TRUE)
    units <- paste(unique(na.omit(df$unit_of_measurement)), collapse = ", ")
    paste0(
      "Parameters: ", paste(sort(unique(df$parameter)), collapse = ", "),
      "\nRows: ", nrow(df),
      "\nYears covered: ", yrs[1], "–", yrs[2],
      "\nRaw value range: ", signif(rng[1], 4), " to ", signif(rng[2], 4),
      "\nUnit set: ", ifelse(nchar(units) > 0, units, "n/a"),
      "\nDate window: ", format(input$dates[1], "%Y-%m-%d"),
      " … ", format(input$dates[2], "%Y-%m-%d")
    )
  })
}

shinyApp(ui, server)




