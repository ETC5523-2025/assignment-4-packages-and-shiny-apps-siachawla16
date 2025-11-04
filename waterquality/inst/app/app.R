library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(lubridate)


data("yarra_wq", package = "waterquality")

# Parameter choices & date limits
param_choices <- sort(unique(yarra_wq$parameter))
date_min <- min(yarra_wq$date, na.rm = TRUE)
date_max <- max(yarra_wq$date, na.rm = TRUE)

# Known data gap
gap_start <- as.Date("1995-01-01")
gap_end   <- as.Date("2019-12-31")

theme_app <- bs_theme(bootswatch = "flatly")

ui <- navbarPage(
  title = "Yarra River water quality",
  theme = theme_app,


  tabPanel(
    "Explore",
    sidebarLayout(
      sidebarPanel(
        h4("Filters"),
        helpText("Each 'parameter' is a water-quality variable measured at the Yarra River site (e.g. pH, conductivity)."),
        helpText("Note: there is a major data gap from ~1995–2019, so blank areas of the plot indicate no available WMIS measurements, not zero values."),
        helpText("Interpretation: if the lines move together, parameters co-vary. If they diverge, they respond differently to conditions."),
        checkboxGroupInput(
          "params", "Parameters",
          choices = param_choices, selected = head(param_choices, 3)
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
          "normalize", "Standardize per parameter (z-score) to compare scales", value = TRUE
        ),
        helpText("Note: No WMIS records between ",
                 format(gap_start, "%Y"), " and ", format(gap_end, "%Y"),
                 ". If lines disappear there, it’s missing data, not zeros.")
      ),
      mainPanel(
        div(class = "card",
            div(class = "card-header", "Time series (overlayed)"),
            plotOutput("ts_plot", height = 460),
            uiOutput("gap_note")
        ),
        div(class = "card", style = "margin-top:12px;",
            div(class = "card-header", "Summary"),
            verbatimTextOutput("summary_txt")
        )
      )
    )
  ),


  tabPanel(
    "Summaries",
    sidebarLayout(
      sidebarPanel(
        h4("Parameters"),
        checkboxGroupInput(
          "params_sum", "Choose parameters",
          choices = param_choices, selected = head(param_choices, 3)
        ),
        div(
          style = "display:flex; gap:8px; margin-top:-8px; margin-bottom:6px;",
          actionButton("sum_select_all", "Select all"),
          actionButton("sum_clear_all", "Clear all"),
          actionButton("sum_copy_from_explore", "Copy from Explore")
        ),
        hr(),
        h4("Choose period type"),
        radioButtons(
          "period_type", NULL,
          choices = c("Before date", "After date"),
          selected = "Before date", inline = TRUE
        ),
        dateInput("before_date", "Before date (≤ this date)", value = as.Date("1994-12-31")),
        dateInput("after_date",  "After date (≥ this date)",  value = as.Date("2020-01-01")),
        helpText("There are no WMIS records between ",
                 format(gap_start, "%Y"), " and ", format(gap_end, "%Y"),
                 ". Choose a period before or after this gap to summarise.")
      ),
      mainPanel(
        div(class = "card",
            div(class = "card-header", "Per-parameter summaries"),
            tableOutput("summary_table")
        ),
        div(class = "card", style = "margin-top:12px;",
            div(class = "card-header", "Notes"),
            htmlOutput("summary_note")
        )
      )
    )
  )
)

server <- function(input, output, session) {


  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "params", selected = param_choices)
  })
  observeEvent(input$clear_all, {
    updateCheckboxGroupInput(session, "params", selected = character(0))
  })

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


  observeEvent(input$sum_select_all, {
    updateCheckboxGroupInput(session, "params_sum", selected = param_choices)
  })
  observeEvent(input$sum_clear_all, {
    updateCheckboxGroupInput(session, "params_sum", selected = character(0))
  })
  observeEvent(input$sum_copy_from_explore, {
    updateCheckboxGroupInput(session, "params_sum", selected = input$params)
  })


  period_filtered <- reactive({
    req(length(input$params_sum) > 0)
    if (input$period_type == "Before date") {
      yarra_wq |>
        filter(parameter %in% input$params_sum,
               date <= input$before_date)
    } else {
      yarra_wq |>
        filter(parameter %in% input$params_sum,
               date >= input$after_date)
    }
  })


  summaries_tbl <- reactive({
    df <- period_filtered()
    df |>
      group_by(parameter) |>
      summarise(
        n      = n(),
        mean   = mean(value, na.rm = TRUE),
        median = median(value, na.rm = TRUE),
        sd     = sd(value, na.rm = TRUE),
        min    = min(value, na.rm = TRUE),
        max    = max(value, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(parameter)
  })

  output$summary_table <- renderTable({
    tbl <- summaries_tbl()
    validate(need(nrow(tbl) > 0, "No data in the chosen period for the selected parameters."))
    num_cols <- setdiff(names(tbl), "parameter")
    tbl[num_cols] <- lapply(tbl[num_cols], function(x) if (is.numeric(x)) round(x, 3) else x)
    tbl
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "s", width = "100%")

  output$summary_note <- renderUI({
    HTML(paste0(
      "<p><strong>Period type:</strong> ", input$period_type, "</p>",
      if (input$period_type == "Before date")
        paste0("<p>Summaries include observations up to and including <em>",
               format(input$before_date, "%Y-%m-%d"), "</em>.</p>")
      else
        paste0("<p>Summaries include observations on/after <em>",
               format(input$after_date, "%Y-%m-%d"), "</em>.</p>"),
      "<p><em>Important:</em> WMIS has <strong>no records between ",
      format(gap_start, "%Y"), " and ", format(gap_end, "%Y"),
      "</strong>. If your chosen cutoff lands in or near this gap, missing data explains any apparent ‘flat’ sections or empty results.</p>"
    ))
  })
}

shinyApp(ui, server)





