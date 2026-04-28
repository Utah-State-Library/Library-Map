##### Utah #####

selected_var_utah <- reactive({
  variable_key %>% filter(INDICATOR == input$utah_var) %>% pull(SHORTNAME)
})

pls_utah_reactive <- reactive({
  pls_utah %>%
    mutate(
      per_name_pretty = case_when(
        per_name == "POP_col" ~ "Per Capita",
        per_name == "FTE_col" ~ "Per FTE"
      )
    ) %>%
    filter(
      var == selected_var_utah(),
      per_name_pretty == input$utah_per
    )
})

#### HC Line Graph ####

output$utah_line_header <- renderUI({
  tooltip(
    span(
      paste0(
        input$utah_highlight,
        " Compared to All Other Utah Libraries"
      ),
      bs_icon("info-circle")
    ),
    p(
      HTML(
        paste0(
          "Trends in library data are affected by many factors, and changes should not be interpreted without considering potential context. Many libraries, for example, undergo construction and need to close for a while which naturally reduces service. Staffing changes can also impact annual numbers. For questions about fluctuations in annual data, please contact the State Data Coordinator."
        )
      )
    ),
    options = list(customClass = "wide-tooltip")
  )
})


output$utah_hc <- renderHighchart({
  if (selected_var_utah() %in% currency_cols) {
    y_tt <- "${point.y:,.2f}"
    var_tt <- "${point.value:,.0f}"
    per_val_tt <- "${point.per_value:,.0f}"
    # Y axis $ prefix
  } else {
    # TODO remove decimals from non-decimal vars
    y_tt <- "{point.y:,.2f}" #
    var_tt <- "{point.value:,f}" #:,.2f
    per_val_tt <- "{point.per_value:,f}"
  }

  if (input$utah_per == "Per Capita") {
    per_total_text <- "Population of Legal Service Area"
  } else {
    per_total_text <- "Total FTE"
  }

  col_name_pretty <- input$utah_var

  df <- pls_utah_reactive() %>%
    filter(
      FISCAL_YEAR >= input$utah_years[1],
      FISCAL_YEAR <= input$utah_years[2]
    )

  per_text <- unique(df$per_text)

  df_highlight <- df %>% filter(CURRENT_LIBNAME == input$utah_highlight)
  df_nhighlight <- df %>% filter(CURRENT_LIBNAME != input$utah_highlight)

  hc <- highchart() %>%
    hc_chart(zoomType = "y") %>%
    hc_add_series(
      df_highlight,
      type = "line",
      color = "#81D0F0",
      index = 2,
      lineWidth = 4,
      hcaes(x = FISCAL_YEAR, y = per_calc, group = CURRENT_LIBNAME)
    )

  hc %<>%
    hc_add_series(
      df_nhighlight,
      type = "line",
      color = "#d6d3d3ff",
      fillOpacity = .6,
      index = 1,
      lineWidth = 1,
      hcaes(x = FISCAL_YEAR, y = per_calc, group = CURRENT_LIBNAME)
    ) %>%
    hc_legend(enabled = FALSE) %>%
    hc_tooltip(
      pointFormat = paste0(
        "<b>{series.name}</b><br>",
        "<b>Rank: {point.rank} out of {point.n}</b><br>",
        "<b>",
        col_name_pretty,
        " {point.per_text}",
        ": ",
        y_tt,
        "</b><br>",
        col_name_pretty,
        ": ",
        var_tt,
        "<br>",
        per_total_text, # Per category total - e.g., 'Total FTE: 1234'
        ": ",
        per_val_tt,
        "<br>",
        #"Legal Service Area Population: {point.POPU_LSA:,.0f}<br>",
        "{point.x}"
      ),
      headerFormat = ""
    ) %>%
    hc_yAxis(
      # title = list(
      #   text = paste0(col_name_pretty, " ", per_text),
      #   style = list(fontSize = "15px")
      # ),
      labels = list(
        style = list(fontSize = "15px")
      )
    ) %>%
    hc_xAxis(
      allowDecimals = FALSE,
      labels = list(
        style = list(fontSize = "15px")
      )
    ) %>%
    hc_title(
      text = paste0(col_name_pretty, " ", per_text),
      align = "left"
    ) %>%
    hc_caption(
      text = "Some libraries may have no data for certain years. Rankings reflect those that did submit data for a given year."
    )

  hc %>%
    hc_plotOptions(
      series = list(
        marker = list(enabled = FALSE),
        states = list(inactive = list(enabled = FALSE)) # prevents greyout
      )
    )
})


#### HC Bar Graph ####

output$utah_bar_header <- renderUI({
  tooltip(
    span(
      paste0(
        input$utah_highlight,
        " Compared to the Utah Library Median"
      ),
      bs_icon("info-circle")
    ),
    p(
      HTML(
        paste0(
          "<b>Utah Libraries</b> shows the median value across all libraries in the state, including ",
          input$utah_highlight,
          "."
        )
      )
    ),
    #placement = "right"
    options = list(customClass = "wide-tooltip")
  )
})


output$utah_hc_bar <- renderHighchart({
  if (selected_var() %in% currency_cols) {
    y_tt <- "${point.y:,.2f}"
    var_tt <- "${point.value:,.0f}"
    # Y axis $ prefix
  } else {
    # TODO remove decimals from non-decimal vars
    y_tt <- "{point.y:,.2f}" #
    var_tt <- "{point.value:,f}" #:,.2f
  }

  per_val_tt <- "{point.per_value:,f}"

  if (input$utah_per == "Per Capita") {
    per_total_text <- "Population of Legal Service Area"
  } else {
    per_total_text <- "FTE"
  }

  col_name_pretty <- input$utah_var

  df <- pls_utah_reactive() %>%
    filter(
      FISCAL_YEAR >= input$utah_years_bar[1],
      FISCAL_YEAR <= input$utah_years_bar[2]
    )

  per_text <- unique(df$per_text)

  df_ut <- df %>%
    filter(CURRENT_LIBNAME == input$utah_highlight) %>%
    mutate(level = CURRENT_LIBNAME, per_text_prefix = "", per_value_prefix = "")

  df_stavg <- df %>%
    group_by(FISCAL_YEAR) %>%
    summarise(
      level = "Utah Libraries",
      per_median = round(median(per_calc, na.rm = T), 2),
      per_text_prefix = "Median "
    )

  per_text <- unique(df$per_text)

  df_ut %<>%
    rename("per_median" = "per_calc")

  highchart() %>%
    hc_add_series(
      df_ut,
      type = "column",
      color = "#FFB81D",
      hcaes(x = FISCAL_YEAR, y = per_median, group = level)
    ) %>%
    hc_add_series(
      df_stavg,
      type = "column",
      color = "#093692",
      hcaes(x = FISCAL_YEAR, y = per_median, group = level)
    ) %>%
    hc_tooltip(
      pointFormat = paste0(
        "<b>{series.name}</b><br>",
        "<b>",
        "{point.per_text_prefix}", # Per category value - e.g., "Visits Per FTE: 1234"
        col_name_pretty,
        " ",
        per_text,
        ": ",
        y_tt,
        "</b><br>",
        "{point.x}"
      ),
      headerFormat = ""
    ) %>%
    hc_yAxis(
      labels = list(
        style = list(fontSize = "15px")
      )
    ) %>%
    hc_xAxis(
      allowDecimals = FALSE,
      labels = list(
        style = list(fontSize = "15px")
      )
    ) %>%
    hc_title(
      text = paste0(col_name_pretty, " ", per_text),
      align = "left"
    ) %>%
    hc_caption(text = "Tip: click on the legend to show/hide specific groups")
})

#### Utah DT ####

output$utah_table_header <- renderUI({
  paste0(
    input$utah_dt_year,
    " ",
    input$utah_var,
    " ",
    unique(pls_utah_reactive()$per_text)
  )
})

output$utah_dt <- renderReactable({
  req(input$utah_var)
  var_name <- input$utah_var
  per_text <- unique(pls_utah_reactive()$per_text)

  df <- pls_utah_reactive() %>%
    filter(FISCAL_YEAR == input$utah_dt_year) %>%
    select(
      #Year = FISCAL_YEAR,
      Library = CURRENT_LIBNAME,
      Population_Service_Area = POPU_LSA,
      FTE,
      value,
      per_calc,
      rank,
      n
    )

  # Render reactable
  df %>%
    reactable(
      resizable = TRUE,
      pagination = FALSE,
      sortable = FALSE,
      defaultSorted = list(rank = "asc"),
      highlight = TRUE,
      height = "auto",
      defaultExpanded = TRUE,
      compact = TRUE,
      theme = reactableTheme(
        headerStyle = list(
          background = "#ecf0f1",
          borderColor = "#555"
        )
      ),
      defaultColDef = colDef(align = "left"),
      columns = list(
        Library = colDef(
          filterable = TRUE,
          minWidth = 150,
          style = list(backgroundColor = "#f7f7f7")
        ),
        value = colDef(
          name = var_name,
          cell = function(value) {
            if (isTRUE(selected_var_utah() %in% currency_cols)) {
              dollar(value)
            } else if (is.na(value)) {
              "No Data"
            } else {
              format(value, big.mark = ",")
            }
          }
        ),
        Population_Service_Area = colDef(
          name = "Legal Service Area Population",
          cell = function(value) {
            format(value, big.mark = ",")
          }
        ),
        FTE = colDef(
          name = "FTE",
          cell = function(value) {
            format(value, big.mark = ",")
          }
        ),
        per_calc = colDef(
          name = paste0(var_name, " ", per_text),
          cell = function(value) {
            if (isTRUE(selected_var_utah() %in% currency_cols)) {
              paste0("$", format(value, big.mark = ","))
            } else if (is.na(value)) {
              "No Data"
            } else {
              format(value, big.mark = ",")
            }
          }
        ),
        rank = colDef(
          name = "Rank (by year)",
          sortNALast = TRUE,
          cell = function(value, index) {
            if (!is.na(value)) {
              paste0(value, "/", df$n[index])
            } else {
              ""
            }
          }
        ),
        n = colDef(show = FALSE)
      )
    )
})
