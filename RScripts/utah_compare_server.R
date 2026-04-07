##### Utah #####

selected_var_utah <- reactive({
  variable_key %>% filter(INDICATOR == input$utah_var) %>% pull(SHORTNAME)
})

pls_utah_reactive <- reactive({
  pls_utah %>%
    filter(
      var == selected_var_utah() #,
      #CURRENT_LIBNAME %in% input$utah_libraries
    )
})

output$utah_hc <- renderHighchart({
  if (selected_var_utah() %in% currency_cols) {
    y_tt <- "${point.y:,.2f}"
    var_tt <- "${point.value:,.0f}"
    # Y axis $ prefix
  } else {
    # TODO remove decimals from non-decimal vars
    y_tt <- "{point.y:,.2f}" #
    var_tt <- "{point.value:,f}" #:,.2f
  }

  col_name_pretty <- input$utah_var

  df <- pls_utah_reactive() %>%
    filter(
      FISCAL_YEAR >= input$utah_years[1],
      FISCAL_YEAR <= input$utah_years[2]
    )

  percap_text <- unique(df$percap_text)

  df_highlight <- df %>% filter(CURRENT_LIBNAME == input$utah_highlight)
  df_nhighlight <- df %>% filter(CURRENT_LIBNAME != input$utah_highlight)
  df_stavg <- df %>%
    group_by(FISCAL_YEAR) %>%
    mutate(
      percap = round(mean(percap, na.rm = T), 2)
    ) %>%
    ungroup()

  hc <- highchart() %>%
    hc_chart(zoomType = "y") %>%
    hc_add_series(
      df_highlight,
      type = "line",
      color = "#4EC3E0",
      index = 2,
      lineWidth = 4,
      hcaes(x = FISCAL_YEAR, y = percap, group = CURRENT_LIBNAME)
    )

  if (input$utah_switch) {
    hc %<>%
      hc_add_series(
        df_nhighlight,
        type = "line",
        color = "#000000", # "#d6d3d3ff",
        fillOpacity = .6,
        index = 1,
        lineWidth = 1,
        hcaes(x = FISCAL_YEAR, y = percap, group = CURRENT_LIBNAME)
      ) %>%
      hc_legend(enabled = FALSE) %>%
      hc_tooltip(
        pointFormat = paste0(
          "<b>{series.name}</b><br>",
          "<b>Rank: {point.rank} out of {point.n}</b><br>",
          "<b>",
          col_name_pretty,
          " {point.percap_text}",
          ": ",
          y_tt,
          "</b><br>",
          col_name_pretty,
          ": ",
          var_tt,
          "<br>",
          "Legal Service Area Population: {point.POPU_LSA:,.0f}<br>",
          "{point.x}"
        ),
        headerFormat = ""
      ) %>%
      hc_yAxis(
        title = list(
          text = paste0(col_name_pretty, " ", percap_text),
          style = list(fontSize = "15px")
        ),
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
        text = paste0(col_name_pretty, " ", percap_text, " by Library")
      ) %>%
      hc_caption(
        text = "Some libraries may have no data for certain years. Rankings reflect those that did submit data for a given year."
      )
  } else {
    hc %<>%
      hc_add_series(
        df_stavg,
        name = "State Average",
        type = "line",
        color = "#000000ff",
        fillOpacity = .6,
        index = 1,
        lineWidth = 1,
        hcaes(x = FISCAL_YEAR, y = percap)
      ) %>%
      hc_legend(enabled = FALSE) %>%
      hc_tooltip(
        pointFormat = paste0(
          "<b>{series.name}</b><br>",
          "<b>",
          col_name_pretty,
          " {point.percap_text}",
          ": ",
          y_tt,
          "</b>"
        ),
        headerFormat = "",
        shared = TRUE,
        split = TRUE
      ) %>%
      hc_yAxis(
        title = list(
          text = paste0(col_name_pretty, " ", percap_text),
          style = list(fontSize = "15px")
        ),
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
        text = paste0(
          col_name_pretty,
          " ",
          percap_text
        )
      ) %>%
      hc_subtitle(text = paste0(input$utah_highlight, " vs. State Average")) %>%
      hc_caption(
        text = paste0(
          "The state average represents the average per capita value across all libraries, including ",
          input$utah_highlight,
          "."
        )
      )
  }

  hc %>%
    hc_plotOptions(
      series = list(
        marker = list(enabled = FALSE),
        states = list(inactive = list(enabled = FALSE)) # prevents greyout
      )
    )
})


output$utah_dt <- renderReactable({
  req(input$utah_var)
  var_name <- input$utah_var
  percap_text <- unique(pls_utah_reactive()$percap_text)

  df <- pls_utah_reactive() %>%
    filter(FISCAL_YEAR == input$utah_dt_year) %>%
    select(
      #Year = FISCAL_YEAR,
      Library = CURRENT_LIBNAME,
      Population_Service_Area = POPU_LSA,
      value,
      percap,
      rank,
      n
    )

  output$utah_dt_title <- renderUI({
    x <- HTML(paste0(
      "<b style='font-size:16px;'>",
      input$utah_var,
      " ",
      unique(pls_utah_reactive()$percap_text),
      " (",
      input$utah_dt_year,
      ")</b>"
    ))
    x
  })

  # Render reactable
  df %>%
    reactable(
      resizable = TRUE,
      pagination = FALSE,
      sortable = FALSE,
      #groupBy = "Year",
      #defaultExpanded = FALSE,
      defaultSorted = list(rank = "asc"), #Year = "desc",
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
          filterable = TRUE
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
        #   Year = colDef(
        #     grouped = JS(
        #       "function(cellInfo) {
        #   return cellInfo.value
        # }"
        #     )
        #   ),
        Population_Service_Area = colDef(
          name = "Legal Service Area Population",
          cell = function(value) {
            format(value, big.mark = ",")
          }
        ),
        percap = colDef(
          name = paste0(var_name, " ", percap_text),
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
