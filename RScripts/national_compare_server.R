##### National #####

## Highchart
# Per Capita / vars
# Option to show national avg per capita, select certain states, or show all states

## Table
# Showing HC data

# Input Needed
# var
# National avg vs state selection

##### Reactives #####
selected_var <- reactive({
  variable_key %>% filter(INDICATOR == input$national_var) %>% pull(SHORTNAME)
})

pls_national_reactive <- reactive({
  pls_national_state %>%
    mutate(
      per_name_pretty = case_when(
        per_name == "POP_col" ~ "Per Capita",
        per_name == "FTE_col" ~ "Per FTE"
      )
    ) %>%
    filter(
      var == selected_var(),
      per_name_pretty == input$national_per
    )
})


##### HC Graph #####

output$national_line_header <- renderUI({
  tooltip(
    span(
      paste0(
        input$national_states,
        " Compared to All Other States and Territories"
      ),
      bs_icon("info-circle")
    ),
    p(
      HTML(
        paste0(
          "Per capita comparisons are a way to make meaningful comparisons between libraries that serve populations of different sizes. Instead of looking at raw totals — which can be misleading — you divide a library’s data by the size of the population it serves. This shows how much service or usage occurs per person, making it easier to compare libraries on equal footing.<br>",
          "For certain comparisons, it’s useful to express data per 100, 1,000, or more people to make differences easier to interpret. In some cases — especially with smaller libraries — the standardized rate may exceed the total population served, resulting in a comparison value that is higher than the actual number."
        )
      )
    ),
    options = list(customClass = "wide-tooltip")
  )
})


output$national_hc <- renderHighchart({
  if (selected_var() %in% currency_cols) {
    y_tt <- "${point.y:,.2f}"
    var_tt <- "${point.value:,.0f}"
    # Y axis $ prefix
  } else {
    # TODO remove decimals from non-decimal vars
    y_tt <- "{point.y:,.2f}" #
    var_tt <- "{point.value:,f}" #:,.2f
  }

  col_name_pretty <- input$national_var

  df <- pls_national_reactive() %>%
    filter(
      FISCAL_YEAR >= input$national_years[1],
      FISCAL_YEAR <= input$national_years[2]
    )

  per_text <- unique(df$per_text)

  df_ut <- df %>% filter(state == input$national_states)
  df_nut <- df %>% filter(state != input$national_states)
  # df_natavg <- df %>%
  #   group_by(FISCAL_YEAR) %>%
  #   mutate(
  #     per_calc = round(mean(per_calc, na.rm = T), 2),
  #     n_states = n_distinct(state)
  #   ) %>%
  #   ungroup() %>%
  #   filter(n_states > 1)

  highchart() %>%
    hc_chart(zoomType = "y") %>%
    hc_add_series(
      df_ut,
      type = "line",
      color = "#81D0F0",
      index = 2,
      lineWidth = 4,
      hcaes(x = FISCAL_YEAR, y = per_calc, group = state)
    ) %>%
    hc_add_series(
      df_nut,
      type = "line",
      color = "#d6d3d3ff",
      fillOpacity = .6,
      index = 1,
      lineWidth = 1,
      hcaes(x = FISCAL_YEAR, y = per_calc, group = state)
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
        "Legal Service Area Population: {point.POPU_LSA:,.0f}<br>",
        "{point.x}"
      ),
      headerFormat = ""
    ) %>%
    hc_yAxis(
      #   title = list(
      #     text = paste0(col_name_pretty, " ", per_text),
      #     style = list(fontSize = "15px")
      #   ),
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
      text = paste0(col_name_pretty, " ", per_text, " by State"),
      align = "left"
    ) %>%
    hc_caption(
      text = "Some states/territories may have no data for certain years. Rankings reflect those that did submit data for a given year."
    ) %>%
    hc_plotOptions(
      series = list(
        marker = list(enabled = FALSE),
        states = list(inactive = list(enabled = FALSE)) # prevents greyout
      )
    )
})


#### HC Bar Graph ####

output$national_bar_header <- renderUI({
  tooltip(
    span(
      paste0(
        input$national_states,
        " Compared to the National Average"
      ),
      bs_icon("info-circle")
    ),
    p(
      HTML(
        paste0(
          "<b>National Average</b> is the average of totals across all states and territories that submitted data to IMLS, including ",
          input$national_states,
          "."
        )
      )
    ),
    #placement = "right"
    options = list(customClass = "wide-tooltip")
  )
})

output$national_hc_bar <- renderHighchart({
  if (selected_var() %in% currency_cols) {
    y_tt <- "${point.y:,.2f}"
    var_tt <- "${point.value:,.0f}"
    per_val_tt <- "${point.per_value:,.0f}"
    # Y axis $ prefix
  } else {
    # TODO remove decimals from non-decimal vars
    y_tt <- "{point.y:,.2f}" #
    var_tt <- "{point.value:,f}" #:,.2f
    per_val_tt <- "{point.per_value:,.f}"
  }

  if (input$national_per == "Per Capita") {
    per_total_text <- "Population of Legal Service Area"
  } else {
    per_total_text <- "Total FTE"
  }

  col_name_pretty <- input$national_var

  df <- pls_national_reactive() %>%
    filter(
      FISCAL_YEAR >= input$national_years_bar[1],
      FISCAL_YEAR <= input$national_years_bar[2]
    )

  per_text <- unique(df$per_text)

  df_ut <- df %>%
    filter(state == input$national_states) %>%
    mutate(level = state, per_text_prefix = "")

  df_natavg <- df %>%
    filter(
      FISCAL_YEAR <= imls_year
    ) %>%
    group_by(FISCAL_YEAR) %>%
    summarise(
      level = "National Average",
      value = sum(value, na.rm = T),
      per_calc = sum(per_calc, na.rm = T),
      per_value = sum(per_value, na.rm = T),
      per_avg = round(value / per_value, 2),
      per_text_prefix = "Average "
    )

  per_text <- unique(df$per_text)

  df_ut %<>%
    rename("per_avg" = "per_calc")

  highchart() %>%
    hc_add_series(
      df_ut,
      type = "column",
      color = "#FFB81D",
      hcaes(x = FISCAL_YEAR, y = per_avg, group = level)
    ) %>%
    hc_add_series(
      df_natavg,
      type = "column",
      color = "#093692",
      hcaes(x = FISCAL_YEAR, y = per_avg, group = level)
    ) %>%
    hc_tooltip(
      pointFormat = paste0(
        "<b>{series.name}</b><br>",
        "<b>",
        "{point.per_text_prefix}",
        col_name_pretty,
        " ",
        per_text,
        ": ",
        y_tt,
        "</b><br>",
        col_name_pretty, #Actual Value - e.g., "Visits: 12345"
        ": ",
        var_tt,
        "<br>",
        per_total_text, # Per category total - e.g., 'Total FTE: 1234'
        ": ",
        per_val_tt,
        "<br>",
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
      text = paste0(col_name_pretty, " ", per_text, " by State"),
      align = "left"
    ) %>%
    hc_caption(text = "Tip: click on the legend to show/hide specific groups")
})


##### Data Table #####

output$national_table_header <- renderUI({
  tooltip(
    span(
      paste0(
        input$national_dt_year,
        " ",
        input$national_var,
        " ",
        unique(pls_national_reactive()$per_text)
      ),
      bs_icon("info-circle")
    ),
    p(
      HTML(
        paste0(
          "Per capita comparisons are a way to make meaningful comparisons between libraries that serve populations of different sizes. Instead of looking at raw totals — which can be misleading — you divide a library’s data by the size of the population it serves. This shows how much service or usage occurs per person, making it easier to compare libraries on equal footing.<br>",
          "For certain comparisons, it’s useful to express data per 100, 1,000, or more people to make differences easier to interpret. In some cases — especially with smaller libraries — the standardized rate may exceed the total population served, resulting in a comparison value that is higher than the actual number."
        )
      )
    ),
    options = list(customClass = "wide-tooltip")
  )
})

output$national_dt <- renderReactable({
  req(input$national_var)
  var_name <- input$national_var
  per_text <- unique(pls_national_reactive()$per_text)

  df <- pls_national_reactive() %>%
    filter(FISCAL_YEAR == input$national_dt_year) %>%
    select(
      # Year = FISCAL_YEAR,
      State = state,
      Population_Service_Area = POPU_LSA,
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
        State = colDef(
          name = "State/Territory",
          filterable = TRUE
        ),
        value = colDef(
          name = var_name,
          cell = function(value) {
            if (isTRUE(selected_var() %in% currency_cols)) {
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
        per_calc = colDef(
          name = paste0(var_name, " ", per_text),
          cell = function(value) {
            if (isTRUE(selected_var() %in% currency_cols)) {
              paste0("$", format(value, big.mark = ","))
            } else if (is.na(value)) {
              "No Data"
            } else {
              format(value, big.mark = ",")
            }
          }
        ),
        rank = colDef(
          name = "Rank",
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


##### National Map #####

pls_national_map_reactive <- reactive({
  pls_national_state_map %>%
    mutate(
      per_name_pretty = case_when(
        per_name == "POP_col" ~ "Per Capita",
        per_name == "FTE_col" ~ "Per FTE"
      )
    ) %>%
    filter(
      YEAR == input$national_map_year,
      var == selected_var(),
      per_name_pretty == input$national_per
    )
})

#### Datawrapper Map Testing ####
# new_choropleth_chart <- dw_create_chart(
#   title = "This is a automated choropleth map",
#   type = "d3-maps-choropleth"
# )

output$frame <- renderUI({
  dw_data_to_chart(
    pls_national_map_reactive(),
    chart_id = "MMRWV" #new_choropleth_chart
  )

  dw_edit_chart(
    "MMRWV", #new_choropleth_chart,
    axes = list(
      keys = "state",
      values = "percap"
    ),
    visualize = list(
      basemap = "us-states",
      "map-key-attr" = "name",
      tooltip = list(
        body = "{{ state }} has value {{ percap }}.",
        title = "{{ var }}",
        fields = list(
          "state" = "state",
          "percap" = "percap",
          "var" = "var"
        )
      )
    )
  )

  dw_publish_chart("MMRWV") #new_choropleth_chart)

  #input$Member
  my_test <- tags$iframe(
    src = "https://datawrapper.dwcdn.net/MMRWV/",
    height = 600,
    width = 535
  )
  print(my_test)
  my_test
})


#### Leaflet Map #####

output$national_map_header <- renderUI({
  tooltip(
    span(
      paste0(
        input$national_map_year,
        " ",
        input$national_var,
        " ",
        unique(pls_national_map_reactive()$pc_text)
      ),
      bs_icon("info-circle")
    ),
    p(
      HTML(
        paste0(
          "Per capita comparisons are a way to make meaningful comparisons between libraries that serve populations of different sizes. Instead of looking at raw totals — which can be misleading — you divide a library’s data by the size of the population it serves. This shows how much service or usage occurs per person, making it easier to compare libraries on equal footing.<br>",
          "For certain comparisons, it’s useful to express data per 100, 1,000, or more people to make differences easier to interpret. In some cases — especially with smaller libraries — the standardized rate may exceed the total population served, resulting in a comparison value that is higher than the actual number."
        )
      )
    ),
    options = list(customClass = "wide-tooltip")
  )
})

output$national_map <- renderLeaflet({
  req(input$national_var)
  var_name <- input$national_var

  df <- pls_national_map_reactive() %>%
    mutate(
      label = paste0(
        "<table>
                    <div style='font-size: 18px;'><b>",
        state,
        "</div>
                    <div style='font-size: 12px;'>",
        "Rank: ",
        rank,
        "/",
        n,
        "<br>",
        var_name,
        " ",
        pc_text,
        ": ",
        format(round(per_calc, 2), big.mark = ","),
        "<br>",
        YEAR,
        "</div>
    </table>"
      )
    )

  per_text <- unique(df$pc_text)

  pal <- colorBin("Blues", domain = df$per_calc, 9)

  map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    #addTiles() %>%
    setView(-98.483330, 38.712046, zoom = 4) %>%
    # addProviderTiles(
    #   "CartoDB.Positron",
    #   group = "CartoDB.Positron"
    # ) %>%
    onRender(
      "function(el, x) {
          L.control.zoom({position:'bottomright'}).addTo(this);
        }"
    ) %>%
    addPolygons(
      data = df,
      ### TODO - pretty label
      label = ~ lapply(df$label, HTML),
      fillColor = ~ pal(df$per_calc),
      fillOpacity = 1,
      weight = .1,
      smoothFactor = .2,
    )

  map
})


##### Map Tab Data Table #####

output$national_map_dt <- renderReactable({
  req(input$national_var)
  var_name <- input$national_var

  df <- pls_national_reactive() %>%
    filter(FISCAL_YEAR == input$national_map_year) %>%
    select(
      Year = FISCAL_YEAR,
      State = state,
      per_calc,
      rank,
      n
    )

  per_text <- unique(pls_national_reactive()$per_text)

  # Render reactable
  df %>%
    reactable(
      resizable = TRUE,
      pagination = FALSE,
      sortable = FALSE,
      defaultSorted = list(rank = "asc"),
      highlight = TRUE,
      height = '63vh',
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
        state = colDef(
          name = "State/Territory",
          filterable = TRUE
        ),
        value = colDef(
          name = var_name,
          cell = function(value) {
            if (isTRUE(selected_var() %in% currency_cols)) {
              dollar(value)
            } else if (is.na(value)) {
              "No Data"
            } else {
              format(value, big.mark = ",")
            }
          }
        ),
        Year = colDef(show = FALSE),
        Population_Service_Area = colDef(
          name = "Legal Service Area Population",
          cell = function(value) {
            format(value, big.mark = ",")
          }
        ),
        per_calc = colDef(
          name = paste0(var_name, " ", per_text),
          cell = function(value) {
            if (isTRUE(selected_var() %in% currency_cols)) {
              paste0("$", format(value, big.mark = ","))
            } else if (is.na(value)) {
              "No Data"
            } else {
              format(value, big.mark = ",")
            }
          }
        ),
        rank = colDef(
          name = "Rank",
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
