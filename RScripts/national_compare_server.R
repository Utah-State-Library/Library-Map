##### National #####

##### Reactives #####

## Pull shortname for selected variable
selected_var <- reactive({
  variable_key %>% filter(INDICATOR == input$national_var) %>% pull(SHORTNAME)
})

## Filter state level dataset based on selected variable and which 'per' value they choose (per capita vs per FTE)
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


#### HC Bar Graph ####

## Header for the card that the graph will sit in
output$national_bar_header <- renderUI({
  tooltip(
    span(
      paste0(
        input$national_states,
        " Compared to the National Median"
      ),
      bs_icon("info-circle")
    ),
    p(
      HTML(
        paste0(
          "<b>All States</b> shows the median value across all states and territories that submitted data to IMLS, including ",
          input$national_states,
          ".",
          " This graph treats each state as a single system, with values from each library contributing to their respective statewide total."
        )
      )
    ),
    options = list(customClass = "wide-tooltip") # style defined in our css file
  )
})

## Bar graph
output$national_hc_bar <- renderHighchart({
  # Style the tooltip so that currency has a $ in front of it
  if (selected_var() %in% currency_cols) {
    y_tt <- "${point.y:,.2f}" # this is highcharter syntax, point.y refers to our y variable, and the .2f is how many decimal places we want
    var_tt <- "${point.value:,.0f}"
    # Y axis $ prefix
  } else {
    # TODO remove decimals from non-decimal vars
    y_tt <- "{point.y:,.2f}"
    var_tt <- "{point.value:,f}"
  }

  per_val_tt <- "{point.per_value:,.f}"

  if (input$national_per == "Per Capita") {
    per_total_text <- "Population of Legal Service Area"
  } else {
    per_total_text <- "FTE"
  }

  col_name_pretty <- input$national_var

  df <- pls_national_reactive() %>%
    filter(
      FISCAL_YEAR >= input$national_years_bar[1],
      FISCAL_YEAR <= input$national_years_bar[2]
    )

  per_text <- unique(df$per_text)

  # add some columns in to standardize across our dfs that we're using in the chart (see df_nat below)
  df_ut <- df %>%
    filter(state == input$national_states) %>%
    mutate(level = state, per_median = per_calc, per_text_prefix = "")

  df_nat <- df %>%
    filter(
      FISCAL_YEAR <= imls_year
    ) %>%
    group_by(FISCAL_YEAR) %>%
    summarise(
      level = "All States",
      per_median = round(median(per_calc, na.rm = T), 2),
      per_text_prefix = "Median "
    )

  per_text <- unique(df$per_text)

  ## Create the chart

  highchart() %>%
    hc_add_series(
      df_ut, # state data (really it can be any state, but I called it df_ut bc I was charting only UT data at first)
      type = "column",
      color = "#FFB81D",
      hcaes(x = FISCAL_YEAR, y = per_median, group = level)
    ) %>%
    hc_add_series(
      df_nat, # National median
      type = "column",
      color = "#093692",
      hcaes(x = FISCAL_YEAR, y = per_median, group = level)
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
    hc_caption(
      text = "Tip: click on the legend to show/hide specific groups"
    ) %>%
    hc_exporting(
      enabled = TRUE,
      filename = paste0(col_name_pretty, "_", per_text, "_state_compare_chart")
    )
})


##### Data Table #####

# Header for the card that the table sits in
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
          "<b>Per Capita</b> shows how much service or usage occurs per person served by a given library, making it easier to compare libraries on equal footing. <br><br>",
          "<b>Per FTE</b> shows how much service or usage occurs per Full Time Equivalent (FTE). One FTE is equal to a full work week. FTE is not necessarily equal to the number of staff working at a library because some staff may be part-time.",
          "<br><br>",
          "Use the gear icon at the top right to change the year."
        )
      )
    ),
    options = list(customClass = "wide-tooltip")
  )
})

# CSV download button for the table
output$nationalcomp_dt_csv_button <- renderUI({
  filename <- paste0(
    input$national_var,
    " ",
    unique(pls_national_reactive()$per_text),
    " State Rankings.csv"
  )

  csvDownloadButton("national_dt", filename = filename)
})

# Create the table
output$national_dt <- renderReactable({
  req(input$national_var)
  var_name <- input$national_var
  per_text <- unique(pls_national_reactive()$per_text)

  df <- pls_national_reactive() %>%
    filter(FISCAL_YEAR == input$national_dt_year) %>%
    select(
      year = FISCAL_YEAR,
      state,
      population_service_area = POPU_LSA,
      variable = var, # hide col in reactable, include for download
      comparison_method = per_name_pretty, # hide col in reactable, include for download
      actual_variable_value = value,
      comparison_value = per_calc,
      per_text, # hide col in reactable, include for download
      rank,
      n
    ) %>%
    mutate(variable = var_name)

  # Render reactable
  df %>%
    reactable(
      resizable = TRUE,
      pagination = FALSE,
      sortable = FALSE,
      defaultSorted = list(rank = "asc"),
      highlight = TRUE,
      virtual = TRUE,
      height = 500,
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
        year = colDef(show = FALSE),
        state = colDef(
          name = "State/Territory",
          filterable = TRUE,
          style = list(backgroundColor = "#f7f7f7")
        ),
        variable = colDef(show = FALSE),
        comparison_method = colDef(show = FALSE),
        actual_variable_value = colDef(
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
        population_service_area = colDef(
          name = "Legal Service Area Population",
          cell = function(value) {
            format(value, big.mark = ",")
          }
        ),
        comparison_value = colDef(
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
        per_text = colDef(show = FALSE),
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

# This isn't currently used on the UI side, but it's here if you want to revive it! I wasn't thrilled with the map projection and how it showed alaska and hawaii, which is why I have it hidden for now
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
# This is a very satisfying map projection, but it's not built for reactivity, so I'm leaving this here just for reference

# new_choropleth_chart <- dw_create_chart(
#   title = "This is a automated choropleth map",
#   type = "d3-maps-choropleth"
# )

# output$frame <- renderUI({
#   dw_data_to_chart(
#     pls_national_map_reactive(),
#     chart_id = "MMRWV" #new_choropleth_chart
#   )

#   dw_edit_chart(
#     "MMRWV", #new_choropleth_chart,
#     axes = list(
#       keys = "state",
#       values = "percap"
#     ),
#     visualize = list(
#       basemap = "us-states",
#       "map-key-attr" = "name",
#       tooltip = list(
#         body = "{{ state }} has value {{ percap }}.",
#         title = "{{ var }}",
#         fields = list(
#           "state" = "state",
#           "percap" = "percap",
#           "var" = "var"
#         )
#       )
#     )
#   )

#   dw_publish_chart("MMRWV") #new_choropleth_chart)

#   #input$Member
#   my_test <- tags$iframe(
#     src = "https://datawrapper.dwcdn.net/MMRWV/",
#     height = 600,
#     width = 535
#   )
#   print(my_test)
#   my_test
# })

#### Leaflet Map #####
# This is that leaflet map that isn't in the UI because I didn't like the projection
# If you want to revive this and if you want to use Leaflet with the CARTO basemap you will need to add the API key in (see state_service_server.R)

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
          "<b>Per Capita</b> shows how much service or usage occurs per person served by a given library, making it easier to compare libraries on equal footing. <br><br>",
          "<b>Per FTE</b> shows how much service or usage occurs per Full Time Equivalent (FTE). One FTE is equal to a full work week. FTE is not necessarily equal to the number of staff working at a library because some staff may be part-time.",
          "<br><br>",
          "Use the gear icon at the top right to change the year."
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
# This is a neat little table that looks good next to a map because it shows all of the states and their ranks so people don't have to hover over every state

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
