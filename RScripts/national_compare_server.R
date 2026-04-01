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
    filter(
      var == selected_var()
    )
})

output$map_title <- renderUI({
  paste0("")
})

##### HC Graph #####

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

  percap_text <- unique(df$percap_text)

  df_ut <- df %>% filter(state == input$national_states)
  df_nut <- df %>% filter(state != input$national_states)
  df_natavg <- df %>%
    group_by(FISCAL_YEAR) %>%
    mutate(
      percap = round(mean(percap, na.rm = T), 2),
      n_states = n_distinct(state)
    ) %>%
    ungroup() %>%
    filter(n_states > 1)

  hc <- highchart() %>%
    hc_chart(zoomType = "y") %>%
    hc_add_series(
      df_ut,
      type = "line",
      color = "#4EC3E0",
      index = 2,
      lineWidth = 4,
      hcaes(x = FISCAL_YEAR, y = percap, group = state)
    )

  if (input$national_switch) {
    hc %<>%
      hc_add_series(
        df_nut,
        type = "line",
        color = "#d6d3d3ff",
        fillOpacity = .6,
        index = 1,
        lineWidth = 1,
        hcaes(x = FISCAL_YEAR, y = percap, group = state)
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
        text = paste0(col_name_pretty, " ", percap_text, " by State")
      ) %>%
      hc_caption(
        text = "Some states/territories may have no data for certain years. Rankings reflect those that did submit data for a given year."
      )
  } else {
    hc %<>%
      hc_add_series(
        df_natavg,
        name = "National Average",
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
        labels = list(style = list(fontSize = "15px"))
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
      hc_subtitle(
        text = paste0(input$national_states, " vs. National Average")
      ) %>%
      hc_caption(
        text = paste0(
          "The national average represents the average per capita value across all states, including ",
          input$national_states,
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


##### Data Table #####

output$national_dt_title <- renderUI({
  x <- HTML(paste0(
    "<b style='font-size:16px;'>",
    input$national_var,
    " ",
    unique(pls_national_reactive()$percap_text),
    " (",
    input$national_dt_year,
    ")</b>"
  ))
  x
})

output$national_dt <- renderReactable({
  req(input$national_var)
  var_name <- input$national_var
  percap_text <- unique(pls_national_reactive()$percap_text)

  df <- pls_national_reactive() %>%
    filter(FISCAL_YEAR == input$national_dt_year) %>%
    select(
      # Year = FISCAL_YEAR,
      State = state,
      Population_Service_Area = POPU_LSA,
      value,
      percap,
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
        Population_Service_Area = colDef(
          name = "Legal Service Area Population",
          cell = function(value) {
            format(value, big.mark = ",")
          }
        ),
        percap = colDef(
          name = paste0(var_name, " ", percap_text),
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
  df <- pls_national_state_map %>%
    filter(
      YEAR == input$national_map_year,
      var == selected_var()
    )
  df
})

output$national_map_title <- renderUI({
  x <- HTML(paste0(
    "<b style='font-size:16px;'>",
    input$national_var,
    " ",
    unique(pls_national_map_reactive()$pc_text),
    " (",
    input$national_map_year,
    ")</b>"
  ))
  x
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
        round(percap, 2),
        "<br>",
        YEAR,
        "</div>
    </table>"
      )
    )

  percap_text <- unique(df$pc_text)

  pal <- colorBin("Blues", domain = df$percap, 9)

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
      fillColor = ~ pal(df$percap),
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
      percap,
      rank,
      n
    )

  percap_text <- unique(pls_national_reactive()$percap_text)

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
        percap = colDef(
          name = paste0(var_name, " ", percap_text),
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
