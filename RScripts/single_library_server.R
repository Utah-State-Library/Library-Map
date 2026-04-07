##### Single Library Server #####

### Single library trends over time - totals & per capita
### Year to year change?
### Compare to all libraries in UT or peer libraries - HC & table

# select - library
# totals vs per capita view
# show just library or ut lib avgs or peers

# Resources: https://usdataexplorer.com/county/salt-lake-county-ut/
selected_var_singlib <- reactive({
  variable_key %>% filter(INDICATOR == input$singlib_var) %>% pull(SHORTNAME)
})

selected_year_singlib <- reactive({
  input$singlib_year
})

pls_peers <- reactive({
  lib <- input$singlib_library

  peers <- pls_national_simlibs %>% filter(CURRENT_LIBNAME_DISAMB == lib)

  y <- peers %>% filter(CURRENT_LIBNAME_DISAMB == lib)
  y$peers <- gsub("[0-9]", "", y$peers)
  closest <- eval(parse(text = y$peers))

  pls_national_peers %>%
    filter(
      CURRENT_LIBNAME_DISAMB == lib |
        CURRENT_LIBNAME_DISAMB %in% closest
    ) %>%
    filter(var == selected_var_singlib())
})


output$peers_hc <- renderHighchart({
  if (selected_var_singlib() %in% currency_cols) {
    y_tt <- "${point.y:,.2f}"
    var_tt <- "${point.value:,.0f}"
    # Y axis $ prefix
  } else {
    # TODO remove decimals from non-decimal vars
    y_tt <- "{point.y:,.2f}" #
    var_tt <- "{point.value:,f}" #:,.2f
  }

  col_name_pretty <- input$singlib_var
  lib <- input$singlib_library

  df_target <- pls_peers() %>%
    filter(
      CURRENT_LIBNAME_DISAMB == lib
    )
  df_peers <- pls_peers() %>%
    filter(
      CURRENT_LIBNAME_DISAMB != lib
    )

  percap_text <- unique(df_target$percap_text)

  hc <- highchart() %>%
    hc_chart(zoomType = "y") %>%
    hc_add_series(
      df_target,
      type = "line",
      color = "#4EC3E0",
      index = 2,
      lineWidth = 4,
      hcaes(x = FISCAL_YEAR, y = percap, group = CURRENT_LIBNAME_DISAMB)
    ) %>%
    hc_add_series(
      df_peers,
      type = "line",
      color = "#d6d3d3ff",
      fillOpacity = .6,
      index = 1,
      lineWidth = 1,
      hcaes(x = FISCAL_YEAR, y = percap, group = CURRENT_LIBNAME_DISAMB)
    ) %>%
    hc_legend(enabled = FALSE) %>%
    hc_tooltip(
      pointFormat = paste0(
        "<b>{series.name}</b><br>",
        #"<b>Rank: {point.rank} out of {point.n}</b><br>",
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
      text = paste0(col_name_pretty, " ", percap_text)
    ) %>%
    hc_subtitle(
      text = c(lib, " vs. Peer Libraries")
    ) %>%
    hc_plotOptions(
      series = list(
        marker = list(enabled = FALSE),
        states = list(inactive = list(enabled = FALSE)) # prevents greyout
      )
    )

  hc
})


output$peer_dt <- renderReactable({
  col_name_pretty <- input$singlib_var
  lib <- input$singlib_library

  df <- pls_national %>%
    filter(
      CURRENT_LIBNAME_DISAMB %in% pls_peers()$CURRENT_LIBNAME_DISAMB,
      FISCAL_YEAR == input$singlib_dt_year
    ) %>%
    select(
      Library = CURRENT_LIBNAME_DISAMB,
      Year = FISCAL_YEAR,
      `Population of Legal Service Area` = POPU_LSA,
      Visits = VISITS,
      Cardholders = REGBOR,
      `Total FTE of Staff` = TOTSTAFF,
      `Total Revenue` = TOTINCM
    )

  # Render reactable
  df %>%
    reactable(
      resizable = TRUE,
      pagination = FALSE,
      sortable = FALSE,
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
        Library = colDef(),
        Year = colDef(),
        `Population of Legal Service Area` = colDef(cell = function(value) {
          format(value, big.mark = ",")
        }),
        Visits = colDef(cell = function(value) {
          format(value, big.mark = ",")
        }),
        Cardholders = colDef(cell = function(value) {
          format(value, big.mark = ",")
        }),
        `Total FTE of Staff` = colDef(cell = function(value) {
          format(value, big.mark = ",")
        }),
        `Total Revenue` = colDef(cell = function(value) {
          dollar(value)
        })
      )
    )
})
