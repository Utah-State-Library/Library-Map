##### Single Library Server #####

### Single library trends over time - totals & per capita
### Year to year change?
### Compare to all libraries in UT or peer libraries - HC & table

# Resources: https://usdataexplorer.com/county/salt-lake-county-ut/

selected_var_singlib <- reactive({
  variable_key %>% filter(INDICATOR == input$singlib_var) %>% pull(SHORTNAME)
})

# selected_var_scatterX <- reactive({
#   variable_key %>% filter(INDICATOR == input$peer_varX) %>% pull(SHORTNAME)
# })

# selected_var_scatterY <- reactive({
#   variable_key %>% filter(INDICATOR == input$peer_varY) %>% pull(SHORTNAME)
# })

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
    )
})


#### Table header ####

output$peers_dt_header <- renderUI({
  tooltip(
    span(
      paste0("National Peer Libraries for ", input$singlib_library),
      bs_icon("info-circle") #, title = "About Peer Libraries"
    ),
    p(
      HTML(
        paste0(
          "Peer libraries are those that are most similar to the selected library.<br><br>",
          "Using data from the most recent year available (2023), five data points were used to calculate the similarity between libraries nationwide. See the methodology page for more information on how peer libraries were calculated.<br><br>
          
          For each library, the 10 most similar libraries were identified on the basis of the following data points.<br><br>",
          "- Population of Legal Service Area<br>
      - Total FTE of Staff<br>
      - Total Revenue<br>
      - Number of Cardholders<br>
      - Number of Visits" #,

          # "Similarity is based on 2023 data (the most recent year available), and libraries that are similar to each other in this year may not be as similar if calculating using a different year. Data points were first standardized, and similarity scores were calculated using Manhattan Distance.",
        )
      )
    ),
    #placement = "right"
    options = list(customClass = "wide-tooltip")
  )
})


#### Peer Line Graph ####

output$peers_line_header <- renderUI({
  df <- pls_peers() %>%
    mutate(
      per_name_pretty = case_when(
        per_name == "POP_col" ~ "Per Capita",
        per_name == "FTE_col" ~ "Per FTE"
      )
    ) %>%
    filter(var == selected_var_singlib(), per_name_pretty == input$singlib_per)

  pc_text <- unique(df$per_text)

  paste0(input$singlib_var, " ", pc_text, " Compared to Peer Libraries")
})

output$peers_hc <- renderHighchart({
  if (selected_var_singlib() %in% currency_cols) {
    y_tt <- "${point.y:,.2f}"
    var_tt <- "${point.value:,.0f}"
  } else {
    y_tt <- "{point.y:,.2f}"
    var_tt <- "{point.value:,f}"
  }

  col_name_pretty <- input$singlib_var
  lib <- input$singlib_library

  df <- pls_peers() %>%
    mutate(
      per_name_pretty = case_when(
        per_name == "POP_col" ~ "Per Capita",
        per_name == "FTE_col" ~ "Per FTE"
      ),
      state_name = state
    ) %>%
    filter(var == selected_var_singlib(), per_name_pretty == input$singlib_per)

  df_target <- df %>%
    filter(
      CURRENT_LIBNAME_DISAMB == lib
    )
  df_peers <- df %>%
    filter(
      CURRENT_LIBNAME_DISAMB != lib
    )

  per_text <- unique(df_target$per_text)

  hc <- highchart() %>%
    hc_chart(zoomType = "y") %>%
    hc_add_series(
      df_target,
      type = "line",
      color = "#81D0F0",
      index = 2,
      lineWidth = 4,
      hcaes(x = FISCAL_YEAR, y = per_calc, group = CURRENT_LIBNAME_DISAMB)
    ) %>%
    hc_add_series(
      df_peers,
      type = "line",
      color = "#d6d3d3ff",
      fillOpacity = .6,
      index = 1,
      lineWidth = 1,
      hcaes(x = FISCAL_YEAR, y = per_calc, group = CURRENT_LIBNAME_DISAMB)
    ) %>%
    hc_legend(enabled = FALSE) %>%
    hc_tooltip(
      pointFormat = paste0(
        "<b>{series.name}</b><br>",
        "<b>{point.state_name}</b><br>",
        #"<b>Rank: {point.rank} out of {point.n}</b><br>",
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
      title = list(
        text = paste0(col_name_pretty, " ", per_text),
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
    hc_plotOptions(
      series = list(
        marker = list(enabled = FALSE),
        states = list(inactive = list(enabled = FALSE)) # prevents greyout
      )
    )

  hc
})

#### Peer Scatter Plot ####

# output$peer_scatter_hc <- renderHighchart({
#   if (selected_var_scatterY() %in% currency_cols) {
#     y_tt <- "${point.y:,.2f}"
#     yvar_tt <- "${point.value_y:,.0f}"
#   } else {
#     y_tt <- "{point.y:,.2f}"
#     yvar_tt <- "{point.value_y:,f}"
#   }

#   if (selected_var_scatterX() %in% currency_cols) {
#     x_tt <- "${point.x:,.2f}"
#     xvar_tt <- "${point.value_x:,.0f}"
#   } else {
#     x_tt <- "{point.x:,.2f}"
#     xvar_tt <- "{point.value_x:,f}"
#   }

#   col_name_pretty_x <- input$peer_varX
#   col_name_pretty_y <- input$peer_varY
#   lib <- input$singlib_library

#   df <- pls_peers() %>%
#     filter(
#       var %in% c(selected_var_scatterX(), selected_var_scatterY()),
#       FISCAL_YEAR == (current_year - 1)
#     ) %>%
#     mutate(
#       var = case_when(
#         var == selected_var_scatterX() ~ "x",
#         var == selected_var_scatterY() ~ "y",
#       )
#     )

#   df %<>%
#     select(-percap_multiplier) %>%
#     pivot_wider(
#       names_from = "var",
#       values_from = c("percap", "value", "percap_text")
#     )

#   df_target <- df %>%
#     filter(
#       CURRENT_LIBNAME_DISAMB == lib
#     )
#   df_peers <- df %>%
#     filter(
#       CURRENT_LIBNAME_DISAMB != lib
#     )

#   percap_text_x <- unique(df_target$percap_text_x)
#   percap_text_y <- unique(df_target$percap_text_y)

#   hc <- highchart() %>%
#     hc_chart(zoomType = "y") %>%
#     hc_add_series(
#       df_target,
#       type = "scatter",
#       color = "#81D0F0",
#       hcaes(x = percap_x, y = percap_y, group = CURRENT_LIBNAME_DISAMB)
#     ) %>%
#     hc_add_series(
#       df_peers,
#       type = "scatter",
#       color = "rgb(0, 0, 0)",
#       hcaes(x = percap_x, y = percap_y, group = CURRENT_LIBNAME_DISAMB)
#     ) %>%
#     hc_legend(enabled = FALSE) %>%
#     hc_tooltip(
#       pointFormat = paste0(
#         "<b>{series.name}</b><br>",
#         "<b>",
#         col_name_pretty_x,
#         " {point.percap_text_x}",
#         ": ",
#         x_tt,
#         "<br>",
#         col_name_pretty_y,
#         " {point.percap_text_y}",
#         ": ",
#         y_tt,
#         "</b><br>",
#         col_name_pretty_x,
#         ": ",
#         xvar_tt,
#         "<br>",
#         col_name_pretty_y,
#         ": ",
#         yvar_tt,
#         "<br>",
#         "Legal Service Area Population: {point.POPU_LSA:,.0f}<br>",
#         "{point.FISCAL_YEAR}"
#       ),
#       headerFormat = ""
#     ) %>%
#     hc_yAxis(
#       title = list(
#         text = paste0(col_name_pretty_y, " ", percap_text_y),
#         style = list(fontSize = "15px")
#       ),
#       labels = list(
#         style = list(fontSize = "15px")
#       )
#     ) %>%
#     hc_xAxis(
#       title = list(
#         text = paste0(col_name_pretty_x, " ", percap_text_x),
#         style = list(fontSize = "15px")
#       ),
#       labels = list(
#         style = list(fontSize = "15px")
#       )
#     )

#   hc
# })

#### Peer DT ####

output$peer_dt <- renderReactable({
  col_name_pretty <- input$singlib_var
  lib <- input$singlib_library

  df <- pls_national %>%
    filter(
      CURRENT_LIBNAME_DISAMB %in% pls_peers()$CURRENT_LIBNAME_DISAMB,
      FISCAL_YEAR == imls_year
    ) %>%
    select(
      Library = CURRENT_LIBNAME_DISAMB,
      State = state,
      Year = FISCAL_YEAR,
      `Population of Legal Service Area` = POPU_LSA,
      Visits = VISITS,
      `Registered Users` = REGBOR,
      `Total FTE of Staff` = TOTSTAFF,
      `Total Revenue` = TOTINCM
    )

  df1 <- df %>% filter(Library == lib)
  df2 <- df %>%
    filter(Library != lib) %>%
    arrange(Library)

  df_r <- rbind(df1, df2)

  # Render reactable
  df_r %>%
    reactable(
      resizable = TRUE,
      pagination = FALSE,
      sortable = TRUE,
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
        Library = colDef(minWidth = 150, style = function(value) {
          if (value == lib) {
            fontweight = "bold"
          } else {
            fontweight = 300
          }
          list(fontWeight = fontweight, backgroundColor = "#f7f7f7")
        }),
        State = colDef(),
        Year = colDef(show = FALSE),
        `Population of Legal Service Area` = colDef(cell = function(value) {
          format(value, big.mark = ",")
        }),
        Visits = colDef(cell = function(value) {
          format(value, big.mark = ",")
        }),
        `Registered Users` = colDef(cell = function(value) {
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


#### Bar Chart ####

output$peers_bar_header <- renderUI({
  tooltip(
    span(
      paste0(
        input$singlib_library,
        " Compared to Peer, State, and National Averages"
      ),
      bs_icon("info-circle")
    ),
    p(
      HTML(
        paste0(
          "<b>Peer Libraries</b> are the 10 most similar libraries to the selected library. The table below lists each of these peers.<br>",
          "<b>State Libraries</b> are all libraries in the same state as the selected library. The state average includes data for the selected library.<br>",
          "<b>National Libraries</b> are all libraries in the country that contribute data to IMLS. Libraries span all 50 states, Washington DC, and several territories.<br>"
        )
      )
    ),
    options = list(customClass = "wide-tooltip")
  )
})


output$peer_hc_bar <- renderHighchart({
  if (selected_var_singlib() %in% currency_cols) {
    y_tt <- "${point.y:,.2f}"
    var_tt <- "${point.value:,.0f}"
  } else {
    y_tt <- "{point.y:,.2f}"
    var_tt <- "{point.value:,f}"
  }

  per_val_tt <- "{point.per_value:,.f}"

  if (input$singlib_per == "Per Capita") {
    per_total_text <- "Population of Legal Service Area"
  } else {
    per_total_text <- "FTE"
  }

  col_name_pretty <- input$singlib_var
  lib <- input$singlib_library

  df <- pls_peers() %>%
    mutate(
      per_name_pretty = case_when(
        per_name == "POP_col" ~ "Per Capita",
        per_name == "FTE_col" ~ "Per FTE"
      ),
      state_name = state
    ) %>%
    filter(var == selected_var_singlib(), per_name_pretty == input$singlib_per)

  per_text <- unique(df$per_text)

  df_target <- df %>%
    filter(
      CURRENT_LIBNAME_DISAMB == lib
    ) %>%
    mutate(
      level = CURRENT_LIBNAME_DISAMB,
      per_text_prefix = "",
      per_value_prefix = ""
    )

  df_peers <- df %>%
    filter(
      CURRENT_LIBNAME_DISAMB != lib,
      FISCAL_YEAR <= imls_year
    ) %>%
    group_by(FISCAL_YEAR) %>%
    summarise(
      level = "Peer Libraries",
      value = sum(value, na.rm = T),
      per_calc = sum(per_calc, na.rm = T),
      per_value = sum(per_value, na.rm = T),
      per_avg = round(value / per_value, 2),
      per_text_prefix = "Average ",
      per_value_prefix = "Total "
    )

  df_nat <- pls_national_peers %>%
    mutate(
      per_name_pretty = case_when(
        per_name == "POP_col" ~ "Per Capita",
        per_name == "FTE_col" ~ "Per FTE"
      )
    ) %>%
    filter(
      var == selected_var_singlib(),
      per_name_pretty == input$singlib_per,
      FISCAL_YEAR <= imls_year
    ) %>%
    group_by(FISCAL_YEAR) %>%
    summarise(
      level = "National Average",
      value = sum(value, na.rm = T),
      per_calc = sum(per_calc, na.rm = T),
      per_value = sum(per_value, na.rm = T),
      per_avg = round(value / per_value, 2),
      per_text_prefix = "Average ",
      per_value_prefix = "Total "
    )

  df_state <- pls_national_peers %>%
    mutate(
      per_name_pretty = case_when(
        per_name == "POP_col" ~ "Per Capita",
        per_name == "FTE_col" ~ "Per FTE"
      )
    ) %>%
    filter(
      var == selected_var_singlib(),
      per_name_pretty == input$singlib_per,
      state == unique(df_target$state_name)
    ) %>%
    group_by(FISCAL_YEAR) %>%
    summarise(
      level = paste0(state, " Libraries"),
      value = sum(value, na.rm = T),
      per_calc = sum(per_calc, na.rm = T),
      per_value = sum(per_value, na.rm = T),
      per_avg = round(value / per_value, 2),
      per_text_prefix = "Average ",
      per_value_prefix = "Total "
    ) %>%
    distinct()

  df_target %<>%
    rename("per_avg" = "per_calc")

  highchart() %>%
    hc_add_series(
      df_target,
      type = "column",
      color = "#FFB81D",
      hcaes(x = FISCAL_YEAR, y = per_avg, group = level)
    ) %>%
    hc_add_series(
      df_peers,
      type = "column",
      color = "#81D0F0",
      hcaes(x = FISCAL_YEAR, y = per_avg, group = level)
    ) %>%
    hc_add_series(
      df_state,
      type = "column",
      color = "#0086BF",
      hcaes(x = FISCAL_YEAR, y = per_avg, group = level)
    ) %>%
    hc_add_series(
      df_nat,
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
        "{point.per_value_prefix}",
        col_name_pretty, #Actual Value - e.g., "Visits: 12345"
        ": ",
        var_tt,
        "<br>",
        "{point.per_value_prefix}",
        per_total_text, # Per category total - e.g., 'Total FTE: 1234'
        ": ",
        per_val_tt,
        "<br>",
        "{point.x}"
      ),
      headerFormat = ""
    ) %>%
    hc_yAxis(
      title = list(
        text = paste0(col_name_pretty, " ", per_text),
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
    hc_caption(text = "Tip: click on the legend to show/hide specific groups")
})


#### testing multi drop

output$peer_hc_bar_multidrop <- renderHighchart({
  # if (selected_var_singlib() %in% currency_cols) {
  #   y_tt <- "${point.y:,.2f}"
  #   var_tt <- "${point.value:,.0f}"
  # } else {
  #   y_tt <- "{point.y:,.2f}"
  #   var_tt <- "{point.value:,f}"
  # }

  #col_name_pretty <- input$singlib_var
  lib <- "American Fork City Library" #input$singlib_library

  df <- pls_peers %>% #() %>%
    #filter(var == selected_var_singlib()) %>%
    mutate(
      state_name = state,
      per_name_pretty = case_when(
        per_name == "POPU_LSA" ~ "Per Capita",
        per_name == "FTE_col" ~ "Per FTE"
      )
    ) %>%
    left_join(variable_key, by = c("var" = "SHORTNAME"))

  #percap_text <- unique(df$percap_text)

  df_target <- df %>%
    filter(
      CURRENT_LIBNAME_DISAMB == lib
    ) %>%
    mutate(level = CURRENT_LIBNAME_DISAMB, percap_text_prefix = "")

  df_peers <- df %>%
    filter(
      CURRENT_LIBNAME_DISAMB != lib,
      FISCAL_YEAR <= imls_year
    ) %>%
    group_by(FISCAL_YEAR, var, per_name) %>%
    summarise(
      level = "Peer Libraries",
      INDICATOR = unique(INDICATOR),
      per_avg = round(mean(per_calc, na.rm = T), 2),
      percap_text_prefix = "Average ",
      per_name = unique(per_name),
      per_name_pretty = unique(per_name_pretty),
      per_text = unique(per_text)
    )

  df_nat <- pls_national_peers %>%
    left_join(variable_key, by = c("var" = "SHORTNAME")) %>%
    mutate(
      per_name_pretty = case_when(
        per_name == "POPU_LSA" ~ "Per Capita",
        per_name == "FTE_col" ~ "Per FTE"
      )
    ) %>%
    filter(FISCAL_YEAR <= imls_year) %>%
    group_by(FISCAL_YEAR, var, per_name) %>%
    summarise(
      level = "National Libraries",
      INDICATOR = unique(INDICATOR),
      per_avg = round(mean(per_calc, na.rm = T), 2),
      percap_text_prefix = "Average ",
      per_name = unique(per_name),
      per_name_pretty = unique(per_name_pretty),
      per_text = unique(per_text)
    ) %>%
    distinct()

  df_state <- pls_national_peers %>%
    left_join(variable_key, by = c("var" = "SHORTNAME")) %>%
    mutate(
      per_name_pretty = case_when(
        per_name == "POPU_LSA" ~ "Per Capita",
        per_name == "FTE_col" ~ "Per FTE"
      )
    ) %>%
    filter(
      state == unique(df_target$state_name)
    ) %>%
    group_by(FISCAL_YEAR, var, per_name) %>%
    summarise(
      level = paste0(state, " Libraries"),
      INDICATOR = unique(INDICATOR),
      per_avg = round(mean(per_calc, na.rm = T), 2),
      percap_text_prefix = "Average ",
      per_name = unique(per_name),
      per_name_pretty = unique(per_name_pretty),
      per_text = unique(per_text)
    ) %>%
    distinct()

  df_target %<>%
    rename("per_avg" = "per_calc")
  #select(level, FISCAL_YEAR, percap_avg = percap, percap_text_prefix)

  highchart() %>%
    hc_add_series(
      df_target,
      type = "column",
      color = "#FFB81D",
      hcaes(x = FISCAL_YEAR, y = per_avg, group = level)
    ) %>%
    hc_add_series(
      df_peers,
      type = "column",
      color = "#81D0F0",
      hcaes(x = FISCAL_YEAR, y = per_avg, group = level)
    ) %>%
    hc_add_series(
      df_state,
      type = "column",
      color = "#0086BF",
      hcaes(x = FISCAL_YEAR, y = per_avg, group = level)
    ) %>%
    hc_add_series(
      df_nat,
      type = "column",
      color = "#093692",
      hcaes(x = FISCAL_YEAR, y = per_avg, group = level)
    ) %>%
    hc_tooltip(
      pointFormat = paste0(
        "<b>{series.name}</b><br>",
        "<b>",
        "{point.percap_text_prefix}",
        "{point.INDICATOR}",
        " ",
        "{point.per_text}",
        ": ",
        "{point.y}",
        #y_tt,
        "</b><br>",
        "{point.x}"
      ),
      headerFormat = ""
    ) %>%
    hc_yAxis(
      title = list(
        #text = paste0(col_name_pretty, " ", percap_text),
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
    hc_caption(
      text = "Tip: click on the legend to show/hide specific groups"
    ) %>%
    add_multi_drop(c("INDICATOR", "per_name_pretty"))
})
