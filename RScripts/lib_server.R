##### Library Compare #####

selected_var_libcompare <- reactive({
  variable_key %>% filter(INDICATOR == input$var_libcompare) %>% pull(SHORTNAME)
})

library_state_libcompare <- reactive({
  pls_national_peers %>% 
    filter(CURRENT_LIBNAME_DISAMB == input$library_libcompare) %>% 
    select(state) %>% 
    unique() %>% 
    pull()
})

library_name_pretty_libcompare <- reactive({
  lib <- input$library_libcompare

  pls_national_peers %>%
    filter(
      CURRENT_LIBNAME_DISAMB == lib
    ) %>% select(CURRENT_LIBNAME) %>% unique() %>% pull()
})

observe({ #if Utah, National table year is current year

  year <- if (library_state_libcompare() == "Utah") {
    current_year
  } else {
    imls_year
  }

  updateSliderInput(
    session,
    inputId = "libcompare_national_dt_year",
    label = "Select a Year",
    value = year,
  )
})

observe({ #if Utah, State Peer table year is current year

  year <- if (library_state_libcompare() == "Utah") {
    current_year
  } else {
    imls_year
  }

  updateSliderInput(
    session,
    inputId = "libcompare_statepeers_dt_year",
    label = "Select a Year",
    value = year,
  )
})

observe({ #if Utah, State Lib table year is current year

  year <- if (library_state_libcompare() == "Utah") {
    current_year
  } else {
    imls_year
  }

  updateSliderInput(
    session,
    inputId = "libcompare_state_dt_year",
    label = "Select a Year",
    value = year,
  )
})

libcompare_reactive <- reactive({
  pls_national_peers %>%
    mutate(
      per_name_pretty = case_when(
        per_name == "POP_col" ~ "Per Capita",
        per_name == "FTE_col" ~ "Per FTE"
      )
    ) %>%
    filter(
      var == selected_var_libcompare(),
      per_name_pretty == input$per_libcompare
    )
})


libcompare_peers_state <- reactive({
  lib <- input$library_libcompare

  peers <- pls_national_simlibs %>% filter(CURRENT_LIBNAME_DISAMB == lib)

  y <- peers %>% select(CURRENT_LIBNAME_DISAMB, peers = state_peers)

  closest <- eval(parse(text = y$peers))

  pls_national_peers %>%
    filter(
      CURRENT_LIBNAME_DISAMB == lib |
        CURRENT_LIBNAME_DISAMB %in% closest
    )
})

libcompare_peers_national <- reactive({
  lib <- input$library_libcompare

  peers <- pls_national_simlibs %>% filter(CURRENT_LIBNAME_DISAMB == lib)

  y <- peers %>% select(CURRENT_LIBNAME_DISAMB, peers)

  closest <- eval(parse(text = y$peers))

  pls_national_peers %>%
    filter(
      CURRENT_LIBNAME_DISAMB == lib |
        CURRENT_LIBNAME_DISAMB %in% closest
    )
})


#### Bar Chart ####

output$libcompare_bar_header <- renderUI({
  tooltip(
    span(
      paste0(
        library_name_pretty_libcompare(),
        " Compared to State and National Library Medians"
      ),
      bs_icon("info-circle")
    ),
    p(
      HTML(
        paste0("<b>State Libraries</b> are all libraries in the same state as the selected library. The state library median is the median value across all libraries in the state, including the selected library.<br>",
          "<b>National Libraries</b> are all libraries in the country that contribute data to IMLS. Libraries span all 50 states, Washington DC, and several territories. The national median is the median value across all libraries in the nation, including the selected library."
        )
      )
    ),
    options = list(customClass = "wide-tooltip")
  )
})


output$libcompare_hc_bar <- renderHighchart({
  if (selected_var_libcompare() %in% currency_cols) {
    y_tt <- "${point.y:,.2f}"
    var_tt <- "${point.value:,.0f}"
  } else {
    y_tt <- "{point.y:,.2f}"
    var_tt <- "{point.value:,f}"
  }

  if (input$per_libcompare == "Per Capita") {
    per_total_text <- "Population of Legal Service Area"
  } else {
    per_total_text <- "FTE"
  }


  # if (input$peerlevel_libcompare == "Statewide Peers"){
  #   peer_year <- max(libcompare_peers()$FISCAL_YEAR)
  # } else {
  #   peer_year <- imls_year
  # }

  col_name_pretty <- input$var_libcompare
  lib <- input$library_libcompare

  df <- libcompare_reactive() %>%
    mutate(state_name = state) ### do not remove, "state" is also an hc call and things get weird

  per_text <- unique(df$per_text)

  
  # df_peers_state <- df_state %>%
  #   filter(
  #     CURRENT_LIBNAME_DISAMB != lib,
  #     FISCAL_YEAR <= peer_year #imls_year
  #   ) %>%
  #   group_by(FISCAL_YEAR) %>%
  #   summarise(
  #     level = "State Peers",
  #     per_median = round(median(per_calc, na.rm = T), 2),
  #     per_text_prefix = "Median "
  #   )
  
  #   df_peers_national <- df_national %>%
  #   filter(
  #     CURRENT_LIBNAME_DISAMB != lib,
  #     FISCAL_YEAR <= peer_year #imls_year
  #   ) %>%
  #   group_by(FISCAL_YEAR) %>%
  #   summarise(
  #     level = "National Peers",
  #     per_median = round(median(per_calc, na.rm = T), 2),
  #     per_text_prefix = "Median "
  #   )

  df_target <- df %>%
    filter(
      CURRENT_LIBNAME_DISAMB == lib
    ) %>%
    mutate(
      level = CURRENT_LIBNAME,
      per_text_prefix = ""
    )

  df_nat <- pls_national_peers %>%
    mutate(
      per_name_pretty = case_when(
        per_name == "POP_col" ~ "Per Capita",
        per_name == "FTE_col" ~ "Per FTE"
      )
    ) %>%
    filter(
      var == selected_var_libcompare(),
      per_name_pretty == input$per_libcompare,
      FISCAL_YEAR <= imls_year
    ) %>%
    group_by(FISCAL_YEAR) %>%
    summarise(
      level = "National Libraries",
      per_median = round(median(per_calc, na.rm = T), 2),
      per_text_prefix = "Median "
    )

  df_state <- pls_national_peers %>%
    mutate(
      per_name_pretty = case_when(
        per_name == "POP_col" ~ "Per Capita",
        per_name == "FTE_col" ~ "Per FTE"
      )
    ) %>%
    filter(
      var == selected_var_libcompare(),
      per_name_pretty == input$per_libcompare,
      state == unique(df_target$state_name)
    ) %>%
    group_by(FISCAL_YEAR) %>%
    summarise(
      level = paste0(state, " Libraries"),
      per_median = round(median(per_calc, na.rm = T), 2),
      per_text_prefix = "Median "
    ) %>%
    distinct()

  df_target %<>%
    rename("per_median" = "per_calc")

  highchart() %>%
    hc_add_series(
      df_target,
      type = "column",
      color = "#FFB81D",
      hcaes(x = FISCAL_YEAR, y = per_median, group = level)
    ) %>%
    # hc_add_series(
    #   df_peers_state,
    #   type = "column",
    #   color = "#81D0F0",
    #   hcaes(x = FISCAL_YEAR, y = per_median, group = level)
    # ) %>%
    # hc_add_series(
    #   df_peers_national,
    #   type = "column",
    #   color = "#2eb1e5",
    #   hcaes(x = FISCAL_YEAR, y = per_median, group = level)
    #     ) %>%
    hc_add_series(
      df_state,
      type = "column",
      color = "#0086BF",
      hcaes(x = FISCAL_YEAR, y = per_median, group = level)
    ) %>%
    hc_add_series(
      df_nat,
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
    hc_caption(text = "Tip: click on the legend to show/hide specific groups") %>%
      hc_exporting(
        enabled = TRUE,
        filename = paste0(col_name_pretty, "_", per_text, "_state_and_national_chart")
      )
})

#### State DT ####

output$libcompare_state_table_header <- renderUI({
  paste0(
    library_state_libcompare(), " - ",
    input$var_libcompare, " ",
    unique(libcompare_reactive()$per_text), " - ",
    input$libcompare_state_dt_year
  )
})

output$libcompare_dt_state <- renderReactable({
  req(input$var_libcompare)
  var_name <- input$var_libcompare
  per_text <- unique(libcompare_reactive()$per_text)

  df <- libcompare_reactive() %>%
    filter(FISCAL_YEAR == input$libcompare_state_dt_year,
      state == library_state_libcompare()) %>%
    select(
      year = FISCAL_YEAR,
      library = CURRENT_LIBNAME,
      FSCS = FSCSKEY,
      population_service_area = POPU_LSA,
      FTE,
      variable = var,# hide col in reactable, include for download
      comparison_method = per_name_pretty, # hide col in reactable, include for download
      actual_variable_value = value,
      comparison_value = per_calc,
      rank_state,
      n_state,
      rank_national,
      n_national
    ) %>%
      mutate(variable = var_name)

  # Render reactable
  df %>%
    reactable::reactable(
      resizable = TRUE,
      pagination = FALSE,
      sortable = FALSE,
      defaultSorted = list(rank_state = "asc"),
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
        library = colDef(
          name = "Library",
          filterable = TRUE,
          minWidth = 150,
          style = list(backgroundColor = "#f7f7f7")
        ),
        FSCS = colDef(),
        population_service_area = colDef(
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
        variable = colDef(show = FALSE),
        comparison_method = colDef(show = FALSE),
        actual_variable_value = colDef(
          name = var_name,
          cell = function(value) {
            if (isTRUE(selected_var_libcompare() %in% currency_cols)) {
              dollar(value)
            } else if (is.na(value)) {
              "No Data"
            } else {
              format(value, big.mark = ",")
            }
          }
        ),
        comparison_value = colDef(
          name = paste0(var_name, " ", per_text),
          cell = function(value) {
            if (isTRUE(selected_var_libcompare() %in% currency_cols)) {
              paste0("$", format(value, big.mark = ","))
            } else if (is.na(value)) {
              "No Data"
            } else {
              format(value, big.mark = ",")
            }
          }
        ),
        rank_state = colDef(
          name = "State Rank",
          sortNALast = TRUE,
          cell = function(value, index) {
            if (!is.na(value)) {
              paste0(value, "/", df$n_state[index])
            } else {
              ""
            }
          }
        ),
        n_state = colDef(show = FALSE),
        rank_national = colDef(
          name = "National Rank",
          sortNALast = TRUE,
          cell = function(value, index) {
            if (df$year[index] > imls_year){
              "Not Yet Available"
            } else if (!is.na(value)) {
              paste0(value, "/", df$n_national[index])
            } else {
              ""
            }
          }
        ),
        n_national = colDef(show = FALSE)
      )
    )
})

#### National DT ####

output$libcompare_national_table_header <- renderUI({
  paste0(
    "Nationwide - ",
    input$var_libcompare, " ",
    unique(libcompare_reactive()$per_text), " - ",
    input$libcompare_national_dt_year
  )
})

output$libcompare_dt_national <- renderReactable({
  req(input$var_libcompare)
  var_name <- input$var_libcompare
  per_text <- unique(libcompare_reactive()$per_text)

  df <- libcompare_reactive() %>%
    filter(FISCAL_YEAR == input$libcompare_national_dt_year) %>%
    select(
      year = FISCAL_YEAR,
      library = CURRENT_LIBNAME,
      FSCS = FSCSKEY,
      state = state,
      population_service_area = POPU_LSA,
      FTE,
      variable = var, # hide col in reactable, include for download
      comparison_method = per_name_pretty, # hide col in reactable, include for download
      actual_variable_value = value,
      comparison_value = per_calc,
      rank_national,
      n_national
    ) %>%
      mutate(variable = var_name)

  # Render reactable

  df %>%
    reactable::reactable(
      resizable = TRUE,
      pagination = FALSE,
      sortable = FALSE,
      defaultSorted = list(rank_national = "asc"),
      highlight = TRUE,
      virtual = TRUE,
      height = 700,
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
        library = colDef(
          name = "Library",
          filterable = TRUE,
          minWidth = 150,
          style = list(backgroundColor = "#f7f7f7")
        ),
        FSCS = colDef(show = FALSE),
        state = colDef(name = "State", filterable = TRUE),
        population_service_area = colDef(
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
        variable = colDef(show = FALSE),
        comparison_method = colDef(show = FALSE),
        actual_variable_value = colDef(
          name = var_name,
          cell = function(value) {
            if (isTRUE(selected_var_libcompare() %in% currency_cols)) {
              dollar(value)
            } else if (is.na(value)) {
              "No Data"
            } else {
              format(value, big.mark = ",")
            }
          }
        ),
        comparison_value = colDef(
          name = paste0(var_name, " ", per_text),
          cell = function(value) {
            if (isTRUE(selected_var_libcompare() %in% currency_cols)) {
              paste0("$", format(value, big.mark = ","))
            } else if (is.na(value)) {
              "No Data"
            } else {
              format(value, big.mark = ",")
            }
          }
        ),
        rank_national = colDef(
          name = "National Rank",
          sortNALast = TRUE,
          cell = function(value, index) {
            if (df$year[index] > imls_year){
              "Not Yet Available"
            } else if (!is.na(value)) {
              paste0(value, "/", df$n_national[index])
            } else {
              ""
            }
          }
        ),
        n_national = colDef(show = FALSE)
      )
  )
})




#### Peer Bar Chart ####

output$libcompare_bar_peers_header <- renderUI({
  tooltip(
    span(
      paste0(
        library_name_pretty_libcompare(),
        " Compared to State and National Peer Library Medians"
      ),
      bs_icon("info-circle")
    ),
    p(
      HTML(
        paste0(
          "<b>Peer Libraries</b> are the 10 most similar libraries to the selected library. The tables below lists each of these state and national peers. The peer median is the median value across all peer libraries."
        )
      )
    ),
    options = list(customClass = "wide-tooltip")
  )
})

output$libcompare_hc_peers_bar <- renderHighchart({
  if (selected_var_libcompare() %in% currency_cols) {
    y_tt <- "${point.y:,.2f}"
    var_tt <- "${point.value:,.0f}"
  } else {
    y_tt <- "{point.y:,.2f}"
    var_tt <- "{point.value:,f}"
  }

  if (input$per_libcompare == "Per Capita") {
    per_total_text <- "Population of Legal Service Area"
  } else {
    per_total_text <- "FTE"
  }

  col_name_pretty <- input$var_libcompare
  lib <- input$library_libcompare

  df <- libcompare_reactive() %>%
    mutate(state_name = state) ### do not remove, "state" is also an hc call and things get weird

  per_text <- unique(df$per_text)

  peer_year <- max(libcompare_peers_state()$FISCAL_YEAR)
  
  df_peers_state <- libcompare_peers_state() %>%
    mutate(
      per_name_pretty = case_when(
        per_name == "POP_col" ~ "Per Capita",
        per_name == "FTE_col" ~ "Per FTE"
      )
    ) %>%
    filter(
      CURRENT_LIBNAME_DISAMB != lib,
      FISCAL_YEAR <= peer_year,
      var == selected_var_libcompare(),
      per_name_pretty == input$per_libcompare
    ) %>%
    group_by(FISCAL_YEAR) %>%
    summarise(
      level = "State Peers",
      per_median = round(median(per_calc, na.rm = T), 2),
      per_text_prefix = "Median "
    )
  
    df_peers_national <- libcompare_peers_national() %>%
      mutate(
        per_name_pretty = case_when(
          per_name == "POP_col" ~ "Per Capita",
          per_name == "FTE_col" ~ "Per FTE"
        )
      ) %>%
    filter(
      CURRENT_LIBNAME_DISAMB != lib,
      FISCAL_YEAR <= imls_year,
      var == selected_var_libcompare(),
      per_name_pretty == input$per_libcompare
    ) %>%
    group_by(FISCAL_YEAR) %>%
    summarise(
      level = "National Peers",
      per_median = round(median(per_calc, na.rm = T), 2),
      per_text_prefix = "Median "
    )

  df_target <- df %>%
    filter(
      CURRENT_LIBNAME_DISAMB == lib
    ) %>%
    mutate(
      level = CURRENT_LIBNAME,
      per_text_prefix = ""
    ) %>%
    rename("per_median" = "per_calc")

  highchart() %>%
    hc_add_series(
      df_target,
      type = "column",
      color = "#FFB81D",
      hcaes(x = FISCAL_YEAR, y = per_median, group = level)
    ) %>%
    hc_add_series(
      df_peers_state,
      type = "column",
      color = "#81D0F0",
      hcaes(x = FISCAL_YEAR, y = per_median, group = level)
    ) %>%
    hc_add_series(
      df_peers_national,
      type = "column",
      color = "#0987b8",
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
    hc_caption(text = "Tip: click on the legend to show/hide specific groups") %>%
      hc_exporting(
        enabled = TRUE,
        filename = paste0(col_name_pretty, "_", per_text, "_peer_chart")
      )
})

#### State Peer DT ####

#### Table header ####

output$libcompare_statepeers_table_header <- renderUI({

  tooltip(
    span(
      paste0(library_state_libcompare()," Peers for ", library_name_pretty_libcompare()),
      bs_icon("info-circle") #, title = "About Peer Libraries"
    ),
    p(
      HTML(
        paste0(
          "Peer libraries are those that are most similar to the selected library.<br><br>",
          "Using data from the most recent year available, five data points were used to calculate the similarity between libraries statewide. See the methodology page for more information on how peer libraries were calculated.<br><br>
          
          For each library, the 10 most similar libraries were identified on the basis of the following data points.<br><br>",
          "- Population of Legal Service Area<br>
      - Total FTE of Staff<br>
      - Total Revenue<br>
      - Number of Cardholders<br>
      - Number of Visits"
        )
      )
    ),
    #placement = "right"
    options = list(customClass = "wide-tooltip")
  )
})

output$libcompare_dt_statepeers <- renderReactable({
  col_name_pretty <- input$var_libcompare
  lib <- input$library_libcompare
  lib_pretty <- library_name_pretty_libcompare()

  peer_year <- input$libcompare_statepeers_dt_year

  df <- pls_national %>%
    filter(
      CURRENT_LIBNAME_DISAMB %in% libcompare_peers_state()$CURRENT_LIBNAME_DISAMB,
      FISCAL_YEAR == peer_year
    ) %>%
    select(
      Library = CURRENT_LIBNAME,
      CURRENT_LIBNAME_DISAMB,
      FSCS = FSCSKEY,
      Year = FISCAL_YEAR,
      `Population of Legal Service Area` = POPU_LSA,
      Visits = VISITS,
      `Registered Users` = REGBOR,
      `Total FTE of Staff` = TOTSTAFF,
      `Total Revenue` = TOTINCM
    )

  df1 <- df %>% filter(CURRENT_LIBNAME_DISAMB == lib)
  df2 <- df %>%
    filter(CURRENT_LIBNAME_DISAMB != lib) %>%
    arrange(Library)

  df_r <- rbind(df1, df2)
  
  df_r %<>% select(-CURRENT_LIBNAME_DISAMB)

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
          if (value == lib_pretty) {
            fontweight = "bold"
          } else {
            fontweight = 300
          }
          list(fontWeight = fontweight, backgroundColor = "#f7f7f7")
        }),
        FSCS = colDef(),
        #State = colDef(),
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


#### National Peer DT ####


output$libcompare_nationalpeers_table_header <- renderUI({

  tooltip(
    span(
      paste0("National Peers for ", library_name_pretty_libcompare()),
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
      - Number of Visits"
        )
      )
    ),
    #placement = "right"
    options = list(customClass = "wide-tooltip")
  )
})

output$libcompare_dt_nationalpeers <- renderReactable({
  col_name_pretty <- input$var_libcompare
  lib <- input$library_libcompare
  lib_pretty <- library_name_pretty_libcompare()

  peer_year <- input$libcompare_nationalpeers_dt_year #imls_year

  df <- pls_national %>%
    filter(
      CURRENT_LIBNAME_DISAMB %in% libcompare_peers_national()$CURRENT_LIBNAME_DISAMB,
      FISCAL_YEAR == peer_year
    ) %>%
    select(
      Library = CURRENT_LIBNAME,
      CURRENT_LIBNAME_DISAMB,
      FSCS = FSCSKEY,
      State = state,
      Year = FISCAL_YEAR,
      `Population of Legal Service Area` = POPU_LSA,
      Visits = VISITS,
      `Registered Users` = REGBOR,
      `Total FTE of Staff` = TOTSTAFF,
      `Total Revenue` = TOTINCM
    )

  df1 <- df %>% filter(CURRENT_LIBNAME_DISAMB == lib)
  df2 <- df %>%
    filter(CURRENT_LIBNAME_DISAMB != lib) %>%
    arrange(Library)

  df_r <- rbind(df1, df2) 

  df_r %<>% select(-CURRENT_LIBNAME_DISAMB)

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
          if (value == lib_pretty) {
            fontweight = "bold"
          } else {
            fontweight = 300
          }
          list(fontWeight = fontweight, backgroundColor = "#f7f7f7")
        }),
        FSCS = colDef(),
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