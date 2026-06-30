library_indiv_reactive <- reactive({
  pls_national_peers %>%
    filter(CURRENT_LIBNAME_DISAMB == input$library_indiv)
})

selected_var_indiv <- reactive({
  variable_key %>% filter(INDICATOR == input$var_indiv) %>% pull(SHORTNAME)
})

output$hc_line_indiv <- renderHighchart({
  if (selected_var_indiv() %in% currency_cols) {
    y_tt <- "${point.y:,.2f}"
    var_tt <- "${point.value:,.0f}"
  } else {
    y_tt <- "{point.y:,.2f}"
    var_tt <- "{point.value:,f}"
  }

  if (input$per_indiv == "Per Capita") {
    per_total_text <- "Population of Legal Service Area"
  } else if (input$per_indiv == "Per FTE") {
    per_total_text <- "FTE"
  } else {
    per_total_text <- "Total"
  }

  if (input$per_indiv == "Total") {
    df <- library_indiv_reactive() %>%
      filter(var == selected_var_indiv()) %>%
      mutate(plot_var = value) %>%
      select(FISCAL_YEAR, CURRENT_LIBNAME, plot_var) %>%
      distinct()
  } else if (input$per_indiv == "Per Capita" | input$per_indiv == "Per FTE") {
    df <- library_indiv_reactive() %>%
      mutate(
        per_name_pretty = case_when(
          per_name == "POP_col" ~ "Per Capita",
          per_name == "FTE_col" ~ "Per FTE"
        )
      ) %>%
      filter(
        var == selected_var_indiv(),
        per_name_pretty == input$per_indiv
      ) %>%
      mutate(plot_var = per_calc)
  }

  highchart() %>%
    hc_add_series(
      df,
      type = "line",
      color = "#777777",
      hcaes(x = FISCAL_YEAR, y = plot_var)
    )
})
