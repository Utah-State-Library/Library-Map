

selected_var_lib <- reactive({
  variable_key %>% filter(INDICATOR == input$lib_var) %>% pull(SHORTNAME)
})

selected_year_lib <- reactive({
  input$lib_year
})

pls_nationalpeers_lib <- reactive({
  lib <- input$lib_library

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