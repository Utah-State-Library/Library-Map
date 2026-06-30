##### individual Library #####

nav_panel(
  title = tags$h5(class = "fw-bold", "Individual Library"),
  class = " bg-body-secondary align-self-center m-1 p-0 border rounded-3",
  style = "width: 95vw; padding: 0; margin: 1;",

  layout_sidebar(
    class = "bg-body-secondary container-fluid align-self-center",
    fill = TRUE,
    sidebar = sidebar(
      title = NULL,
      width = "20%",
      pickerInput(
        "library_indiv",
        label = "Select a Library",
        choices = aes_national,
        selected = NULL,
        multiple = FALSE,
        options = list(
          `live-search` = TRUE # This enables the search bar
        )
      )
    ),
    ## Main Body ##

    navset_card_underline(
      nav_panel(
        "Look at a Specific Variable",
        icon = bs_icon("graph-up"),
        card(
          card_header(
            #uiOutput("header_temp"),
          ),
          sidebar(
            pickerInput(
              "var_indiv",
              label = "Select a Variable",
              choices = national_vars_pretty$INDICATOR,
              selected = "Library Visits",
              multiple = FALSE
            ),
            pickerInput(
              "per_indiv",
              label = "Show variable as...",
              choices = c("Total", "Per Capita", "Per FTE"),
              selected = "Total",
              multiple = FALSE
            )
          ),
          highchartOutput("hc_line_indiv")
        )
      )
    )
  )
)
