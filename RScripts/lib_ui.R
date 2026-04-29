##### Utah #####

nav_panel(
  title = tags$h5(class = "fw-bold", "Library Page"),
  class = " bg-body-secondary align-self-center m-1 p-0 border rounded-3",
  style = "width: 95vw; padding: 0; margin: 1;",

  layout_sidebar(
    class = "bg-body-secondary container-fluid align-self-center",
    fill = TRUE,
    sidebar = sidebar(
      title = NULL,
      width = "20%",
      pickerInput(
        "lib_library",
        label = "Select a Library",
        choices = aes,
        selected = NULL,
        multiple = FALSE,
      ),
      pickerInput(
        "lib_var",
        label = "Select a Variable",
        choices = national_vars_pretty$INDICATOR,
        selected = "Library Visits",
        multiple = FALSE
      ),
      pickerInput(
        "lib_per",
        label = tooltip(
          trigger = list("Compare...", bs_icon("info-circle")),
          p(HTML(paste0(
            "<b>Per Capita</b> shows how much service or usage occurs per person served by a given library, making it easier to compare libraries on equal footing. <br><br>",
            "<b>Per FTE</b> shows how much service or usage occurs per Full Time Equivalent (FTE). One FTE is equal to a full work week. FTE is not necessarily equal to the number of staff working at a library because some staff may be part-time."
          ))),
          options = list(customClass = "wide-tooltip")
        ),
        choices = c("Per Capita", "Per FTE"),
        selected = "Per Capita",
        multiple = FALSE
      )
    ),
    ## Main Body ##

    navset_card_underline(
      id = "lib_tabs",
      #height = '84vh',
      nav_panel(
        "Visualize",
        icon = bs_icon("graph-up"),
        card(
          card_header(
            uiOutput("lib_bar_header"),
            popover(
              bs_icon("gear"),
              sliderInput(
                "lib_years_bar",
                label = "Range of Years to Graph",
                min = min(pls_national$FISCAL_YEAR),
                max = max(pls_national$FISCAL_YEAR),
                value = c(
                  max(pls_national$FISCAL_YEAR) - 5,
                  max(pls_national$FISCAL_YEAR)
                ),
                round = TRUE,
                step = 0,
                sep = ""
              ),
              title = "Graph Controls"
            ),
            class = "my-header-grey d-flex justify-content-between",
          ),
          highchartOutput("lib_hc_bar")
        ),
        card(
          card_header(
            uiOutput("lib_line_header"),
            popover(
              bs_icon("gear"),
              sliderInput(
                "lib_years",
                label = "Range of Years to Graph",
                min = min(pls_national$FISCAL_YEAR),
                max = max(pls_national$FISCAL_YEAR),
                value = c(
                  max(pls_national$FISCAL_YEAR) - 5,
                  max(pls_national$FISCAL_YEAR)
                ),
                round = TRUE,
                step = 0,
                sep = ""
              ),
              title = "Graph Controls"
            ),
            class = "my-header-grey d-flex justify-content-between",
          ),
          highchartOutput("lib_hc")
        )
      ),
      nav_panel(
        "Table",
        icon = bs_icon("table"),
        fillable = TRUE,
        card(
          card_header(
            uiOutput("lib_table_header"),
            popover(
              bs_icon("gear"),
              sliderInput(
                "lib_dt_year",
                label = "Select a Year",
                min = min(pls_national$FISCAL_YEAR),
                max = max(pls_national$FISCAL_YEAR),
                value = c(current_year),
                round = TRUE,
                step = 0,
                sep = ""
              ),
              title = "Table Controls"
            ),
            class = "my-header-grey d-flex justify-content-between",
          ),
          reactableOutput("lib_dt")
        )
      )
    )
  )
)
