##### National #####

nav_panel(
  title = tags$h5(class = "fw-bold", "State Comparison"),
  class = " bg-body-secondary align-self-center m-1 p-0 border rounded-3",
  style = "width: 95vw; padding: 0; margin: 1;",

  layout_sidebar(
    class = "bg-body-secondary container-fluid align-self-center",
    fill = TRUE,
    sidebar = sidebar(
      title = NULL,
      width = "25%",
      pickerInput(
        "national_var",
        label = "Select a Variable",
        choices = national_vars_pretty$INDICATOR,
        selected = "Library Visits",
        multiple = FALSE
      ),
      conditionalPanel(
        condition = "input.national_tabs === 'Graph'",

        pickerInput(
          "national_states",
          label = "Highlight a State",
          choices = states,
          selected = "Utah",
          multiple = FALSE
        ),
        sliderInput(
          "national_years",
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
        input_switch("national_switch", "Show All States?", value = TRUE)
      ),
      conditionalPanel(
        condition = "input.national_tabs === 'Map'",
        sliderInput(
          "national_map_year",
          label = "Select a Year",
          min = min(pls_national$FISCAL_YEAR),
          max = max(pls_national$FISCAL_YEAR),
          value = c(current_year - 1),
          round = TRUE,
          step = 0,
          sep = ""
        )
      ),
      conditionalPanel(
        condition = "input.national_tabs === 'Table'",
        sliderInput(
          "national_dt_year",
          label = "Select a Year",
          min = min(pls_national$FISCAL_YEAR),
          max = max(pls_national$FISCAL_YEAR),
          value = c(current_year - 1),
          round = TRUE,
          step = 0,
          sep = ""
        )
      )
    ),
    ## Main Body ##

    navset_card_underline(
      id = "national_tabs",
      height = '84vh',
      nav_panel(
        "Graph",
        icon = bs_icon("graph-up"),
        highchartOutput("national_hc"), #height = "600px", height = "60vh"
        height = 600
      ),
      nav_panel(
        "Map",
        icon = bs_icon("globe-americas"),
        layout_columns(
          col_widths = c(12, 12),
          uiOutput("national_map_title"),
          layout_columns(
            col_widths = c(8, 4),
            leafletOutput("national_map") |> #height = 200, height = '60vh'
              withSpinner() |>
              as_fill_carrier(),
            reactableOutput("national_map_dt")
          )
        )
      ),
      nav_panel(
        "Table",
        icon = bs_icon("table"),
        fillable = TRUE,
        uiOutput("national_dt_title"),
        reactableOutput("national_dt")
      )
    ),
    card(
      card_header(
        "About Per Capita Comparisons",
        class = "my-header-dkblue text-white"
      ),
      p(
        HTML(
          paste0(
            "Per capita comparisons are a way to make meaningful comparisons between libraries that serve populations of different sizes. Instead of looking at raw totals — which can be misleading — you divide a library’s data by the size of the population it serves. This shows how much service or usage occurs per person, making it easier to compare libraries on equal footing.<br>",
            "For certain comparisons, it’s useful to express data per 100, 1,000, or more people to make differences easier to interpret. In some cases — especially with smaller libraries — the standardized rate may exceed the total population served, resulting in a comparison value that is higher than the actual number."
          )
        )
      )
    )
  )
)
