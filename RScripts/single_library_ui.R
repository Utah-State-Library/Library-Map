##### Single Library UI #####

# Resources:

nav_panel(
  title = tags$h5(class = "fw-bold", "Peer Library Comparison"),
  class = " bg-body-secondary align-self-center m-1 p-0 border rounded-3",
  style = "width: 95vw; padding: 0; margin: 1;",

  layout_sidebar(
    class = "bg-body-secondary container-fluid align-self-center",
    fill = TRUE,
    sidebar = sidebar(
      title = NULL,
      width = "25%",
      pickerInput(
        "singlib_var",
        label = "Select a Variable",
        choices = national_vars_pretty$INDICATOR,
        selected = "Library Visits",
        multiple = FALSE
      ),
      pickerInput(
        "singlib_library",
        label = "Select a Library",
        choices = aes_national,
        selected = NULL,
        multiple = FALSE,
      ),
      conditionalPanel(
        condition = "input.singlib_tabs === 'Graph'",

        # sliderInput(
        #   "singlib_years",
        #   label = "Range of Years to Graph",
        #   min = min(pls_national_peers$FISCAL_YEAR),
        #   max = max(pls_national_peers$FISCAL_YEAR),
        #   value = c(
        #     max(pls_national_peers$FISCAL_YEAR) - 5,
        #     max(pls_national_peers$FISCAL_YEAR)
        #   ),
        #   round = TRUE,
        #   step = 0,
        #   sep = ""
        # ),
        #input_switch("utah_switch", "Show All Libraries?", value = TRUE)
      ),
      conditionalPanel(
        condition = "input.singlib_tabs === 'Table'",
        sliderInput(
          "singlib_dt_year",
          label = "Select a Year",
          min = min(pls_national_peers$FISCAL_YEAR),
          max = max(pls_national_peers$FISCAL_YEAR),
          value = c(current_year - 1),
          round = TRUE,
          step = 0,
          sep = ""
        )
      )
    ),
    ## Main Body ##
    navset_card_underline(
      id = "singlib_tabs",
      height = '84vh',
      nav_panel(
        "Graph",
        icon = bs_icon("graph-up"),
        highchartOutput("peers_hc"), #height = "600px", height = "60vh"
        height = 600
      ),
      nav_panel(
        "Table",
        icon = bs_icon("table"),
        fillable = TRUE,
        #uiOutput("utah_dt_title"),
        reactableOutput("peer_dt")
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
