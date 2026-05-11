##### Compare: Library Level #####

nav_panel(
  title = tags$h5(class = "fw-bold", "Compare Libraries"),
  class = " bg-body-secondary align-self-center m-1 p-0 border rounded-3",
  style = "width: 95vw; padding: 0; margin: 1;",

  layout_sidebar(
    class = "bg-body-secondary container-fluid align-self-center",
    fill = TRUE,
    sidebar = sidebar(
      title = NULL,
      width = "20%",
      pickerInput(
        "library_libcompare",
        label = "Select a Library",
        choices = aes_national,
        selected = NULL,
        multiple = FALSE,
        options = list(
          `live-search` = TRUE # This enables the search bar
        )
      ),
      pickerInput(
        "var_libcompare",
        label = "Select a Variable",
        choices = national_vars_pretty$INDICATOR,
        selected = "Library Visits",
        multiple = FALSE
      ),
      pickerInput(
        "per_libcompare",
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
      id = "libcompare_tabs",
      nav_panel(
        "Compare to Statewide and Nationwide Libraries",
        icon = bs_icon("graph-up"),
        card(
          card_header(
            uiOutput("libcompare_bar_header"),
            popover(
              span("Settings", bs_icon("gear")),
              sliderInput(
                "libcompare_years_bar",
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
          highchartOutput("libcompare_hc_bar")
        ),
        card(
          card_header(
            uiOutput("libcompare_state_table_header"),
            popover(
              span("Settings & Download", bs_icon("gear")),
              sliderInput(
                "libcompare_state_dt_year",
                label = "Select a Year",
                min = min(pls_national$FISCAL_YEAR),
                max = max(pls_national$FISCAL_YEAR),
                value = c(current_year),
                round = TRUE,
                step = 0,
                sep = ""
              ),
              csvDownloadButton("libcompare_dt_state", filename = "within_state_library_rankings.csv"),
              title = "Table Controls"
            ),
            class = "my-header-grey d-flex justify-content-between",
          ),
          reactableOutput("libcompare_dt_state")
        )
      ),
      nav_panel(
        "Compare to Peer Libraries",
        icon = bs_icon("bank2"),
        card(
          card_header(
            uiOutput("libcompare_bar_peers_header"),
            popover(
              span("Settings", bs_icon("gear")),
              sliderInput(
                "libcompare_peer_years_bar",
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
          highchartOutput("libcompare_hc_peers_bar")
        ),
        card(
          card_header(
            uiOutput("libcompare_statepeers_table_header"),
            popover(
              span("Settings & Download", bs_icon("gear")),
              sliderInput(
                "libcompare_statepeers_dt_year",
                label = "Select a Year",
                min = min(pls_national$FISCAL_YEAR),
                max = max(pls_national$FISCAL_YEAR),
                value = c(current_year),
                round = TRUE,
                step = 0,
                sep = ""
              ),
              csvDownloadButton("libcompare_dt_statepeers", filename = "state_peers.csv"),
              title = "Table Controls"
            ),
            class = "my-header-grey d-flex justify-content-between",
          ),
          reactableOutput("libcompare_dt_statepeers")
        ),
        card(
          card_header(
            uiOutput("libcompare_nationalpeers_table_header"),
            popover(
              span("Settings & Download", bs_icon("gear")),
              sliderInput(
                "libcompare_nationalpeers_dt_year",
                label = "Select a Year",
                min = min(pls_national$FISCAL_YEAR),
                max = max(pls_national$FISCAL_YEAR),
                value = c(imls_year),
                round = TRUE,
                step = 0,
                sep = ""
              ),
              csvDownloadButton("libcompare_dt_nationalpeers", filename = "national_peers.csv"),
              title = "Table Controls"
            ),
            class = "my-header-grey d-flex justify-content-between",
          ),
          reactableOutput("libcompare_dt_nationalpeers")
        )
      ),
      nav_panel(
        "National Rankings",
        icon = bs_icon("table"),
        fillable = TRUE,
        card(
          card_header(
            uiOutput("libcompare_national_table_header"),
            popover(
              span("Settings & Download", bs_icon("gear")),
              sliderInput(
                "libcompare_national_dt_year",
                label = "Select a Year",
                min = min(pls_national$FISCAL_YEAR),
                max = max(pls_national$FISCAL_YEAR),
                value = c(current_year),
                round = TRUE,
                step = 0,
                sep = ""
              ),
              csvDownloadButton("libcompare_dt_national", filename = "national_rankings.csv"),
              title = "Table Controls"
            ),
            class = "my-header-grey d-flex justify-content-between",
          ),
          reactableOutput("libcompare_dt_national") |>
            withSpinner() |>
            as_fill_carrier()
        )
      )
    )
  )
)
