##### National #####

nav_panel(
  title = tags$h5(class = "fw-bold", "Compare States"), # Page title
  class = " bg-body-secondary align-self-center m-1 p-0 border rounded-3",
  style = "width: 95vw; padding: 0; margin: 1;",

  layout_sidebar(
    class = "bg-body-secondary container-fluid align-self-center",
    fill = TRUE,
    sidebar = sidebar(
      title = NULL,
      width = "20%",

      # Select a target state
      pickerInput(
        "national_states",
        label = "Select a State",
        choices = states,
        selected = "Utah",
        multiple = FALSE
      ),

      # Select a variable
      pickerInput(
        "national_var",
        label = "Select a Variable",
        choices = national_vars_pretty$INDICATOR,
        selected = "Library Visits",
        multiple = FALSE
      ),

      # Select a per comparison
      pickerInput(
        "national_per",
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
      id = "national_tabs",
      #height = '84vh',
      nav_panel(
        # if you want additional tabs, add more nav_panels after this one
        "Compare State to National",
        icon = bs_icon("graph-up"),
        card(
          card_header(
            uiOutput("national_bar_header"),
            popover(
              span("Settings", bs_icon("gear")),
              sliderInput(
                "national_years_bar",
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
          # The actual bar graph
          highchartOutput("national_hc_bar")
        ),

        ## These are some old sections that could be revived; the first is a line graph that I eventually replaced with the current bar chart and the second is the national map that would be awesome to bring back (just try to find a better map projection if you can, the leaflet one makes alaska, hawaii, and the territories look awful)
        # card(
        #   card_header(
        #     uiOutput("national_line_header"),
        #     popover(
        #       bs_icon("gear"),
        #       sliderInput(
        #         "national_years",
        #         label = "Range of Years to Graph",
        #         min = min(pls_national$FISCAL_YEAR),
        #         max = max(pls_national$FISCAL_YEAR),
        #         value = c(
        #           max(pls_national$FISCAL_YEAR) - 5,
        #           max(pls_national$FISCAL_YEAR)
        #         ),
        #         round = TRUE,
        #         step = 0,
        #         sep = ""
        #       ),
        #       title = "Graph Controls"
        #     ),
        #     class = "my-header-grey d-flex justify-content-between",
        #   ),
        #   highchartOutput("national_hc")
        # ),
        # card(
        #   card_header(
        #     uiOutput("national_map_header"),
        #     popover(
        #       bs_icon("gear"),
        #       sliderInput(
        #         "national_map_year",
        #         label = "Select a Year",
        #         min = min(pls_national_state_map$YEAR),
        #         max = max(pls_national_state_map$YEAR),
        #         value = c(current_year - 1),
        #         round = TRUE,
        #         step = 0,
        #         sep = ""
        #       ),
        #       title = "Map Controls"
        #     ),
        #     class = "my-header-grey d-flex justify-content-between",
        #   ),
        #   layout_columns(
        #     col_widths = c(12, 12),

        #     layout_columns(
        #       col_widths = c(8, 4),
        #       #htmlOutput("frame"),
        #       leafletOutput("national_map") |> #height = 200, height = '60vh'
        #         withSpinner() |>
        #         as_fill_carrier(),
        #       reactableOutput("national_map_dt")
        #     )
        #   )
        # ),
        ## This is the table that shows state numbers and rankings
        card(
          card_header(
            uiOutput("national_table_header"),
            popover(
              span("Settings & Download", bs_icon("gear")),
              sliderInput(
                "national_dt_year",
                label = "Select a Year",
                min = min(pls_national$FISCAL_YEAR),
                max = max(pls_national$FISCAL_YEAR),
                value = c(current_year - 1),
                round = TRUE,
                step = 0,
                sep = ""
              ),
              uiOutput("nationalcomp_dt_csv_button"),
              title = "Table Controls"
            ),
            class = "my-header-grey d-flex justify-content-between",
          ),
          reactableOutput("national_dt")
        )
      )
    )
  )
)
