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
      width = "20%",
      pickerInput(
        "singlib_library",
        label = "Select a Library",
        choices = aes_national,
        selected = NULL,
        multiple = FALSE,
        options = list(
          `live-search` = TRUE # This enables the search bar
        )
      ),
      # conditionalPanel(
      #   condition = "input.singlib_tabs === 'Peer Comparisons'",
      pickerInput(
        "singlib_var",
        label = "Select a Variable",
        choices = national_vars_pretty$INDICATOR,
        selected = "Library Visits",
        multiple = FALSE
      ),
      pickerInput(
        "singlib_per",
        label = tooltip(
          trigger = list("Compare...", bs_icon("info-circle")),
          p(HTML(paste0(
            "<b>Per Capita</b> shows how much service or usage occurs per person served by a given library, making it easier to compare libraries on equal footing. <br><br>",
            "<b>Per FTE</b> shows how much service or usage occurs per Full Time Equivalent (FTE). One FTE is equal to a 4full work week. FTE is not necessarily equal to the number of staff working at a library because some staff may be part-time."
          ))),
          options = list(customClass = "wide-tooltip")
        ),
        choices = c("Per Capita", "Per FTE"),
        selected = "Per Capita",
        multiple = FALSE
        #)
      ),
      pickerInput(
        "peerlevel_per",
        label = tooltip(
          trigger = list("Peer Level", bs_icon("info-circle")),
          p(HTML(paste0(
            "<b>Nationwide Peers</b> are the 10 libraries across all states and territories that are most similar to the selected library. <br><br>",
            "<b>Statewide Peers</b> are the 10 libraries across the state that are most similar to the selected library."
          ))),
          options = list(customClass = "wide-tooltip")
        ),
        choices = c("Nationwide Peers", "Statewide Peers"),
        selected = "Nationwide Peers",
        multiple = FALSE
      )
    ),
    ## Main Body ##
    # navset_card_underline(
    #   id = "singlib_tabs",
    #   nav_panel(
    #     "Peer Comparisons",
    #     icon = bs_icon("graph-up"),
    card(
      card_header(
        uiOutput("peers_bar_header"),
        class = "my-header-grey",
      ),
      highchartOutput("peer_hc_bar") #,
      #highchartOutput("peers_hc")
      # )
    ),
    card(
      card_header(
        uiOutput("peers_dt_header"),
        class = "my-header-grey"
      ),
      reactableOutput("peer_dt")
    )
  )
)
