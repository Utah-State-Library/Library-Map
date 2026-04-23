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
        label = "Select a Library System",
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
        label = "Compare...",
        choices = c("Per Capita", "Per FTE"),
        selected = "Per Capita",
        multiple = FALSE
        #)
      ),
      # conditionalPanel(
      #   condition = "input.singlib_tabs === 'Scatter'",
      #   pickerInput(
      #     "peer_varY",
      #     label = "Variable 1",
      #     choices = national_vars_pretty$INDICATOR,
      #     selected = "Library Visits",
      #     multiple = FALSE
      #   ),
      #   pickerInput(
      #     "peer_varX",
      #     label = "Variable 2",
      #     choices = national_vars_pretty$INDICATOR,
      #     selected = "Total Operating Revenue",
      #     multiple = FALSE
      #   )
      # )
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
    ) #,

    #   height = 600
    # ) #,
    # nav_panel(
    #   "Scatter",
    #   icon = bs_icon("graph-up"),
    #   highchartOutput("peer_scatter_hc"), #height = "600px", height = "60vh"
    #   height = 600
    # )
    #)
  )
)
