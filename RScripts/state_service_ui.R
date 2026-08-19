nav_panel(
  title = tags$h5(class = "fw-bold", "Utah Library Map"),
  class = " bg-body-secondary align-self-center m-1 p-0 border rounded-3",
  style = "width: 95vw; padding: 0; margin: 1;",
  height = "82vh",

  layout_sidebar(
    class = "bg-body-secondary container-fluid align-self-center",
    fill = TRUE,
    sidebar = sidebar(
      title = h5(
        HTML(
          paste0(
            "<br><b>",
            current_year,
            " Public Library Survey Data</b><br><br><hr>"
          )
        ),
        class = "header-text text-center",
        style = "color: #093692; padding: 0px; margin: 0px;"
      ),
      #"Filters",
      width = "20%",

      pickerInput(
        "st_county",
        label = NULL,
        choices = counties,
        selected = counties,
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          `selected-text-format` = paste0(
            "count > ",
            length(counties) - 1
          ),
          `count-selected-text` = "All Counties"
        )
      ),
      pickerInput(
        "ae",
        label = NULL,
        choices = aes,
        selected = aes,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = paste0(
            "count > ",
            length(aes) - 1
          ),
          `count-selected-text` = "All Library Systems"
        )
      ),
      pickerInput(
        "system_type",
        label = NULL,
        choices = c("City Library" = "city", "County Library" = "county"),
        selected = c("city", "county"),
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = paste0(
            "count > ",
            1
          ),
          `count-selected-text` = "All Library Types"
        )
      ),
      pickerInput(
        "outlet_type",
        label = NULL,
        choices = c("Central Library" = "CE", "Branch Library" = "BR"),
        selected = c("CE", "BR"),
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = paste0(
            "count > ",
            1
          ),
          `count-selected-text` = "All Location Types"
        )
      ),
      actionButton(
        "submitButton",
        "Submit",
        width = "100%"
      ),
      uiOutput("ce_text")
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(
        title = NULL,
        min_height = "85vh",
        max_height = "85vh",
        leafletOutput("state_map", height = '85vh') |>
          withSpinner() |>
          as_fill_carrier()
      ),
      layout_columns(
        col_widths = c(12, 12),
        # h5(
        #   HTML(
        #     paste0(
        #       "<b>",
        #       current_year,
        #       " Public Library Survey Data</b>"
        #     )
        #   ),
        #   class = "header-text text-center",
        #   style = "color: #093692; padding: 0px; margin: 0px;"
        # ),
        value_box(
          title = "Library Cardholders",
          value = uiOutput("n_pcnt_regbor"),
          hr(),
          p(uiOutput("n_regbor")),
          p(uiOutput("n_popu_lsa")),
          p(uiOutput("n_citylibs")),
          p(uiOutput("n_countylibs")),
          showcase = bsicons::bs_icon("file-person-fill"),
          theme = value_box_theme(bg = "#ffffff", fg = "#093692"),
          class = "p-0 nopad"
        ),
        value_box(
          title = "Visits",
          value = uiOutput("n_visits"),
          hr(),
          p(uiOutput("n_pro")),
          p(uiOutput("n_atten")),
          p(uiOutput("n_circ")),
          p(uiOutput("n_kidcirc")),
          showcase = bsicons::bs_icon("people-fill"), #bsicons::bs_icon("people-fill"),
          theme = value_box_theme(bg = "#ffffff", fg = "#093692"),
          class = "p-0 nopad"
        ),
        value_box(
          title = "Total Revenue",
          value = uiOutput("n_totincm"),
          hr(),
          p(uiOutput("n_locgvt")),
          p(uiOutput("n_stgvt")),
          p(uiOutput("n_fedgvt")),
          p(uiOutput("n_othincm")),
          showcase = bsicons::bs_icon("bank2"),
          theme = value_box_theme(bg = "#ffffff", fg = "#093692"),
          class = "p-0 nopad"
        )
      )
    )
  )
)
