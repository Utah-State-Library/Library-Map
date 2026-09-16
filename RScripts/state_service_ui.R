nav_panel(
  title = tags$h5(class = "fw-bold", "Utah Library Map"), # Page name
  class = " bg-body-secondary align-self-center m-1 p-0 border rounded-3",
  style = "width: 95vw; padding: 0; margin: 1;",
  height = "82vh", # Keep this unless you have a good reason to change it; this has looked good on different screen sizes

  layout_sidebar(
    class = "bg-body-secondary container-fluid align-self-center",
    fill = TRUE,
    sidebar = sidebar(
      title = h5(
        # sidebar title, but it's more like a header for the page and specifies that the data are from the current year
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

      ### Filters ###
      width = "20%",

      ## Filter counties
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

      ## Filter libraries
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

      ## Filter system types
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

      ## Filter outlet types
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

      ## Submit button
      actionButton(
        "submitButton",
        "Submit",
        width = "100%"
      ),

      ## Context text (for systems with no central library)
      uiOutput("ce_text")
    ),

    ### Main Body ###
    layout_columns(
      col_widths = c(8, 4), # horizontally, map takes 8 cols & valueboxes take 4

      ## Map card
      card(
        title = NULL,
        min_height = "85vh",
        max_height = "85vh",
        leafletOutput("state_map", height = '85vh') |>
          withSpinner() |>
          as_fill_carrier()
      ),

      ## Value box column
      layout_columns(
        col_widths = c(12, 12), # make 12,12 so that valueboxes stack vertically

        ## Community oriented valuebox
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

        ## Service stats oriented valuebox
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

        ## Financial oriented valuebox
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
