# nav_panel(
#   title = tags$h5(class = "fw-bold", "Library Service Map"),
#   class = " bg-body-secondary align-self-center m-1 p-0 border rounded-3",
#   style = "width: 95vw; padding: 0; margin: 1;",

layout_sidebar(
  fill = TRUE,
  sidebar = sidebar(
    title = "Filters",
    width = "25%",

    # div(
    #   class = "mb-2",
    #   tags$h5(class = "mb-1 mt-0", "Select Libraries by County"),
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
      # )
    ),
    actionButton(
      "submitButton",
      "Submit",
      width = "100%"
    )
  ),
  #nav_panel(
  card(
    title = NULL,
    min_height = "85vh",
    max_height = "85vh",
    leafletOutput("state_map", height = '92vh') |>
      withSpinner() |>
      as_fill_carrier()
  )
  #)
)
# )
