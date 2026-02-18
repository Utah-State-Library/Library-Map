## Create the library locations df
map_all <- outlets %>%
  mutate(
    LAT = as.numeric(LAT),
    LONG = as.numeric(LONG),
    library_info = case_when(
      CURRENT_LIBNAME_OUTLET != CURRENT_LIBNAME_AE ~
        paste0(
          "<table>
                        <div style='font-size: 18px;'><b>",
          CURRENT_LIBNAME_OUTLET,
          "</div>
                        <div style='font-size: 12px;'>",
          CURRENT_LIBNAME_AE,
          "</div>
                        <div style='font-size: 12px;'>",
          str_to_title(ADDRESS),
          ", ",
          str_to_title(CITY),
          ", ",
          ZIP,
          "</div>
        </table>"
        ),
      CURRENT_LIBNAME_OUTLET == CURRENT_LIBNAME_AE ~
        paste0(
          "<table>
                        <div style='font-size: 18px;'><b>",
          CURRENT_LIBNAME_OUTLET,
          "</div>
                        <div style='font-size: 12px;'>",
          str_to_title(ADDRESS),
          ", ",
          str_to_title(CITY),
          ", ",
          ZIP,
          "</div>
        </table>"
        )
    )
  )

##### Sync Inputa #####
observe({
  aes <- outlets %>%
    filter(
      CNTY %in% input$st_county,
      SERVICE_AREA %in% input$system_type,
      C_OUT_TY %in% input$outlet_type
    ) %>%
    reframe(unique(CURRENT_LIBNAME_AE)) %>%
    pull() %>%
    sort()

  updatePickerInput(
    session,
    "ae",
    label = "Library System",
    choices = aes,
    selected = aes,
    options = list(
      `actions-box` = TRUE,
      `selected-text-format` = paste0(
        "count > ",
        length(aes) - 1
      ),
      `count-selected-text` = "All Library Systems"
    )
  )
})

# observe({
#   system_type <- outlets %>%
#     filter(
#       CNTY %in% input$st_county,
#       CURRENT_LIBNAME_AE %in% input$ae,
#       C_OUT_TY %in% input$outlet_type
#     ) %>%
#     reframe(unique(SERVICE_AREA)) %>%
#     pull() %>%
#     sort()

#   updatePickerInput(
#     session,
#     "system_type",
#     choices = c("City Library" = "city", "County Library" = "county"),
#     selected = system_type,
#     options = list(
#       `actions-box` = TRUE,
#       `selected-text-format` = paste0(
#         "count > ",
#         1
#       ),
#       `count-selected-text` = "All Library Types"
#     )
#   )
# })

# observe({
#   outlet_type <- outlets %>%
#     filter(
#       CNTY %in% input$st_county,
#       CURRENT_LIBNAME_AE %in% input$ae,
#       SERVICE_AREA %in% input$system_type
#     ) %>%
#     reframe(unique(C_OUT_TY)) %>%
#     pull() %>%
#     sort()

#   updatePickerInput(
#     session,
#     "outlet_type",
#     choices = c("Central Library" = "CE", "Branch Library" = "BR"),
#     selected = outlet_type,
#     options = list(
#       `actions-box` = TRUE,
#       `selected-text-format` = paste0(
#         "count > ",
#         1
#       ),
#       `count-selected-text` = "All Location Types"
#     )
#   )
# })

##### Filter Data #####
map_libs_filtered <- eventReactive(
  input$submitButton,
  {
    map_all %>%
      filter(
        CNTY %in% input$st_county,
        CURRENT_LIBNAME_AE %in% input$ae,
        C_OUT_TY %in% input$outlet_type,
        SERVICE_AREA %in% input$system_type
      )
  },
  ignoreNULL = FALSE
)

#### Render State Map ####
output$state_map <- renderLeaflet({
  input$submitButton

  map_df <- isolate(map_libs_filtered())

  shiny::validate(
    need((nrow(map_df) != 0), "No data available based on your selection.")
  )

  map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    addTiles() %>%
    addProviderTiles(
      "CartoDB.Positron",
      group = "CartoDB.Positron"
    ) %>%
    onRender(
      "function(el, x) {
          L.control.zoom({position:'bottomright'}).addTo(this);
        }"
    )

  ## Show Library Locations
  map <- map %>%
    addCircleMarkers(
      data = map_df,
      lng = ~LONG,
      lat = ~LAT,
      radius = 4,
      fillOpacity = .75,
      color = "#002f6c",
      label = ~ lapply(library_info, HTML),
      popupOptions = popupOptions(keepInView = TRUE),
    )

  map
})


output$n_aes <- renderUI({
  map_libs_filtered() %>%
    reframe(n = n_distinct(CURRENT_LIBNAME_AE)) %>%
    pull(n)
})

output$n_locations <- renderUI({
  map_libs_filtered() %>%
    reframe(n = n_distinct(CURRENT_LIBNAME_OUTLET)) %>%
    pull(n)
})

output$n_citylibs <- renderUI({
  map_libs_filtered() %>%
    filter(SERVICE_AREA == "city") %>%
    reframe(n = n_distinct(CURRENT_LIBNAME_AE)) %>%
    pull(n)
})

output$n_countylibs <- renderUI({
  map_libs_filtered() %>%
    filter(SERVICE_AREA == "county") %>%
    reframe(n = n_distinct(CURRENT_LIBNAME_AE)) %>%
    pull(n)
})
