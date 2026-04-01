## Create the library locations df

map_all <- outlets %>%
  left_join(pls, by = c("CURRENT_LIBNAME_AE" = "CURRENT_LIBNAME")) %>%
  group_by(CURRENT_LIBNAME_AE) %>%
  mutate(
    n_locs = sum(C_OUT_TY == "CE") + sum(C_OUT_TY == "BR"),
    OUTLET_NAME = gsub(
      paste0(CURRENT_LIBNAME_AE, " "),
      "",
      CURRENT_LIBNAME_OUTLET
    ),
    OUTLET_NAME = gsub(
      "Salt Lake City Public Library |Washington County Library |Weber County Library",
      "",
      OUTLET_NAME
    ),
    OUTLET_NAME = trimws(OUTLET_NAME)
  ) %>%
  ungroup()

map_all %<>%
  mutate(
    LAT = as.numeric(LAT),
    LONG = as.numeric(LONG),
    library_data_header = case_when(
      n_locs == 1 ~ paste0(
        "
      <table style='width: 100%'>
        <div style='font-size: 14px;'><b>",
        CURRENT_LIBNAME_AE,
        "</b><br>",
        FISCAL_YEAR,
        " Public Library Survey",
        "<br></div><br>"
      ),
      n_locs > 1 ~ paste0(
        "
      <table style='width: 100%'>
        <div style='font-size: 14px;'><b>",
        CURRENT_LIBNAME_AE,
        "</b><br>",
        FISCAL_YEAR,
        " Public Library Survey",
        "<br>",
        "</div> <div style='font-size: 12px;'><em>",
        "This table shows data for the entire library system, and is not branch specific.</em>",
        "<br></div>"
      )
    ),
    library_data_table = paste0(
      "<tr>
          <td style = \"text-align:left; background-color: #f2f2f2;\">",
      "Number of Library Locations: ",
      "</td>
          <td style = \"text-align: right; background-color: #f2f2f2;\">",
      n_locs,
      "</td>
        </tr> <tr>
          <td style = \"text-align:left; background-color: #ffffff;\">",
      "Population of Legal Service Area: ",
      "</td>
          <td style = \"text-align: right; background-color: #ffffff;\">",
      format(POPU_LSA, big.mark = ","),
      "</td>
        </tr> <tr>
          <td style = \"text-align:left; background-color: #f2f2f2;\">",
      "Visits: ",
      "</td>
          <td style = \"text-align: right; background-color: #f2f2f2;\">",
      format(VISITS, big.mark = ","),
      "</td>
        </tr> <tr>
          <td style = \"text-align:left; background-color: #ffffff;\">",
      "Number of Library Staff: ",
      "</td>
          <td style = \"text-align: right; background-color: #ffffff;\">",
      format(TOT_LIB_STAFF, big.mark = ""),
      "</td>
        </tr> <tr>
          <td style = \"text-align:left; background-color: #f2f2f2;\">",
      "Total FTE of Library Staff: ",
      "</td>
          <td style = \"text-align: right; background-color: #f2f2f2;\">",
      format(TOTSTAFF, big.mark = ","),
      "</td>
        </tr> <tr>
          <td style = \"text-align:left; background-color: #ffffff;\">",
      "Local Government Revenue: ",
      "</td>
          <td style = \"text-align: right; background-color: #ffffff;\">",
      dollar(LOCGVT),
      "</td>
        </tr> <tr>
          <td style = \"text-align:left; background-color: #f2f2f2;\">",
      "State Government Revenue: ",
      "</td>
          <td style = \"text-align: right; background-color: #f2f2f2;\">",
      dollar(STGVT),
      "</td>
        </tr> <tr>
          <td style = \"text-align:left; background-color: #ffffff;\">",
      "Federal Government Revenue: ",
      "</td>
          <td style = \"text-align: right; background-color: #ffffff;\">",
      dollar(FEDGVT),
      "</td>
        </tr> <tr>
          <td style = \"text-align:left; background-color: #f2f2f2;\">",
      "Other Revenue: ",
      "</td>
          <td style = \"text-align: right; background-color: #f2f2f2;\">",
      dollar(OTHINCM),
      "</td>
        </tr> </table>"
    ),
    library_header = case_when(
      CURRENT_LIBNAME_OUTLET != CURRENT_LIBNAME_AE ~
        paste0(
          "<table style='width: 100%'>
           <div style='font-size: 16px;'><b>",
          CURRENT_LIBNAME_AE,
          "</b> </div> <hr>
           <div style='font-size: 14px;'><b>",
          OUTLET_NAME,
          "</b> </div>"
        ),
      CURRENT_LIBNAME_OUTLET == CURRENT_LIBNAME_AE ~
        paste0(
          "<table>
           <div style='font-size: 16px;'><b>",
          CURRENT_LIBNAME_OUTLET,
          "</b>",
          "</div>"
        )
    ),
    library_label = paste0(
      library_header,
      "<div style='font-size: 12px;'>",
      str_to_title(ADDRESS),
      ", ",
      str_to_title(CITY),
      ", ",
      ZIP,
      "<hr><div style='font-size: 12px;'>",
      "Click to see system-wide information",
      "</div> </table>"
    ),
    library_popup = paste0(
      library_data_header,
      library_data_table
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

ce_selected <- eventReactive(input$submitButton, {
  input$outlet_type
})

output$ce_text <- renderUI({
  req(ce_selected())

  if ("CE" %in% ce_selected() && !"BR" %in% ce_selected()) {
    HTML(
      "<hr><em>Note: Emery County, Salt Lake County, and San Juan County do not have a central library.</em>"
    )
  }
})


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
      color = "#002f6c", #~ marker_color(C_OUT_TY)
      label = ~ lapply(library_label, HTML),
      popup = ~ lapply(library_popup, HTML),
      popupOptions = popupOptions(keepInView = TRUE),
    )

  map
})

output$map_year <- renderUI({
  paste0("Utah Public Libraries - ", current_year)
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

output$n_visits <- renderUI({
  map_libs_filtered() %>%
    select(CURRENT_LIBNAME_AE, VISITS) %>%
    distinct() %>%
    reframe(n = format(sum(VISITS, na.rm = T), big.mark = ",")) %>%
    pull(n)
})

output$n_popu_lsa <- renderUI({
  map_libs_filtered() %>%
    select(CURRENT_LIBNAME_AE, POPU_LSA) %>%
    distinct() %>%
    reframe(n = format(sum(POPU_LSA, na.rm = T), big.mark = ",")) %>%
    pull(n)
})

output$n_regbor <- renderUI({
  map_libs_filtered() %>%
    select(CURRENT_LIBNAME_AE, REGBOR) %>%
    distinct() %>%
    reframe(n = format(sum(REGBOR, na.rm = T), big.mark = ",")) %>%
    pull(n)
})
