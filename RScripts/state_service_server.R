##### Sync Inputs #####

# Filter AE list based on what counties are selected
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

##### Context text #####

# if an AE is selected that does not have a central library, make context text appear

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
        CNTY %in% input$st_county, # filter to selected counties
        CURRENT_LIBNAME_AE %in% input$ae, # filter to selected libraries
        C_OUT_TY %in% input$outlet_type, # filter to selected outlet type
        SERVICE_AREA %in% input$system_type # filter to selected system type
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

  # Note: We're using the Carto basemap, and they've added the need for an API key as of 8/26ish. I've added the key below, but it may need to be updated since it has my email attached. If it does need updating you can do that by visiting https://carto.com/basemaps/apikey/ and filling out the request. It's free for the first 5,000,000 requests per month, so we're good :)
  # For the future: implement a secret file and store the api key there so that when we push to github it's not public facing

  map <- leaflet(
    options = leafletOptions(zoomControl = FALSE),
  ) %>%
    addProviderTiles(
      "CartoDB.Positron",
      group = "CartoDB.Positron"
    ) %>%
    onRender(
      paste0(
        "function(el, x) {
          L.control.zoom({position:'bottomright'}).addTo(this);  

          L.tileLayer('https://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}.png?key=cb1_2e4a_1_b730580af8930712da7b58b8', {
          attribution: '&copy; <a href=\"https://www.openstreetmap.org/copyright\">OpenStreetMap</a>, &copy; <a href=\"https://carto.com/attributions\">CARTO</a>',
          subdomains: 'abcd', maxZoom: 20
          }).addTo(this);
        }"
      )
    )

  ## Show Library Locations
  map <- map %>%
    addCircleMarkers(
      data = map_df,
      lng = ~LONG,
      lat = ~LAT,
      radius = 4,
      fillOpacity = .75,
      color = "#093692", #~ marker_color(C_OUT_TY)
      label = ~ lapply(library_label, HTML),
      popup = ~ lapply(library_popup, HTML),
      popupOptions = popupOptions(keepInView = TRUE),
    )

  map
})

# Map data year
output$map_year <- renderUI({
  paste0("Utah Public Libraries - ", current_year)
})


#### Value Box Values ####

# Number of libraries
output$n_aes <- renderUI({
  map_libs_filtered() %>%
    reframe(n = paste0(n_distinct(CURRENT_LIBNAME_AE), " Libraries")) %>%
    pull(n)
})

# Number of locations
output$n_locations <- renderUI({
  map_libs_filtered() %>%
    reframe(
      n = paste0(n_distinct(CURRENT_LIBNAME_OUTLET), " Locations")
    ) %>%
    pull(n)
})

# Number of City libraries
output$n_citylibs <- renderUI({
  map_libs_filtered() %>%
    filter(SERVICE_AREA == "city") %>%
    reframe(n = paste0("City Libraries: ", n_distinct(CURRENT_LIBNAME_AE))) %>%
    pull(n)
})

# Number of County Libraries
output$n_countylibs <- renderUI({
  map_libs_filtered() %>%
    filter(SERVICE_AREA == "county") %>%
    reframe(
      n = paste0("County Libraries: ", n_distinct(CURRENT_LIBNAME_AE))
    ) %>%
    pull(n)
})

# Number of Visits
output$n_visits <- renderUI({
  map_libs_filtered() %>%
    select(CURRENT_LIBNAME_AE, VISITS) %>%
    distinct() %>%
    reframe(
      n = paste0(format(sum(VISITS, na.rm = T), big.mark = ","))
    ) %>%
    pull(n)
})

# Number of Programs
output$n_pro <- renderUI({
  map_libs_filtered() %>%
    select(CURRENT_LIBNAME_AE, TOTPRO) %>%
    distinct() %>%
    reframe(
      n = paste0("Programs: ", format(sum(TOTPRO, na.rm = T), big.mark = ","))
    ) %>%
    pull(n)
})

# Program Attendance
output$n_atten <- renderUI({
  map_libs_filtered() %>%
    select(CURRENT_LIBNAME_AE, TOTATTEN) %>%
    distinct() %>%
    reframe(
      n = paste0(
        "Program Attendance: ",
        format(sum(TOTATTEN, na.rm = T), big.mark = ",")
      )
    ) %>%
    pull(n)
})

# Total Circulation
output$n_circ <- renderUI({
  map_libs_filtered() %>%
    select(CURRENT_LIBNAME_AE, TOTCIR) %>%
    distinct() %>%
    reframe(
      n = paste0(
        "Total Circulation: ",
        format(sum(TOTCIR, na.rm = T), big.mark = ",")
      )
    ) %>%
    pull(n)
})

# Total Kids Circulation
output$n_kidcirc <- renderUI({
  map_libs_filtered() %>%
    select(CURRENT_LIBNAME_AE, KIDPHYSCIR) %>%
    distinct() %>%
    reframe(
      n = paste0(
        "Children's Circulation: ",
        format(sum(KIDPHYSCIR, na.rm = T), big.mark = ",")
      )
    ) %>%
    pull(n)
})

# Total Operating Revenue
output$n_totincm <- renderUI({
  map_libs_filtered() %>%
    select(CURRENT_LIBNAME_AE, TOTINCM) %>%
    distinct() %>%
    reframe(
      n = paste0(
        "$",
        format(sum(TOTINCM, na.rm = T), big.mark = ",")
      )
    ) %>%
    pull(n)
})

# Local Operating Revenue
output$n_locgvt <- renderUI({
  map_libs_filtered() %>%
    select(CURRENT_LIBNAME_AE, LOCGVT) %>%
    distinct() %>%
    reframe(
      n = paste0(
        "Local: $",
        format(sum(LOCGVT, na.rm = T), big.mark = ",")
      )
    ) %>%
    pull(n)
})

# State Operating Revenue
output$n_stgvt <- renderUI({
  map_libs_filtered() %>%
    select(CURRENT_LIBNAME_AE, STGVT) %>%
    distinct() %>%
    reframe(
      n = paste0(
        "State: $",
        format(sum(STGVT, na.rm = T), big.mark = ",")
      )
    ) %>%
    pull(n)
})

# Federal Operating Revenue
output$n_fedgvt <- renderUI({
  map_libs_filtered() %>%
    select(CURRENT_LIBNAME_AE, FEDGVT) %>%
    distinct() %>%
    reframe(
      n = paste0(
        "Federal: $",
        format(sum(FEDGVT, na.rm = T), big.mark = ",")
      )
    ) %>%
    pull(n)
})

# Other Operating Revenue
output$n_othincm <- renderUI({
  map_libs_filtered() %>%
    select(CURRENT_LIBNAME_AE, OTHINCM) %>%
    distinct() %>%
    reframe(
      n = paste0(
        "Other: $",
        format(sum(OTHINCM, na.rm = T), big.mark = ",")
      )
    ) %>%
    pull(n)
})

# Population of legal service area
output$n_popu_lsa <- renderUI({
  map_libs_filtered() %>%
    select(CURRENT_LIBNAME_AE, POPU_LSA) %>%
    distinct() %>%
    reframe(
      n = paste0(
        "Population Served: ",
        format(sum(POPU_LSA, na.rm = T), big.mark = ",")
      )
    ) %>%
    pull(n)
})

# Number of Cardholders (registered borrowers)
output$n_regbor <- renderUI({
  map_libs_filtered() %>%
    select(CURRENT_LIBNAME_AE, REGBOR) %>%
    distinct() %>%
    reframe(
      n = paste0(
        "Cardholders: ",
        format(sum(REGBOR, na.rm = T), big.mark = ",")
      )
    ) %>%
    pull(n)
})

# Percent of Utahns that are cardholders (knowing that some people do have multiple cards; not a perfect measure but it's what we have)
output$n_pcnt_regbor <- renderUI({
  map_libs_filtered() %>%
    select(CURRENT_LIBNAME_AE, REGBOR, POPU_LSA) %>%
    distinct() %>%
    reframe(
      sum_regbor = sum(REGBOR, na.rm = T),
      sum_populsa = sum(POPU_LSA, na.rm = T),
      n = paste0(
        format(round((sum_regbor / sum_populsa) * 100, 2), big.mark = ","),
        "%"
      )
    ) %>%
    pull(n)
})
