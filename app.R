# Load necessary packages
# library(highcharter)
library(tidyverse)
library(magrittr)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(DT)
library(leaflet)
library(shinycssloaders)
# library(reactable)
# library(reactablefmtr)
library(bslib)
library(bsicons)
library(shinyalert)
library(sjmisc)
library(htmlwidgets)
# library(sf)
library(thematic)
library(fontawesome)

#### Color Palette ####
# head_color <- "#002F6C"
# sub1_color <- "#0086BF"
# sub2_color <- "#4EC3E0"

#### Set Options ####
# hcoptslang <- getOption("highcharter.lang")
# hcoptslang$thousandsSep <- ","
# options(highcharter.lang = hcoptslang)

#### Load Data ####

outlets <- readRDS("data/pls_outlet_national.rds") %>%
  filter(STABR == "UT", hide_lib == 0, FISCAL_YEAR == max(FISCAL_YEAR)) %>%
  mutate(
    CITY = case_when(
      CITY == "South Salt Lake City" ~ "South Salt Lake",
      CITY == "Mt. Pleasant" ~ "Mount Pleasant",
      .default = CITY
    ),
  )

counties <- outlets %>%
  reframe(unique(CNTY)) %>%
  pull() %>%
  sort()

aes <- outlets %>%
  reframe(unique(CURRENT_LIBNAME_AE)) %>%
  pull() %>%
  sort()

current_year <- max(as.numeric(outlets$FISCAL_YEAR))

source("RScripts/theme.R", local = TRUE)$value

#### UI ####

ui <- fluidPage(
  theme = usl_theme,

  page_navbar(
    title = strong("Utah Public Libraries - Central Library and Branch Map"),
    navbar_options = navbar_options(
      bg = NULL,
      underline = TRUE
    ),
    shiny::includeCSS("www/styles.css"),

    source("RScripts/state_service_ui.R", local = TRUE)$value,
    nav_spacer(),
    #source("RScripts/about_ui.R", local = TRUE)$value
  )
)

#### Server ####
server <- function(input, output, session) {
  source("RScripts/state_service_server.R", local = TRUE)$value
}


#### Run App ####
shinyApp(
  ui = ui,
  server = server
)
