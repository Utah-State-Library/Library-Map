# Load necessary packages
library(highcharter)
library(tidyverse)
library(magrittr)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(DT)
library(leaflet)
library(shinycssloaders)
library(reactable)
library(reactablefmtr)
library(bslib)
library(bsicons)
library(shinyalert)
library(sjmisc)
library(htmlwidgets)
library(thematic)
library(fontawesome)
library(scales)
library(sf)

#### Color Palette ####
# head_color <- "#002F6C"
# sub1_color <- "#0086BF"
# sub2_color <- "#4EC3E0"

#### Set Options ####
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)
options(scipen = 999)

#rsconnect::writeManifest()

#### Load Data ####

variable_key <- read.csv("data/pls_variable_key.csv")
outlets <- readRDS("data/processed/outlet_ut_app.RDS")
pls <- readRDS("data/processed/pls_ut_app.RDS")
pls_national <- readRDS("data/processed/pls_national_app.RDS")
pls_utah <- readRDS("data/processed/pls_utah_app.RDS")
pls_national_state <- readRDS("data/processed/pls_national_state_app.RDS")
pls_national_state_map <- read_sf(
  "data/processed/pls_national_state_map_app.shp"
)
pls_national_peers <- readRDS("data/processed/pls_national_peers_app.RDS")
pls_national_simlibs <- readRDS("data/processed/pls_national_simlibs.RDS")

current_year <- max(as.numeric(outlets$FISCAL_YEAR))

national_vars <- setdiff(
  names(pls_national),
  c("FISCAL_YEAR", "CURRENT_LIBNAME_DISAMB", "STABR", "state", "POPU_LSA")
)

national_vars_pretty <- variable_key %>%
  filter(SHORTNAME %in% national_vars) %>%
  select(GROUP, SHORTNAME, INDICATOR)

states <- unique(pls_national$state) %>% sort()

counties <- outlets %>%
  reframe(unique(CNTY)) %>%
  pull() %>%
  sort()

aes <- outlets %>%
  reframe(unique(CURRENT_LIBNAME_AE)) %>%
  pull() %>%
  sort()

aes_national <- pls_national_simlibs %>%
  reframe(unique(CURRENT_LIBNAME_DISAMB)) %>%
  pull() %>%
  sort()

source("RScripts/lists.R", local = TRUE)$value
source("RScripts/theme.R", local = TRUE)$value

#### UI ####

ui <- fluidPage(
  theme = usl_theme,

  page_navbar(
    title = strong("Utah Public Libraries"),
    navbar_options = navbar_options(
      bg = NULL,
      underline = TRUE
    ),
    shiny::includeCSS("www/styles.css"),

    source("RScripts/state_service_ui.R", local = TRUE)$value,
    source("RScripts/national_compare_ui.R", local = TRUE)$value,
    source("RScripts/utah_compare_ui.R", local = TRUE)$value,
    #source("RScripts/single_library_ui.R", local = TRUE)$value,
    nav_spacer(),
    #source("RScripts/about_ui.R", local = TRUE)$value
  )
)

#### Server ####
server <- function(input, output, session) {
  source("RScripts/state_service_server.R", local = TRUE)$value
  source("RScripts/national_compare_server.R", local = TRUE)$value
  source("RScripts/utah_compare_server.R", local = TRUE)$value
  #source("RScripts/single_library_server.R", local = TRUE)$value
}


#### Run App ####
shinyApp(
  ui = ui,
  server = server
)
