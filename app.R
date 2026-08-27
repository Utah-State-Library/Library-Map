# Load necessary packages - IF THERE ARE ANY CHANGES, run:
# rsconnect::writeManifest()
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
library(bslib)
library(bsicons)
library(sjmisc)
library(htmlwidgets)
library(thematic)
library(scales)
library(sf)


#### Color Palette ####
# head_color <- "#002F6C"
# sub1_color <- "#0086BF"
# sub2_color <- "#4EC3E0"

#new
# #093692
# #81D0F0
# #FFB81D

#### Set Options ####
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)
options(scipen = 999)

#### Load Data ####

# pls <- readRDS("data/processed/pls_ut_app.RDS")
# pls_ut_simlibs <- readRDS("data/processed/pls_ut_simlibs.RDS")
pls_national <- readRDS("data/processed/pls_national_app.RDS")

# All Pages
variable_key <- read.csv("data/pls_variable_key.csv")

# Library Map Page
map_all <- readRDS("data/processed/library_map_app.RDS")
outlets <- readRDS("data/processed/outlet_ut_app.RDS")

# State Comp Page
pls_national_state <- readRDS("data/processed/pls_national_state_appv2.RDS")
pls_national_state_map <- read_sf(
  "data/processed/pls_national_state_map_appv2.shp"
)

# Utah Library Comp Page
pls_utah <- readRDS("data/processed/pls_utah_appv2.RDS")

# Peer Page
pls_national_simlibs <- readRDS("data/processed/pls_national_simlibs.RDS")
pls_national_peers <- readRDS("data/processed/pls_national_peers_appv2.RDS")


#### Global Values ####

current_year <- max(as.numeric(outlets$FISCAL_YEAR))
imls_year <- pls_national_state %>%
  filter(STABR != "UT") %>%
  summarise(max(FISCAL_YEAR)) %>%
  pull()

national_vars <- setdiff(
  names(pls_national),
  c(
    "FISCAL_YEAR",
    "CURRENT_LIBNAME",
    "CURRENT_LIBNAME_DISAMB",
    "FSCSKEY",
    "STABR",
    "state",
    "POPU_LSA"
  )
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

csvDownloadButton <- function(
  id,
  filename = "data.csv",
  label = "Download as CSV"
) {
  tags$button(
    tagList(icon("download"), label),
    onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", id, filename)
  )
}


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

    tags$head(
      tags$script(
        HTML(
          "$(document).ready(function () {",
          "  $('body').on('click', function (e) {",
          "    $('[data-bs-toggle=popover]').each(function () {",
          "      if (!$(this).is(e.target) &&",
          "          $(this).has(e.target).length === 0 &&",
          "          $('.popover').has(e.target).length === 0) {",
          "        $(this).popover('hide');",
          "      }",
          "    });",
          "  });",
          "})"
        )
      )
    ),

    source("RScripts/state_service_ui.R", local = TRUE)$value,
    source("RScripts/national_compare_ui.R", local = TRUE)$value,
    source("RScripts/lib_ui.R", local = TRUE)$value,
    #source("RScripts/singlib_ui.R", local = TRUE)$value,
    #nav_spacer(),
    source("RScripts/methodology.R", local = TRUE)$value
  )
)


#### Server ####
server <- function(input, output, session) {
  source("RScripts/state_service_server.R", local = TRUE)$value
  source("RScripts/national_compare_server.R", local = TRUE)$value
  source("RScripts/lib_server.R", local = TRUE)$value
  #source("RScripts/singlib_server.R", local = TRUE)$value
}


#### Run App ####
shinyApp(
  ui = ui,
  server = server
)
