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

# Define the current max year. I used the outlets df for this but really you can do it with any df that has utah data
current_year <- max(as.numeric(outlets$FISCAL_YEAR))

# We want to be able to see what the most recent IMLS data year is, and this will help us throughout the app
imls_year <- pls_national_state %>%
  filter(STABR != "UT") %>% # filter out UT because we have added in our 2025 data already
  summarise(max(FISCAL_YEAR)) %>%
  pull()

# This is the variable list that users will be able to select from. We're doing it this way because we may add additional columns to pls_national in the future, and this method means that we don't have to manually adjust a variable list.
# Note, if any extra ID columns get added please include them in the list below because they are not variables to show; variables being those that users can select from to see library metrics, e.g., REGBOR, TOTSTAFF, TOTINCM, etc.
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

# This is a crosswalk between the shortnames and the longer names. So, instead of users selecting TOTINCM, they'll select Total Operating Revenue
national_vars_pretty <- variable_key %>%
  filter(SHORTNAME %in% national_vars) %>%
  select(GROUP, SHORTNAME, INDICATOR)


## Define values used for filtering

states <- unique(pls_national$state) %>% sort()

counties <- outlets %>%
  reframe(unique(CNTY)) %>%
  pull() %>%
  sort()

aes <- outlets %>% # aes = library (at the system level; administrative entity)
  reframe(unique(CURRENT_LIBNAME_AE)) %>%
  pull() %>%
  sort()

aes_national <- pls_national_simlibs %>% # all library systems in the country
  reframe(unique(CURRENT_LIBNAME_DISAMB)) %>%
  pull() %>%
  sort()

## load in values from other R scripts. These are in different files because they can get a bit long, but technically the code in these files could just be in this app.R file. I just like to keep things grouped and as clean as possible. Plus, it's easier to just open the lists file if you need to adjust anything rather than scrolling through this file trying to find what you need.
source("RScripts/lists.R", local = TRUE)$value
source("RScripts/theme.R", local = TRUE)$value


## Define function - note, if you want to add more functions I would just make a new function file and source it like we're doing just above. I'm defining this function here because it's so short
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
# This is the structure of the overall app. You can use this UI section to move pages around and change some elements of the overall styling

ui <- fluidPage(
  theme = usl_theme, # sourced from theme.R

  page_navbar(
    title = strong("Utah Public Libraries"), # Dashboard title
    navbar_options = navbar_options(
      bg = NULL, # background color
      underline = TRUE # underline pages
    ),
    shiny::includeCSS("www/styles.css"), # load in our css for styling

    ## this section makes it so you can click outside of a popup to close it. Without this you have to click the X button and it's a little annoying. If you add more javascript like this I would consider putting it in a separate file and sourcing it here just to keep things clean
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

    ## Source Page UIs
    # You can easily switch these around if you want to change the order that the pages appear on the navbar
    source("RScripts/state_service_ui.R", local = TRUE)$value,
    source("RScripts/national_compare_ui.R", local = TRUE)$value,
    source("RScripts/lib_ui.R", local = TRUE)$value,
    #source("RScripts/singlib_ui.R", local = TRUE)$value, ##started working on this but haven't quite finished
    #nav_spacer(), ## use this if you want there to be a gap between the actual dashboard style data pages and the methodology page
    source("RScripts/methodology.R", local = TRUE)$value
  )
)


#### Server ####
server <- function(input, output, session) {
  ## Source Page Servers
  # Even if you swap around the pages in the UI it shouldn't matter what the order is here. I do like to keep them in order though just to make things nice and tidy
  source("RScripts/state_service_server.R", local = TRUE)$value
  source("RScripts/national_compare_server.R", local = TRUE)$value
  source("RScripts/lib_server.R", local = TRUE)$value
  #source("RScripts/singlib_server.R", local = TRUE)$value ##started working on this but haven't quite finished
}


#### Run App ####
shinyApp(
  ui = ui,
  server = server
)
