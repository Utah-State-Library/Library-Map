usl_theme <- bs_theme(
  version = 5,
  bootswatch = NULL,
  bg = "#f8f9fa",
  fg = "#212529",
  primary = "#002f6C",
  secondary = "#0086BF",
  success = "#7DA267",
  info = "#66748F",
  warning = "#E9CF72",
  danger = "#AF4646",
  base_font = "Raleway",
  code_font = NULL,
  heading_font = bslib::font_google("Raleway")
)

options(shiny.useragg = TRUE)
thematic_shiny(font = "auto")
