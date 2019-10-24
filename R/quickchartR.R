# Here's a good place to put your top-level package documentation

.onAttach <- function (lib, pkgname = "quickchartR") {
  ## Put stuff here you want to run when your package is loaded
  invisible()
}

#' @title hello function
#'
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

require(rjson)

TYPES = list(
  "bar",
  "line",
  "radar",
  "pie",
  "doughnut",
  "scatter",
  "bubble",
  "radialGauge",
  "sparkline"
)

checkType = function(type) {
  if (!(type %in% TYPES)) {
    stop(paste0("Incorrect chart type. Types: ", paste(TYPES, collapse = ', ')))
  }
}

quickchartR <- function(type, data) {
  checkType(type)

  MAIN_LINK = "https://quickchart.io/chart?c222222222222222222222="

  json <- list(type = type,
               data = data)
  json = toJSON(json)

  cat(MAIN_LINK, json)
}
