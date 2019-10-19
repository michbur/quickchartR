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
  "sparkline",
  ""
)

checkType = function(type) {
  if (!(type %in% TYPES)) {
    stop(paste0("Incorrect chart type. Types: ", paste(TYPES, collapse = ', ')))
  }
}

quickchartR <- function(type, data) {
  checkType(type)

  MAIN_LINK = "https://quickchart.io/chart?c="

  json <- list(type = type,
               data = data)
  json = toJSON(json)

  paste0(MAIN_LINK, json)
}

type = "bar"
labels = c('a', 'b', 'c', 'd', 'e')

datasets = list(list(label = "X", data = list(1, 2, 3)), list(label = "Y", data =
                                                                list(2, 4, 6)))
data = list(labels = labels, datasets = datasets)

url = quickchartR(type, data)

browseURL(url)
