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
require(rlist)

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

#'
#' @export
quickchartR <- function(type, inputData) {
  checkType(type)

  MAIN_LINK = "https://quickchart.io/chart?c="

  labels = as.character(unique(inputData$label))

  # categories == values of x
  categories = unique(inputData$x)

  datasets = list()

  for (label in labels) {
    datasets = list.append(datasets, list(label = label, data =
                                            as.list(inputData[inputData$label == label, ]$y)))
  }

  data = list(labels = categories, datasets = datasets)

  json <- list(type = type,
               data = data)
  json = toJSON(json)

  paste0(MAIN_LINK, json)
}
