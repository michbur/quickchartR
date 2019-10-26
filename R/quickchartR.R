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
require(caTools)

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
quickchartR <-
  function(type,
           inputData,
           colors = NULL,
           options = NULL,
           base64 = T) {
    checkType(type)

    MAIN_LINK = paste0("https://quickchart.io/chart?")

    labels = as.character(unique(inputData$label))

    # categories == values of x
    categories = unique(inputData$x)

    datasets = list()

    for (i in 1:length(labels)) {
      datasets = list.append(datasets,
                             list(
                               label = labels[i],
                               data =
                                 as.list(inputData[inputData$label == labels[i],]$y),
                               backgroundColor = colors[i]
                             ))
    }

    data = list(labels = categories, datasets = datasets)

    json <- list(type = type,
                 data = data,
                 options = options)
    json = toJSON(json)

    paste0(
      MAIN_LINK,
      ifelse(base64, "&encoding=base64", ""),
      "&c=",
      ifelse(base64, base64encode(json), json)
    )
  }
