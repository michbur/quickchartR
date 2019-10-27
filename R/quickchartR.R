# Here's a good place to put your top-level package documentation

.onAttach <- function (lib, pkgname = "quickchartR") {
  ## Put stuff here you want to run when your package is loaded
  invisible()
}

#install.packages("rjson")
#install.packages("rlist")
#install.packages("caTools")

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

checkTypes = function(types) {
  for (type in types) {
    if (!(type %in% TYPES)) {
      stop(paste0(
        "Incorrect chart type. Types: ",
        paste(TYPES, collapse = ', ')
      ))
    }
  }
}

getDatasets = function(types, inputData, labels, colors) {
  datasets = list()

  for (i in 1:length(labels)) {
    nextDataset = list(
      label = labels[i],
      data =
        as.list(inputData[inputData$label == labels[i], ]$y),
      backgroundColor = colors[i]
    )

    if (!is.na(types[i])) {
      nextDataset[["type"]] = types[i]
    }

    datasets = list.append(datasets,
                           nextDataset)
  }

  datasets
}

#' @title hello function
#'
#' @export
quickchartR <-
  function(types,
           inputData,
           colors = NULL,
           options = NULL,
           additionalGraphsData = NULL,
           base64 = T) {
    checkTypes(types)

    MAIN_LINK = paste0("https://quickchart.io/chart?")

    labels = as.character(unique(inputData$label))

    # categories == values of x
    categories = unique(inputData$x)

    datasets = getDatasets(types, inputData, labels, colors)

    data = list(labels = categories, datasets = datasets)

    json <- list(type = types[1],
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
