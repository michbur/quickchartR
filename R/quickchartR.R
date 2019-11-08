.onAttach <- function (lib, pkgname = "quickchartR") {
  ## Put stuff here you want to run when your package is loaded
  if (!require(rjson))
    install.packages("rjson", repos = "http://cran.us.r-project.org")
  if (!require(caTools))
    install.packages("caTools", repos = "http://cran.us.r-project.org")
  invisible()
}

# ---------------------------------- CONSTANTS ----------------------------------

#' Defines all handled chart types
#' Note that type "sparkline" is not present.
#' In its assumption of "simplicity" which ignores labels, axis etc. sparkline chartit
#' is considered a terrible data visualization practice.
#' Even though it is usually used e.g. inside a table to minimize the eye-candy and with
#' a common axis with other charts, implementing it here would cause someone who is not
#' familiar with advanced data visualization and good practices to make a mistake (e.g. by
#' using it regardless of its true purpose).
#' It is assumed that professional data scientists do NOT use Quickcharts for their
#' charts because they have their own advanced tools for it.
#' Therefore, to somehow control and help the newbie data visualizators, sparkline chart
#' is NOT included in quickchartR.
TYPES = list("bar",
             "line",
             "radar",
             "pie",
             "doughnut",
             "scatter",
             "bubble")

MAIN_LINK = paste0("https://quickchart.io/chart?")

# ---------------------------------- FUNCTIONS ----------------------------------

#' @export
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

# takes a data frame with columns x, y, r
# returns a list that will be converted to JSON
#' @export
inputDataToNamedList = function(dataFrame) {
  print(dataFrame)
  namedList = list()
  for (index in 1:nrow(dataFrame)) {
    row = dataFrame[index, ]
    newList = list(x = row$x, y = row$y, r = row$r)
    namedList[[length(namedList) + 1]] <- newList
  }
  namedList
}

# prepares datasets in list form to be converted to json
#' @export
getDatasets = function(types, inputData, labels, colors) {
  datasets = list()

  for (i in 1:length(labels)) {
    if (types[i] == "bubble" || types[i] == "scatter") {
      nextDataset = list(
        label = labels[i],
        data =
          inputDataToNamedList(inputData),
        backgroundColor = colors[i]
      )
    }
    else{
      nextDataset = list(
        label = labels[i],
        data =
          as.list(inputData[inputData$label == labels[i], ]$y),
        backgroundColor = colors[i]
      )
    }

    if (!is.na(types[i])) {
      nextDataset[["type"]] = types[i]
    }

    datasets = c(datasets, list(nextDataset))
  }

  datasets
}

# https://github.com/mini-pw/2020Z-ProgramowanieWR/blob/9f1a3e4364fd372b92ede3635a8ed14f8e3fd430/Prezentacje/P2.Rmd#L157
# gets element (e.g. column) from an object (e.g. dataframe) using NSE, i.e. element_name is not string but object
# e.g. getElementNse(input_list, x) returns input_list$x (x is NOT a string)
#' @export
getElementNse <- function(input_list, element_name) {
  s_element_name <- element_name
  if (as.character(s_element_name) %in% names(input_list)) {
    eval(s_element_name, input_list)
  } else {
    NULL
  }
}

#' @export
getLabels = function(labels) {
  as.character(unique(labels))
}

#' @export
prepareData = function(categories, datasets) {
  list(labels = categories, datasets = datasets)
}

#' @export
prepareJson = function(mainType, data, options) {
  list(type = mainType,
       data = data,
       options = options)
}

#' @export
createLink = function(json, base64) {
  paste0(
    MAIN_LINK,
    ifelse(base64, "&encoding=base64", ""),
    "&c=",
    ifelse(base64, base64encode(json), json)
  )
}

#' @title main function
#'
#' @export
quickchartR <-
  function(types,
           inputData,
           xData,
           yData,
           labels,
           rData = NULL,
           colors = NULL,
           options = NULL,
           additionalGraphsData = NULL,
           base64 = T) {
    checkTypes(types)

    xData = substitute(xData)
    yData = substitute(yData)
    labels = substitute(labels)
    rData = substitute(rData)

    inputData$x = getElementNse(inputData, xData)
    inputData$y = getElementNse(inputData, yData)
    inputData$labels = getElementNse(inputData, labels)
    #if (!is.null(rData))
    #{
    inputData$r = getElementNse(inputData, rData)
    #}

    labels = getLabels(inputData$labels)

    # categories == values of x
    categories = unique(inputData$x)

    datasets = getDatasets(types, inputData, labels, colors)

    data = prepareData(categories, datasets)

    json = toJSON(prepareJson(types[1], data, options))

    createLink(json, base64)
  }
