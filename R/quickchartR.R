.onAttach <- function (lib, pkgname = "quickchartR") {
  ## Put stuff here you want to run when your package is loaded
  if (!require(rjson))
    install.packages("rjson", repos = "http://cran.us.r-project.org")
  if (!require(caTools))
    install.packages("caTools", repos = "http://cran.us.r-project.org")
  invisible()
}

# ---------------------------------- CONSTANTS ----------------------------------

# "radialGauge" type is not included
# "sparkline" type is not included because they are a terrible visualizations
TYPES = list("bar",
             "line",
             "radar",
             "pie",
             "doughnut",
             "scatter",
             "bubble")

MAIN_LINK = paste0("https://quickchart.io/chart?")

# ---------------------------------- FUNCTIONS ----------------------------------


#' @title Check Type Function
#' @param types list of type of charts
#' @details Function checks type of charts wchich we want to draw.
#' @author Jacek Myna, Aleksandra Łuczak, Agata Pałdyna, Tomasz Radzikowski, Jan Sawicki
#' @export
#' @examples
#' types = c("line", "bubble")
#' checkType(types)
#'
#' checkType(c('xxx','yyy'))
#' # Incorrect chart type.
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
#' @title Data To NamedList
#' @param dataFrame DataFrame with columns "x","y","r"
#' @details Function takes a data frame with columns "x", "y", "r" and returns a list that will be converted to JSON
#' @author Jacek Myna, Aleksandra Łuczak, Agata Pałdyna, Tomasz Radzikowski, Jan Sawicki
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

#
#' @title From NamedList to Datasets
#' @param types list of type of charts
#' @param inputData DataFrame with columns "x","y","r"
#' @param labels Names of columns wchich be used to bulid charts
#' @param colors list of background colors wchich be used
#' @details Function prepares datasets in list form to be converted to json
#' @author Jacek Myna, Aleksandra Łuczak, Agata Pałdyna, Tomasz Radzikowski, Jan Sawicki
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


#' @title Get Element NSE
#' @param input_list object wchich one of elemets name is element_name
#' @param element_name name of element without parethesis
#' @details Function allows to take element from the input_list without uses parethesis
#' @author Jacek Myna, Aleksandra Łuczak, Agata Pałdyna, Tomasz Radzikowski, Jan Sawicki
#' @export
getElementNse <- function(input_list, element_name) {
  s_element_name <- element_name
  if (as.character(s_element_name) %in% names(input_list)) {
    eval(s_element_name, input_list)
  } else {
    NULL
  }
}
#' @title Get Labels
#' @param labels list of labels
#' @details Function returns list of unique labels
#' @author Jacek Myna, Aleksandra Łuczak, Agata Pałdyna, Tomasz Radzikowski, Jan Sawicki
#' @export
getLabels = function(labels) {
  as.character(unique(labels))
}
#' @title Prepare Data
#' @param categories list of categories/labels
#' @param datasets list of data wchch be used to build chart
#' @details Function returns list of labels and datasets
#' @author Jacek Myna, Aleksandra Łuczak, Agata Pałdyna, Tomasz Radzikowski, Jan Sawicki
#' @export
prepareData = function(categories, datasets) {
  list(labels = categories, datasets = datasets)
}
#' @title Prepare Json
#' @param mainType XXXXXXXXXXXXXX
#' @param data list of datasets
#' @param options list of options to create Json
#' @details Functons prepare objetc to create Json
#' @author Jacek Myna, Aleksandra Łuczak, Agata Pałdyna, Tomasz Radzikowski, Jan Sawicki
#' @export
prepareJson = function(mainType, data, options) {
  list(type = mainType,
       data = data,
       options = options)
}

#' @title Create Link
#' @param base64 XXXXXXXXXXXXXXXXXXX
#' @param json file with format Json to save in link
#' @details Functions create link to given Json
#' @author Jacek Myna, Aleksandra Łuczak, Agata Pałdyna, Tomasz Radzikowski, Jan Sawicki
#' @export
createLink = function(json, base64) {
  paste0(
    MAIN_LINK,
    ifelse(base64, "&encoding=base64", ""),
    "&c=",
    ifelse(base64, base64encode(json), json)
  )
}

#' @title Main function
#' @param types list of types of charts
#' @param inputData DataFrame with columns "x","y","r"
#' @param xData data wchich be used to plot xaxis
#' @param yData data wchich be used to plot yaxis
#' @param labels list name of labels
#' @param rData XXXXXXXXXXXXXx
#' @param colors list of background colors wchich be used
#' @param options list of options to create Json
#' @param additionalGraphsData XXXXXXXXXXXXXXXXXXXXXXX
#' @param base64 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#' @details Functions create same basic charts from inputData.
#' @author Jacek Myna, Aleksandra Łuczak, Agata Pałdyna, Tomasz Radzikowski, Jan Sawicki
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
