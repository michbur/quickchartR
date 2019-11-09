#' @title on attaching the package
#' @details installs required packages if they are not present
#' @seealso rjson, caTools
.onAttach <- function (lib, pkgname = "quickchartR") {
  if (!require(rjson))
    install.packages("rjson", repos = "http://cran.us.r-project.org")
  if (!require(caTools))
    install.packages("caTools", repos = "http://cran.us.r-project.org")
  invisible()
}

# ---------------------------------- CONSTANTS ----------------------------------

#' @title TYPES list
#' @details Defines all handled chart types Note that type "sparkline" is not present.
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
#' @author Jacek Myna, Aleksandra Łuczak, Agata Pałdyna, Tomasz Radzikowski, Jan Sawicki
#' @export
TYPES = list("bar",
             "line",
             "radar",
             "pie",
             "doughnut",
             "scatter",
             "bubble")

#' @title Main link to quickchart.io
#' @details Main link to quickchart.io used to construct the final link to chart
MAIN_LINK = paste0("https://quickchart.io/chart?")

# ---------------------------------- FUNCTIONS ----------------------------------


#' @title Check Type Function
#' @param types list of types of charts
#' @details Checks types of charts (verifis with list TYPES)
#' @seealso TYPES
#' @author Jacek Myna, Aleksandra Łuczak, Agata Pałdyna, Tomasz Radzikowski, Jan Sawicki
#' @export
#' @examples
#' types = c("line", "bubble")
#' checkTypes(types)
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
#' @details Takes a data frame with columns "x", "y", "r" and returns a list that will be converted to JSON
#' @author Jacek Myna, Aleksandra Łuczak, Agata Pałdyna, Tomasz Radzikowski, Jan Sawicki
#' @export
inputDataToNamedList = function(dataFrame) {
  namedList = list()
  for (index in 1:nrow(dataFrame)) {
    row = dataFrame[index,]
    newList = list(x = row$x, y = row$y, r = row$r)
    namedList[[length(namedList) + 1]] <- newList
  }
  namedList
}

#
#' @title From NamedList to Datasets
#' @param types list of type of charts
#' @param inputData DataFrame with columns "x","y","r"
#' @param labels Names of columns which be used to build charts
#' @param colors list of background colors which be used
#' @details Prepares datasets in the form of a list to be converted to JSON
#' @author Jacek Myna, Aleksandra Łuczak, Agata Pałdyna, Tomasz Radzikowski, Jan Sawicki
#' @export
getDatasets = function(types,
                       inputData,
                       labels,
                       colors,
                       detailedOptions) {
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
          as.list(inputData[inputData$label == labels[i],]$y),
        backgroundColor = colors[i],
        fill = detailedOptions[[i]]$fill
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
#' @param input_list object which one of elements name is element_name
#' @param element_name name of element without parenthesis
#' @details Allows to take element from the input_list without uses parenthesis (using non-standard evaluation)
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
#' @details Returns a list of unique labels
#' @author Jacek Myna, Aleksandra Łuczak, Agata Pałdyna, Tomasz Radzikowski, Jan Sawicki
#' @export
getLabels = function(labels) {
  as.character(unique(labels))
}
#' @title Prepare Data
#' @param categories list of categories/labels
#' @param datasets list of data which be used to build chart
#' @details Returns list of labels and datasets
#' @author Jacek Myna, Aleksandra Łuczak, Agata Pałdyna, Tomasz Radzikowski, Jan Sawicki
#' @export
prepareData = function(categories, datasets) {
  list(labels = categories, datasets = datasets)
}

#' @title Prepare Json
#' @param mainType string that describes first (main) chart type
#' @param data list of datasets
#' @param options list of options to create Json
#' @details Prepares a list to be converted into Json
#' @author Jacek Myna, Aleksandra Łuczak, Agata Pałdyna, Tomasz Radzikowski, Jan Sawicki
#' @export
prepareJson = function(mainType, data, options) {
  list(type = mainType,
       data = data,
       options = options)
}

#' @title Create Link
#' @param base64 boolean describing if the url should or should not be encoded in base64
#' @param json file with format Json to save in link
#' @details Creates link to given Json
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
#' @param inputData data.frame with columns "x","y","r"
#' @param xData data which be used to plot x axis (column of inputData)
#' @param yData data which be used to plot y axis (column of inputData)
#' @param labels list name of labels
#' @param rData data which be used to plot r axis, e.g. for bubble charts (column of inputData)
#' @param colors list of background colors which be used
#' @param options list of options to create Json
#' @param detailedOptions list of lists of additional options; only "fill" for line graphs is handled
#' @param base64 boolean describing if the url should or should not be encoded in base64
#' @details Creates a link to quickchart with chart/charts based on inputData.
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
           detailedOptions = NULL,
           base64 = T) {
    checkTypes(types)

    xData = substitute(xData)
    yData = substitute(yData)
    labels = substitute(labels)
    rData = substitute(rData)

    inputData$x = getElementNse(inputData, xData)
    inputData$y = getElementNse(inputData, yData)
    inputData$labels = getElementNse(inputData, labels)
    inputData$r = getElementNse(inputData, rData)

    labels = getLabels(inputData$labels)

    # categories == values of x
    categories = unique(inputData$x)

    datasets = getDatasets(types, inputData, labels, colors, detailedOptions)

    data = prepareData(categories, datasets)

    json = toJSON(prepareJson(types[1], data, options))

    createLink(json, base64)
  }
