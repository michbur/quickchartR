# Here's a good place to put your top-level package documentation

.onAttach <- function (lib, pkgname = "quickchartR") {
  ## Put stuff here you want to run when your package is loaded
  if (!require(rjson))
    install.packages("rjson", repos = "http://cran.us.r-project.org")
  if (!require(caTools))
    install.packages("caTools", repos = "http://cran.us.r-project.org")
  invisible()
}

# radialGauges are not included
#"radialGauge",
# sparklines are not included because they are a terrible visualization
#"sparkline"
TYPES = list("bar",
             "line",
             "radar",
             "pie",
             "doughnut",
             "scatter",
             "bubble")

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

inputDataToNamedList = function(dataFrame) {
  print(dataFrame)
  namedList = list()
  for (index in 1:nrow(dataFrame)) {
    row = dataFrame[index,]
    newList = list(x = row$x, y = row$y, r = row$r)
    namedList[[length(namedList) + 1]] <- newList
  }
  namedList
}

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
          as.list(inputData[inputData$label == labels[i],]$y),
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

# https://github.com/mini-pw/2020Z-ProgramowanieWR/blob/9f1a3e4364fd372b92ede3635a8ed14f8e3fd430/Prezentacje/P2.Rmd#L112
getElementNse <- function(input_list, element_name) {
  s_element_name <- element_name
  if (as.character(s_element_name) %in% names(input_list)) {
    eval(s_element_name, input_list)
  } else {
    NULL
  }
}

#' @title hello function
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


    print("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    inputData$x = getElementNse(inputData, xData)
    inputData$y = getElementNse(inputData, yData)
    inputData$labels = getElementNse(inputData, labels)
    if (!is.null(rData))
    {
      inputData$r = getElementNse(inputData, rData)
    }

    MAIN_LINK = paste0("https://quickchart.io/chart?")

    labels = as.character(unique(inputData$labels))

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
