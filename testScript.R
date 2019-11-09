require(quickchartR)

types = c("bar", "line", "bubble")
inputData = data.frame(
  myXColumn = rep(1:6, 3),
  myYColumn = sample(1:18, 18),
  myRColumn = sample(1:18, 9),
  myLabelColumn = rep(letters[1:3], 6)
)

colors = c('blue', 'red')
options = list(
  title = list(
    display = T,
    text = "My first quickchart",
    fontSize = 32
  ),
  legend = list(position = "bottom")
)
detailedOptions = list(list(), list(fill = "false"), list())

browseURL(
  quickchartR(
    types = types,
    inputData = inputData,
    xData = myXColumn,
    yData = myYColumn,
    rData = myRColumn,
    labels = myLabelColumn,
    #colors = colors,
    options = options,
    detailedOptions = detailedOptions,
    base64 = F
  )
)

