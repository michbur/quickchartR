require(quickchartR)

types = c("line", "bubble")
inputData = data.frame(
  myXColumn = rep(1:3, 2),
  myYColumn = 2 * 1:6,
  yRColumn = 1:6,
  myLabelColumn = rep(letters[1:2], 3)
)

colors = c('blue', 'red')
options = list(
  title = list(
    display = T,
    text = "TEST TITLE",
    fontSize = 32
  ),
  legend = list(position = "bottom")
)

browseURL(
  quickchartR(
    types = types,
    inputData = inputData,
    xData = myXColumn,
    yData = myYColumn,
    rData = myRColumn,
    labels = myLabelColumn,
    colors = colors,
    options = options,
    base64 = F
  )
)
