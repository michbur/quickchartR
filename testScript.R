require(quickchartR)

type = "bar"
inputData = data.frame(x = rep(1:3, 2),
                       y = 2 * 1:6,
                       label = rep(letters[1:2], 3))
colors = c('blue', 'red')
options = list(
  title = list(
    display = T,
    text = "TEST TITLE",
    fontSize = 32
  ),
  legend = list(position = "bottom")
)


browseURL(quickchartR(type, inputData, colors, options))
