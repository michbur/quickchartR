context("Your first test")

test_that("Tests are running--delete this and put in real tests!", {
  types = c("bar", "line")
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

  browseURL(quickchartR(types, inputData, colors, options, base64 = F))

  succeed()
})
