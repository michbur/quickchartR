library(testthat)
library(quickchartR)

test_that("checkTypes function returns error", {
  expect_error(checkTypes("wrong type"))
})

test_that("checkTypes function returns error", {
  expect_error(checkTypes("bubblle"))
})

test_that("output type of getLabels is character", {
  labels <- 1:10
  expect_type(getLabels(labels), "character")
})

test_that("getLabels returns object of correct length", {
  labels <- c(1,2,3,3,3,4,5,5)
  expect_equal(length(getLabels(labels)), 5)
})

test_that("output type of prepareData is list", {
  categories <- 1:10
  datasets <- 1:10
  expect_type(prepareData(categories, datasets), "list")
})

test_that("output type of prepareJson is list", {
  mainType <- 1:10
  data <- 1:10
  options <- 1:10
  expect_type(prepareJson(mainType, data, options), "list")
})

test_that("output type of createLink is character", {
  expect_type(createLink('aaaaa',T), "character")
})

test_that("output type of createLink cotains 'quickcharts.io'", {
  expect_true(grepl("quickchart.io",createLink('aaaaa',T)))
})

test_that("createLink returns correct output encoded in base64", {
  expect_equal(createLink('aaaaa',T), "https://quickchart.io/chart?&encoding=base64&c=YWFhYWE=")
  expect_equal(createLink('bcdef',T), "https://quickchart.io/chart?&encoding=base64&c=YmNkZWY=")
  expect_equal(createLink('Jacek',T), "https://quickchart.io/chart?&encoding=base64&c=SmFjZWs=")
})


test_that("quickchartR returns correct link not encoded in base64", {
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


  url <- quickchartR(
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

  expect_true(str_detect(url, "line"))
  expect_true(str_detect(url, "bubble"))
  expect_true(str_detect(url, "blue"))
  expect_true(str_detect(url, "red"))
  expect_true(str_detect(url, "TEST"))
  expect_true(str_detect(url, "TITLE"))
  expect_true(str_detect(url, "bottom"))
})

test_that("output type of inputDataToNamedList is list", {
  inputData <- data.frame("x" = 1:10, "y" = 1:10, "r" = 1:10)
  expect_type(inputDataToNamedList(inputData), "list")
})


test_check("quickchartR")
