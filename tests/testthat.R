library(testthat)
library(quickchartR)

test_that("output type of getLabels", {
  labels <- 1:10
  expect_type(getLabels(labels), "character")
})

test_that("uniqueness test of getLabels", {
  labels <- c(1,2,3,3,3,4,5,5)
  expect_equal(length(getLabels(labels)), 5)
})

test_that("output type of prepareData", {
  categories <- 1:10
  datasets <- 1:10
  expect_type(prepareData(categories, datasets), "list")
})

test_that("output type of prepareJson", {
  mainType <- 1:10
  data <- 1:10
  options <- 1:10
  expect_type(prepareJson(mainType, data, options), "list")
})

test_that("output type of createLink", {
  expect_type(createLink('aaaaa',T), "character")
})

test_that("corectness of link in createLink", {
  expect_equal(createLink('aaaaa',T), "https://quickchart.io/chart?&encoding=base64&c=YWFhYWE=")
  expect_equal(createLink('bcdef',T), "https://quickchart.io/chart?&encoding=base64&c=YmNkZWY=")
  expect_equal(createLink('Jacek',T), "https://quickchart.io/chart?&encoding=base64&c=SmFjZWs=")
})

test_that("output type of inputDataToNamedList", {
  inputData <- data.frame("x" = 1:10, "y" = 1:10, "r" = 1:10)
  expect_type(inputDataToNamedList(inputData), "list")
})


test_check("quickchartR")
