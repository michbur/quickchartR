library(testthat)
library(quickchartR)

test_that("output type of getLabels", {
  labels <- 1:10
  expect_type(getLabels(labels), "character")
})

test_that("unique test of getLabels", {
  labels <- c(1,2,3,3,3,4,5,5)
  expect_equal(length(getLabels(labels)), 5)
})

test_that("output type of inputDataToNamedList", {
  inputData <- data.frame("x" = 1:10, "y" = 1:10, "r" = 1:10)
  expect_type(inputDataToNamedList(inputData), "list")
})


test_check("quickchartR")
