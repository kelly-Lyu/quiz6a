#### Preamble ####
# Purpose: Simulation using testthat
# Author: Kelly Lyu
# Date: 13 February 2024
# Contact: kelly.lyu@mail.utoronto.ca
# Pre-requisites: None

library(testthat)

german_cities <- c("Berlin", "Hamburg", "Munich", "Cologne", "Frankfurt", "Rostock")

# Test 1: Correct Spelling
test_that("german_cities are spelled correctly", {
  correctly_spelled_cities <- c("Berlin", "Hamburg", "Munich", "Cologne", "Frankfurt", "Rostock")
  expect_equal(german_cities, correctly_spelled_cities)
})

# Test 2: No Empty Strings
test_that("german_cities does not contain empty strings", {
  expect_false(any(german_cities == ""))
})

# Test 3: Data Type
test_that("german_cities is a character vector", {
  expect_type(german_cities, "character")
})
