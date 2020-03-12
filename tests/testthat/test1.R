library(week4project)
library(testthat)

test_that("make_filename", {
  year<-2020
  expect_that(make_filename(year), is_a("character"))
})
