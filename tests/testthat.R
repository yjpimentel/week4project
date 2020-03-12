library(testthat)
library(week4project)

test_check("week4project")

test_that("make_filename", {
  year<-2020
  expect_that(make_filename(year), is_a("character"))
})
