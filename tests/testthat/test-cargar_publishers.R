context("test-cargar_publishers.R")

test_that("cargar_publishers returns correct format", {
  publisher <- cargar_publishers()

  expect_is(publisher, "data.frame")
  expect_equal(ncol(publisher), 2)
  expect_gte(nrow(publisher), 50)
  expect_true(all(vapply(publisher, is.character, FUN.VALUE = logical(1))))
})


test_that("publishers_available returns correct format", {
  publisher <- publishers_available

  expect_is(publisher, "data.frame")
  expect_equal(ncol(publisher), 2)
  expect_true(all(vapply(publisher, is.character, FUN.VALUE = logical(1))))
})
