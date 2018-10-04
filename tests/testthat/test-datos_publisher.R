context("test-datos_publisher.R")

test_that("datos_publisher returns correct format", {
  publisher <- datos_publisher()

  expect_is(publisher, "data.frame")
  expect_equal(ncol(publisher), 2)
  expect_gte(nrow(publishers), 50)
  expect_true(all(vapply(publisher, is.character, FUN.VALUE = logical(1))))
})


test_that("publishers_available returns correct format", {
  publisher <- publishers_available

  expect_is(publisher, "data.frame")
  expect_equal(ncol(publisher), 2)
  expect_true(all(vapply(publisher, is.character, FUN.VALUE = logical(1))))
})
