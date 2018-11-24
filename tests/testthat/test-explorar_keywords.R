context("test-explorar_keywords")

check_format <- function(kw) {
  expect_is(kw, "tbl_df")
  expect_equal(ncol(kw), 5)
  expect_equal(colnames(kw), c("description", "publisher", "is_readable", "path_id", "url"))
  expect_gt(nrow(kw), 1)
  expect_true(all(!duplicated(kw$description)))
}

test_that("explorar_keywords returns correct format", {

  skip_on_cran()

  kw <- explorar_keywords("vivienda", "l01080193") # Ayuntamiento de Barcelona

  check_format(kw)

})

test_that("explorar_keywords returns error when wrong publisher", {

  skip_on_cran()

  expect_error(explorar_keywords("w", as.character(1e9)),
               "Publisher `1e+09` not available. Please check publishers_available() to get the available ones.",
               fixed = TRUE)
})

test_that("explorar_keywords returns correct format when publisher is upper case", {

  skip_on_cran()

  kw <- explorar_keywords("vivienda", "L01080193") # Ayuntamiento de Barcelona
  check_format(kw)
})

test_that("explorar_keywords errors when inputs are not of correct length or format", {

  skip_on_cran()

  expect_error(explorar_keywords(c("what", "ever"), "l01080193"))
  expect_error(explorar_keywords(c("what", "ever"), c("l01080193", "whatever")))
  expect_error(explorar_keywords(factor("vivienda"), "l01080193"))
  expect_error(explorar_keywords("vivienda", factor("l01080193")))
  expect_error(explorar_keywords(factor("vivienda"), factor("l01080193")))

  expect_error(explorar_keywords('vivienda', ""),
               "Publisher `` not available. Please check publishers_available() to get the available ones.",
               fixed = TRUE)

  # If there's no keyword, then the request is not found
  expect_error(explorar_keywords('', 'l01080193'), 'Not Found (HTTP 404).', fixed = TRUE)
})
