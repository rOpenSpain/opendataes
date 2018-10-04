context("test-cargardatos.R")

test_that("cargar_datos for ONE dataset returns correct format", {
  skip_on_cran()

  example_id <- 'l01080193-fecundidad-madres-de-15-a-19-anos-quinquenal-2003-2014'
  res <- cargar_datos(example_id)

  # Check classes
  expect_s3_class(res, "datos_gob_es")
  expect_is(unclass(res), "list")
  expect_is(res$metadata, "data.frame")
  expect_true(all(vapply(res$data, is.data.frame, FUN.VALUE = logical(1))))

  # Check structure
  expect_named(unclass(res))
  expect_named(res$data)
  expect_length(res, 2)
  expect_true(all(vapply(res$data,
                         function(x) nrow(x) >= 1, FUN.VALUE = logical(1))))
  expect_gte(nrow(res$metadata), 1)
})

test_that("cargar_datos for SEVERAL dataset returns correct format", {
  skip_on_cran()

  example_id <- 'l01080193-domicilios-segun-nacionalidad'
  res <- cargar_datos(example_id)

  # Check classes
  expect_s3_class(res, "datos_gob_es")
  expect_is(unclass(res), "list")
  expect_is(res$metadata, "data.frame")
  expect_true(all(vapply(res$data, is.data.frame, FUN.VALUE = logical(1))))

  # Check structure
  expect_named(unclass(res))
  expect_named(res$data)
  expect_length(res, 2)
  expect_true(all(vapply(res$data,
                         function(x) nrow(x) >= 1, FUN.VALUE = logical(1))))
  expect_gte(nrow(res$metadata), 1)
})


test_that("cargar_datos for inexistent dataset returns empty list", {
  example_id <- 'random_data'
  res <- cargar_datos(example_id)

  # Check classes
  expect_is(res, "list")
  expect_length(res, 0)
})
