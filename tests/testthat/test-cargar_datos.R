context("test-cargardatos.R")

standard_check <- function(res) {
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
}

test_that("cargar_datos for ONE dataset returns correct format", {
  skip_on_cran()

  example_id <- 'l01080193-fecundidad-madres-de-15-a-19-anos-quinquenal-2003-2014'
  res <- cargar_datos(example_id)

  standard_check(res)
})

test_that("cargar_datos for SEVERAL dataset returns correct format", {
  skip_on_cran()

  example_id <- 'l01080193-domicilios-segun-nacionalidad'
  res <- cargar_datos(example_id)

  standard_check(res)
})


test_that("cargar_datos for inexistent dataset returns empty list", {
  example_id <- 'random_data'
  res <- cargar_datos(example_id)

  # Check classes
  expect_is(res, "list")
  expect_length(res, 0)
})

test_that("cargar_datos for more than one end path", {
  example_id <- c('l01080193-domicilios-segun-nacionalidad',
                  'l01080193-fecundidad-madres-de-15-a-19-anos-quinquenal-2003-2014')

  expect_error(cargar_datos(example_id), "`x` must be a character of length 1", fixed = TRUE)
})


test_that("cargar_datos doesn't read if it's not a character of length 1", {

  # Factors
  example_id <- factor('l01080193-fecundidad-madres-de-15-a-19-anos-quinquenal-2003-2014')
  expect_error(cargar_datos.character(example_id), "`x` must be a character of length 1")

  # Numerics
  expect_error(cargar_datos.character(1), "`x` must be a character of length 1")


  example_id <- list('l01080193-domicilios-segun-nacionalidad')
  expect_error(cargar_datos.character(example_id), "`x` must be a character of length 1")
})

test_that("cargar_datos uses encoding and readr arguments to read only 5 rows", {
  tst <-
    cargar_datos('l01080193-elecciones-al-parlamento-europeo-sobre-electores',
                 'latin1',
                 n_max = 5)

  expect_true(all(vapply(tst$data, nrow, FUN.VALUE = numeric(1)) == 5))
})


test_that("cargar_datos returns links when format is not readable", {
  skip_on_cran()

  not_readable <- cargar_datos("l01080193-estaciones-de-bicing-mecanicas-y-electricas")
  standard_check(not_readable)
})

# No need to check for the keyword's format and content because if this passes
# it means it has the same structure as the character tests
test_that("cargar_datos's character and keyword results match exactly", {
  character_method <- cargar_datos('l01080193-elecciones-al-parlamento-europeo-sobre-electores', 'latin1', n_max = 5)

  kw <- explorar_keywords("la Prosperitat", publishers_available$publisher_code)

  intm <- kw[grepl("Elecciones al Parlamento Europeo. % sobre electores", kw$description), ]
  keyword_method <- cargar_datos(intm, 'latin1', n_max = 5)

  expect_identical(character_method, keyword_method)
})

test_that("cargar_datos throws errors when the keyword data frame is not in expected formed", {
  kw <- explorar_keywords("la Prosperitat", publishers_available$publisher_code)
  cp <- kw

  expect_error(cargar_datos(kw),
               "The data frame resulted from explorar_keywords must have only 1 dataset (1 row). Make sure you filter down to only one dataset",
               fixed = TRUE)

  cp <- cp[grepl("Elecciones al Parlamento Europeo. % sobre electores", cp$description), ]
  cp$path_id <- factor(path_id)

  expect_error(cargar_datos(cp),
               "The data frame resulted from explorar_keywords must have only 1 dataset (1 row). Make sure you filter down to only one dataset",
               fixed = TRUE)


  expect_identical(character_method, keyword_method)
})

test_that("cargar_datos for empty df throws error", {
  kw <- explorar_keywords("la Prosperitat", publishers_available$publisher_code)

  intm <- kw[grepl("Elecciones al Parlamento Europeo. % sobre electores", kw$description), ]
  keyword_method <- cargar_datos(intm, 'latin1', n_max = 5)

  expect_identical(character_method, keyword_method)
})

