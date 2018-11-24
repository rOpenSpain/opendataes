context("test-cargardatos.R")

standard_check <- function(res) {
  # Check classes
  expect_s3_class(res, "datos_gob_es")
  expect_is(unclass(res), "list")
  expect_is(res$metadata, "data.frame")
  expect_true(all(!is.na(res$metadata$date_issued))) # all dates should not be missing
  expect_true(all(!is.na(res$metadata$date_modified)))
  expect_true(all(vapply(res$data, is.data.frame, FUN.VALUE = logical(1))))

  # Check structure
  expect_named(unclass(res))
  expect_named(res$data)
  expect_length(res, 2)
  expect_true(all(vapply(res$data,
                         function(x) nrow(x) >= 1, FUN.VALUE = logical(1))))
  expect_gte(nrow(res$metadata), 1)
}


determine_number <- function(x) {
  check_read <- function(data) !all(names(data) %in% c('name', 'format', "URL"))

  has_url_col <- vapply(x[[2]], check_read, logical(1))
  number_of_reads  <- sum(has_url_col)


  number_of_reads
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

test_that("cargar_datos can read dates successfully while changing locale", {

  # This dataset has a date of release in the month of 'dic' in Spanish.
  # Here we test that `extract_modified_date` and `extract_issued_date`
  # is not missing inside STANDARD CHECK. This test was included
  # because we were not changing the locale to Spanish before
  # and months such as ene (enero) or dic (diciembre) were throwing NA's.
  res <- cargar_datos('l01080193-carta-arqueologica-de-barcelona')

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

  expect_error(cargar_datos(example_id),
               "`x` must be a character of length 1", fixed = TRUE)
  expect_error(cargar_datos.character(list(example_id)),
               "`x` must be a character of length 1", fixed = TRUE)
})

test_that("Checks that encoding in cargar_datos is in correct format", {
  example_id <- c('l01080193-domicilios-segun-nacionalidad')
  encoding <- c("UTF-8", "latin1")


  expect_error(cargar_datos(example_id, encoding = encoding),
               "`encoding` must be a character of length 1", fixed = TRUE)

  expect_error(cargar_datos(example_id, encoding = list(encoding)),
               "`encoding` must be a character of length 1", fixed = TRUE)
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


# For some reason, this crashes travis, which I don't know why.
# test_that("cargar_datos uses encoding and readr arguments to read only 5 rows", {
#   skip_on_cran()
#
#   tst <-
#     cargar_datos('l01080193-elecciones-al-parlamento-europeo-sobre-electores',
#                  'latin1',
#                  n_max = 5)
#
#   expect_true(all(vapply(tst$data, nrow, FUN.VALUE = numeric(1)) == 5))
# })



test_that("cargar_datos returns links when format is not readable", {
  skip_on_cran()

  not_readable <- cargar_datos("l01080193-estaciones-de-bicing-mecanicas-y-electricas")
  standard_check(not_readable)
})

test_that("cargar_datos fails for publisher not permitted", {
  skip_on_cran()

  expect_error(cargar_datos("ea0008369-guias-para-la-gestion-publica-de-la-diversidad-religiosa-observatorio-del-pluralismo-religioso-en-espana"),
               "Publisher not available. Please check publishers_available() to get the available ones.",
                fixed = TRUE)
})

test_that("cargar_datos returns tibbles with URL's when it cannot read the file", {
  skip_on_cran()

  id <- 'l01080193-descripcion-de-la-causalidad-de-los-accidentes-gestionados-por-la-guardia-urbana-en-la-ciudad-de-barcelona'
  pl <- cargar_datos(id)

  standard_check(pl)
})


# No need to check for the keyword's format and content because if this passes
# it means it has the same structure as the character tests
test_that("cargar_datos's character and keyword results match exactly", {
  skip_on_cran()

  character_method <- cargar_datos('l01080193-elecciones-al-parlamento-europeo-sobre-electores', 'latin1', n_max = 5)

  kw <- explorar_keywords("la Prosperitat", "l01080193")

  intm <- kw[grepl("Elecciones al Parlamento Europeo. % sobre electores", kw$description), ]
  keyword_method <- cargar_datos.datos_gob_es_keywords(intm, 'latin1', n_max = 5)

  expect_identical(character_method, keyword_method)
})



test_that("cargar_datos throws errors when the keyword data frame is not in expected format", {
  skip_on_cran()

  cp <- explorar_keywords("la Prosperitat", "l01080193")

  expect_error(cargar_datos(cp),
               "The data frame resulted from explorar_keywords must have only 1 dataset (1 row). Make sure you filter down to only one dataset",
               fixed = TRUE)

  expect_error(cargar_datos(data.frame()),
               "The data frame resulted from explorar_keywords must have only 1 dataset (1 row). Make sure you filter down to only one dataset",
               fixed = TRUE)


  cp <- cp[grepl("Elecciones al Parlamento Europeo. % sobre electores", cp$description), ]
  cp$path_id <- factor(cp$path_id)

  expect_error(cargar_datos(cp),
               "Column `path_id` from the keywords data frame must be a character vector",
               fixed = TRUE)

  kw <- explorar_keywords("Barcino", "l01080193")
  expect_error(cargar_datos(kw),
               "The chosen dataset from the keywords data frame is not readable")

  colnames(cp)[1] <- 'test'
  expect_error(cargar_datos(cp),
               "The keywords data frame must contain and have this order of columns: description, publisher, is_readable, path_id, url",
               fixed = TRUE)
})

# This test is implemented since we found some troubles when reading some datasets on different OS. Therefore,
# since this test will run by Travis we ensure we have the same results in different OS
test_that("Check that elections dataset is correctly read", {
  skip_on_cran()
  pt <- cargar_datos('l01080193-elecciones-al-parlamento-europeo-sobre-electores')
  files_read <- determine_number(pt)

  expect_gte(files_read, 2)

})

