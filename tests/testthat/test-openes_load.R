context("test-openesload.R")

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


test_that("openes_load for ONE dataset returns correct format", {
  skip_on_cran()

  example_id <- 'l01080193-tasa-especifica-de-fecundidad-en-madres-de-15-a-19-anos-de-la-ciudad-de-barcelona1'
  res <- openes_load(example_id)

  standard_check(res)
})

test_that("openes_load for SEVERAL dataset returns correct format", {
  skip_on_cran()

  example_id <- 'l01080193-domicilios-de-la-ciudad-de-barcelona-segun-la-nacionalidad-de-los-residentes'
  res <- openes_load(example_id)

  standard_check(res)
})

# test_that("openes_load can read dates successfully while changing locale", {
#   skip_on_cran()
#
#   # This dataset has a date of release in the month of 'dic' in Spanish.
#   # Here we test that `extract_modified_date` and `extract_issued_date`
#   # is not missing inside STANDARD CHECK. This test was included
#   # because we were not changing the locale to Spanish before
#   # and months such as ene (enero) or dic (diciembre) were throwing NA's.
#   res <- openes_load('l01080193-carta-arqueologica-de-barcelona')
#
#   standard_check(res)
# })

test_that("openes_load for inexistent dataset returns empty list", {
  example_id <- 'random_data'
  res <- openes_load(example_id)

  # Check classes
  expect_is(res, "list")
  expect_length(res, 0)
})

test_that("openes_load for more than one end path", {
  example_id <- c('l01080193-domicilios-de-la-ciudad-de-barcelona-segun-la-nacionalidad-de-los-residentes',
                  'l01080193-tasa-especifica-de-fecundidad-en-madres-de-15-a-19-anos-de-la-ciudad-de-barcelona1')

  expect_error(openes_load(example_id),
               "`x` must be a character of length 1", fixed = TRUE)
  expect_error(openes_load.character(list(example_id)),
               "`x` must be a character of length 1", fixed = TRUE)
})

test_that("Checks that encoding in openes_load is in correct format", {
  example_id <- c('l01080193-domicilios-de-la-ciudad-de-barcelona-segun-la-nacionalidad-de-los-residentes')
  encoding <- c("UTF-8", "latin1")


  expect_error(openes_load(example_id, encoding = encoding),
               "`encoding` must be a character of length 1", fixed = TRUE)

  expect_error(openes_load(example_id, encoding = list(encoding)),
               "`encoding` must be a character of length 1", fixed = TRUE)
})

test_that("Checks that guess_encoding in openes_load is in correct format", {
  example_id <- c('l01080193-domicilios-de-la-ciudad-de-barcelona-segun-la-nacionalidad-de-los-residentes')

  expect_error(openes_load(example_id, guess_encoding = c(TRUE, FALSE)),
               "`guess_encoding` must be a logical of length 1", fixed = TRUE)

  expect_error(openes_load(example_id, guess_encoding = "TRUE"),
               "`guess_encoding` must be a logical of length 1", fixed = TRUE)
})

test_that("openes_load for more than one end path", {
  example_id <- c('l01080193-domicilios-de-la-ciudad-de-barcelona-segun-la-nacionalidad-de-los-residentes',
                  'l01080193-tasa-especifica-de-fecundidad-en-madres-de-15-a-19-anos-de-la-ciudad-de-barcelona1')

  expect_error(openes_load(example_id), "`x` must be a character of length 1", fixed = TRUE)
})

test_that("openes_load doesn't read if it's not a character of length 1", {

  # Factors
  example_id <- factor('l01080193-tasa-especifica-de-fecundidad-en-madres-de-15-a-19-anos-de-la-ciudad-de-barcelona1')
  expect_error(openes_load.character(example_id), "`x` must be a character of length 1")

  # Numerics
  expect_error(openes_load.character(1), "`x` must be a character of length 1")


  example_id <- list('l01080193-domicilios-de-la-ciudad-de-barcelona-segun-la-nacionalidad-de-los-residentes')
  expect_error(openes_load.character(example_id), "`x` must be a character of length 1")
})

# Run this when Madrid is included as publisher
test_that("openes_load assigns 'Distribucion sin nombre' when there is no name", {
  skip_on_cran()
  # This dataset has (or had) one of the files with a title and all other without titles
  id <- "l01280796-centros-de-servicios-sociales-municipales"
  pt <- openes_load(id)
  expect_true("Distribucion sin nombre" %in% names(pt$data))

  # This dataset has (or had) ALL of the files without titles. I repeat the same to test that both are assigned
  # a distribucion sin nombre
  #id <- "l01280796-trafico-semaforos-con-avisadores-acusticos"
  #pt <- openes_load(id)
  #expect_true("Distribucion sin nombre" %in% names(pt$data))

  # This dataset has only ONE file and it has no names
  pt <- openes_load('l01280796-perfiles-profesionales-de-concejales-directivos-eventuales-funcionarios-de-nivel-28-o-superior-y-vocales-vecinos-del-ayuntamiento-de-madrid1')
  expect_true("Distribucion sin nombre" %in% names(pt$data))
})

test_that("openes_load works fine when guess_encoding is FALSE", {
  skip_on_cran()

  tst <- openes_load('l01080193-resultados-absolutos-de-las-elecciones-al-parlamento-europeo-de-la-ciudad-de-barcelona',
                      encoding = 'latin1',
                      guess_encoding = FALSE)

  standard_check(tst)
})


################
# We should add a test that can confirm that when guess_encoding
# is false, then the encoding argument is used. I cannot think of a proper
# test because even if a CSV files is in, let's say, latin1, whenever a data
# frame is read we cannot check what the initial encoding was. If we check
# the encoding of the dataframe it would be normalized

# For example...

# Supose the file below is latin1 encoded
# latin1_encoded <- "./mtcars.csv"

# Supose I want to read it with the encoding ASCII and not GUESS the encoding
# res <- openes_load(latin1_encoded, encoding = "ASCII", guess_encoding = FALSE)

# If I would've set guess_encoding to TRUE, the guessed encoding would have been latin1
# However, because the encoding argument overrides guess_encoding, how can I test whether
# the resulting dataframe is indeed 'ASCII' encoded and not 'latin1' encoded?
################


# For some reason, this crashes travis, which I don't know why.
# test_that("openes_load uses encoding and readr arguments to read only 5 rows", {
#   skip_on_cran()
#
#   tst <-
#     openes_load('l01080193-elecciones-al-parlamento-europeo-resultados-absolutos-por-seccion-censal-de-la-ciudad-de-barcelona',
#                  n_max = 5)
#
#   expect_true(all(vapply(tst$data, nrow, FUN.VALUE = numeric(1)) == 5))
# })

test_that("openes_load returns links when format is not readable", {
  skip_on_cran()

  not_readable <- openes_load("l01280796-bicimad-datos-de-la-situacion-de-estaciones-bicimad-por-dia-y-hora1")
  standard_check(not_readable)
})

test_that("openes_load fails for publisher not permitted", {
  skip_on_cran()

  expect_error(openes_load("ea0008369-guias-para-la-gestion-publica-de-la-diversidad-religiosa-observatorio-del-pluralismo-religioso-en-espana"),
               "Publisher not available. Please check publishers_available() to get the available ones.",
                fixed = TRUE)
})

test_that("openes_load returns tibbles with URL's when it cannot read the file", {
  skip_on_cran()

  id <- 'l01080193-descripcion-de-la-causalidad-de-los-accidentes-gestionados-por-la-guardia-urbana-en-la-ciudad-de-barcelona'
  pl <- openes_load(id)

  standard_check(pl)
})

# No need to check for the keyword's format and content because if this passes
# it means it has the same structure as the character tests
test_that("openes_load's character and keyword results match exactly", {
  skip_on_cran()

  character_method <- openes_load('l01080193-resultados-absolutos-de-las-elecciones-al-parlamento-europeo-de-la-ciudad-de-barcelona',
                                  'latin1',
                                  n_max = 5)

  kw <- openes_keywords("elecciones", "l01080193")

  intm <- kw[grepl("Resultados absolutos de las elecciones al Parlamento Europeo de la ciudad de Barcelona",
                   kw$description,
                   fixed = TRUE), ]

  keyword_method <- openes_load.datos_gob_es_keywords(intm, 'latin1', n_max = 5)

  expect_identical(character_method, keyword_method)
})

test_that("openes_load throws errors when the keyword data frame is not in expected format", {
  skip_on_cran()

  cp <- openes_keywords("renta", "l01080193")

  expect_error(openes_load(cp),
               "The data frame resulted from openes_keywords must have only 1 dataset (1 row). Make sure you filter down to only one dataset",
               fixed = TRUE)

  expect_error(openes_load(data.frame()),
               "The data frame resulted from openes_keywords must have only 1 dataset (1 row). Make sure you filter down to only one dataset",
               fixed = TRUE)


  cp <- cp[grepl("Distribución territorial de la renta familiar en la ciudad de Barcelona.", cp$description, fixed = TRUE), ]
  cp$path_id <- factor(cp$path_id)

  expect_error(openes_load(cp),
               "Column `path_id` from the keywords data frame must be a character vector",
               fixed = TRUE)

  kw <- openes_keywords("Barcino", "l01080193")
  expect_error(openes_load(kw),
               "The chosen dataset from the keywords data frame is not readable")

  colnames(cp)[1] <- 'test'
  expect_error(openes_load(cp),
               "The keywords data frame must contain and have this order of columns: description, publisher, is_readable, path_id, url",
               fixed = TRUE)
})

# This test is implemented since we found that reading the same file between Mac and Windows,
# in Windows the files were read but in Mac they weren't. We found this was due to an encoding
# problem and we fixed it. This test reads a dataset that previously was read in Windows
# but not in Mac. This way, when we run this in Appveyor and Travis it should be true in both
test_that("Check that elections dataset is correctly read", {
  skip_on_cran()
  pt <- openes_load('l01080193-resultados-absolutos-de-las-elecciones-al-parlamento-europeo-de-la-ciudad-de-barcelona')
  files_read <- determine_number(pt)

  expect_gte(files_read, 2)
})
