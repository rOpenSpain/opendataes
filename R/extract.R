#' Extract data from a data_list.
#'
#' @param data_list A data_list similar to resp$result$items[[1]] that contains information on a dataset
#' @param encoding A valid encoding string passed to \code{\link[readr]{read_csv}}
#' @param ... Arguments passed to \code{\link[readr]{read_csv}} and all other \code{read_*} functions.
#'
#' @details \code{get_data} will accept the end path of a data base and it will search for the access url.
#' If the dataset is either a csv, xls, xlsx or xml, then it will
#' attempt to read it. If it succeeds, it will return the data frame. If not, it will return
#' the data frame with only one column containing all available access URL's.
#'
#' For example, this URL: http://datos.gob.es/es/catalogo/a02002834-numero-de-centros-segun-ancho-de-banda-de-la-conexion-a-internet-que-tiene-el-centro6
#' says that it has a XML file but once you click on the 'download' XML, it redirects to a JavaScript based
#' website that has the table. This file unfortunately is unreadable to the package.
extract_data <- function(data_list, encoding, ...) {

  # Check if the data_list is readable
  is_file_readable <- determine_dataset_url(data_list)

  if (length(is_file_readable) != 0) {
    # Grab the dataset names from the actual names from datos.gob.es
    names_datasets <- determine_dataset_name(data_list)

    if (!all(names_datasets == is_file_readable)) stop('Data cannot be read because it is not in correct order')

    output_data <- vector("list", length(is_file_readable))

    # Loop over each chosen file
    for (index in seq_along(is_file_readable)) {

      data_url <- names(is_file_readable)[index]
      # Determine the delimited of the file
      read_generic <- determine_read_generic(data_url)

      custom_locale <- readr::locale("es", encoding = encoding)

      # Try reading the data and saving it in **that** data frame's slot
      output_data[[index]] <-
        try(read_generic(file = data_url,
                         locale = custom_locale,
                         ...),
            silent = TRUE)

      # If the data that was read raised an error, return
      # the format and URL as we would have done if there are no
      # files to read at all from this data_list
      if (inherits(output_data[[index]], "try-error")) {
        output_data[[index]] <-
          tibble::tibble(format = is_file_readable[index],
                         URL = names(is_file_readable[index]))
      }
    }

    # Assign the the same name of ech dataset to each slot
    names(output_data) <- names(names_datasets)

  } else {
    # If no file to read, return all urls and formats
    output_data <- list(return_metadata(data_list))
    names(output_data) <- 'unavailable_formats'
  }


  output_data
}


return_metadata <- function(data_list) {
  # If there's any error, this means that none of the formats
  # could be read. So we return the the tibble with the url
  # formats and the access urls
  output_data <- tibble::tibble(extract_dataset_name(data_list),
                                extract_url_format(data_list),
                                extract_access_url(data_list))

  names(output_data) <- c("name", "format", "URL")

  output_data
}

#' Extract all metadata from a data_list
#'
#' @param data_list A data_list similar to resp$result$items[[1]] that contains information on a dataset
extract_metadata <- function(data_list) {

  keywords <- extract_keywords(data_list)
  description <- extract_description(data_list)
  languages <- extract_language(data_list)
  url_path <- extract_url(data_list)
  url_formats <- extract_url_format(data_list)
  date_data <- extract_release_date(data_list)
  modified_data <- extract_modified_date(data_list)
  publisher <- extract_publisher_name(data_list)
  publisher_data_url <- extract_publisher_data_url(data_list)


  # Title, description, languages and url_path
  # are vector of the same length as the number of
  # languages. We construct them with N rows
  # and then add the url_path (which is
  # the path to the info of the dataset
  # and no the accessURL). That is, the data frame
  # is in long format

  first_df <-
    tibble::tibble(
      language = languages,
      description = description
    )

  first_df$url <- url_path

  # Do not change this to tibble because
  # first_df is a df and it will throw an error.
  # data.frame in this case allows to pass a data frrame
  # and bins the columns together
  data_df <-
    data.frame(
      keywords = keywords,
      first_df,
      date_issued = date_data,
      date_modified = modified_data,
      publisher = publisher,
      publisher_data_url = publisher_data_url,
      stringsAsFactors = FALSE
    )

  final_df <- tibble::as_tibble(data_df)
  final_df
}

#' Extract keywords from data_list
#'
#' @inheritParams extract_metadata
extract_keywords <- function(data_list) {

  if (!'keyword' %in% names(data_list)) {
    return("No keywords available")
  }

  keywords <- paste0(unlist(data_list$keyword), collapse = "; ")
  keywords
}

extract_publisher_data_url <- function(data_list) {

  if (!'identifier' %in% names(data_list)) {
    return("No identifier available")
  }

  data_list$identifier
}

#' Extract description from data_list
#'
#' @inheritParams extract_metadata
extract_description <- function(data_list) {

  if (!'_value' %in% names(unlist(data_list$description))) {
    return("No description available")
  }

  desc <- unlist(data_list$description)
  descriptions <- unname(desc[names(desc) == "_value"])

  descriptions
}

#' Extract URL from datos.gob.es from data_list
#'
#' @inheritParams extract_metadata
extract_url <- function(data_list) {

  if (!'_about' %in% names(data_list)) {
    return("No URL available")
  }

  info_url <- data_list$`_about`
  info_url
}

#' Extract access URL to the actual data from data_list
#'
#' @inheritParams extract_metadata
extract_access_url <- function(data_list) {

  if (!'accessURL' %in% names(unlist(data_list$distribution))) {
    return("No access URL available")
  }

  if (is.null(getElement(data_list$distribution, "accessURL"))) {
    access_url <- vapply(data_list$distribution, function(x) x$accessURL,
                         FUN.VALUE = character(1))
  } else {
    access_url <- data_list$distribution$accessURL
  }

  distr <- unlist(data_list['distribution'])

  access_url <- distr[names(distr) == 'distribution.accessURL']

  access_url
}

#' Extract the format of the dataset from data_list
#'
#' For example, csv or xml
#' @inheritParams extract_metadata
extract_url_format <- function(data_list) {

  if (!'format.value' %in% names(unlist(data_list$distribution))) {
    return("No format available")
  }

  distr <- unlist(data_list['distribution'])
  raw_formats <- unname(distr[names(distr) == 'distribution.format.value'])
  # mimemap is a vector in R/utils.R containing all formats. For more info
  # go to utils.R
  correct_formats <- names(mimemap)[match(raw_formats, mimemap, nomatch = numeric())]

  correct_formats
}

extract_dataset_name <- function(data_list) {
  # There are as many dataset names as there are languages for
  # the dataset. In practice, they're always the same but I
  # search for the word title to just check that at least one is there.
  if (!any(grepl("title", names(unlist(data_list$distribution))))) {
    return("No dataset name available")
  }

  # The name of the data, in principle, is according to the language.
  # That is, if there's english, catalan and spanish, there will be
  # three dataset names such as votos_politica.csv, votos_politica.csv, etc..
  # In practice, no one names their datasets differently per language
  # But to avoid creating a complex chain of which names to pick, I
  # always pick the first language, assuming that there's at least one
  # because the previous check makes sure there is at least one

  if ("title" %in% names(data_list[['distribution']])) {
    distribution <- data_list['distribution']
  }  else {
    distribution <- data_list[['distribution']]
  }

  data_set_names <-
    vapply(distribution,
           function(x) x$title[[1]], FUN.VALUE = character(1))

  data_set_names
}

#' Extract access languages available from data_list
#'
#' @inheritParams extract_metadata
extract_language <- function(data_list) {

  if (!'_lang' %in% names(unlist(data_list$description))) {
    return("No language available")
  }

  desc <- unlist(data_list$description)

  languages <- unname(desc[names(desc) == "_lang"])

  languages
}

#' Extract the date at which the data was submitted from data_list
#'
#' The date is currently exported as a string but
#' should be turned into a a date class
#' @inheritParams extract_metadata
extract_release_date <- function(data_list) {

  if (!'issued' %in% names(data_list)) {
    return("No release date available")
  }

  issued <- as.POSIXct(substr(data_list$issued, 6, 25),
                       format = "%d %b %Y %H:%M:%S", tz = "GMT")

  # For now, but this should be converted
  # to date time
  issued
}

#' Extract the date at which the data was modified from data_list
#'
#' The date is currently exported as a string but
#' should be turned into a a date class
#' @inheritParams extract_metadata
extract_modified_date <- function(data_list) {

  if (!'modified' %in% names(data_list)) {
    return("No modification date available")
  }

  modified <- as.POSIXct(substr(data_list$modified, 6, 25),
                         format = "%d %b %Y %H:%M:%S", tz = "GMT")

  # For now, but this should be converted
  # to date time
  modified
}

#' Extract the publisher code of the dataset from data_list
#'
#' @inheritParams extract_metadata
extract_publisher_code <- function(data_list) {

  if (!'publisher' %in% names(data_list)) {
    return("No publisher available")
  }

  publisher_code <- sub(".*\\/", "", data_list$publisher)

  publisher_code
}

#' Extract the publisher name of the dataset from data_list
#'
#' @inheritParams extract_metadata
extract_publisher_name <- function(data_list) {

  if (!'publisher' %in% names(data_list)) {
    return("No publisher available")
  }

  publisher_code <- sub(".*\\/", "", data_list$publisher)
  publisher_name <- translate_publisher(publisher_code)

  publisher_name
}


#' Extract the end path of the dataset that directs to datos.gob.es from a data_list
#'
#' @inheritParams extract_metadata
extract_endpath <- function(data_list) {

  if (!'_about' %in% names(data_list)) {
    return("No link to the data in datos.gob.es")
  }

  end_path <- sub(".*\\/", "", data_list[["_about"]])

  end_path
}



#' Check data_list is in correct formats
#'
#' When new checks come up, add them in
#' the same format: logical tests first
#' and then add them to the if statement
#'
#' @param raw_json Raw JSON response from datos.gob.es
data_list_correct <- function(raw_json) {

  no_items <- !"items" %in% names(raw_json$result)
  no_datasets <- length(raw_json$result$items) == 0

  if (no_items || no_datasets) return(FALSE)

  data_list <- raw_json$result$items[[1]]

  wrong_length <- length(data_list) == 0
  no_names <- is.null(attr(data_list, 'names'))
  # Because the URL slot is in the distribution
  # and all data_lists must follow the same structure
  no_distribution_slot <- !'distribution' %in% names(data_list)
  no_description_slot <- !'description' %in% names(data_list)

  failure_tests <- c(
    wrong_length,
    no_names,
    no_distribution_slot,
    no_description_slot
  )

  if (any(failure_tests)) {
    return(FALSE)
  }

  TRUE
}

# Some of the extract_components return vectors of length > 1
# that refers to many languages. Like the description
# comes in three langauges. We want those vectors to be turned
# into columns with the language prefix in the column names
vector_to_df_columnwise <- function(vec, column_names) {
  semi_df <- tibble::as_tibble(matrix(vec, ncol = length(vec)))
  names(semi_df) <- column_names
  semi_df
}
