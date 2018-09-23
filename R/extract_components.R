
## Two examples to test the functions below

## EX 1
# id <- 'l01080193-numero-total-de-edificios-con-viviendas-segun-numero-de-plantas'
# resp <- get_resp(path_dataset_id(id))
# data_list <- resp$result$items[[1]]

## EX 2
# resp <- get_resp(path_datasets())
# data_list <- resp$result$items[[2]]



#' Extract all metadata from a data_list
#'
#' @param data_list A data_list similar to resp$result$items[[1]] that contains information on a dataset
extract_metadata <- function(data_list) {

  keywords <- extract_keywords(data_list)
  title <- extract_title(data_list)
  description <- extract_description(data_list)
  languages <- extract_language(data_list)
  url_path <- extract_url(data_list)
  url_formats <- extract_url_format(data_list)
  date_data <- extract_date(data_list)
  publisher <- extract_publisher(data_list)


  # Title, description, languages and url_path
  # are vector of the same length as the number of
  # languages. We construct them with N rows
  # and then add the url_path (which is
  # the path to the info of the dataset
  # and no the accessURL). That is, the data frame
  # is in long format

  first_df <-
    dplyr::tibble(
      language = languages,
      title = title,
      description = description,
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
      date = date_data,
      publisher = publisher,
      stringsAsFactors = FALSE
    )

  final_df <- dplyr::as_tibble(data_df)
  final_df
}

#' Extract keywords from data_list
#'
#' @inheritParams extract_metadata
extract_keywords <- function(data_list) {

  if (!data_list_correct(data_list)) {
    return(character())
  }

  if (!'keyword' %in% names(data_list)) {
    "No keywords available"
  }

  keywords <- paste0(unlist(data_list$keyword), collapse = "; ")
  keywords
}

#' Extract title from data_list
#'
#' @inheritParams extract_metadata
extract_title <- function(data_list) {

  if (!data_list_correct(data_list)) {
    return(character())
  }

  if (!'title' %in% names(data_list)) {
    "No title available"
  }

  title <- unlist(data_list$title)
  title
}

#' Extract description from data_list
#'
#' @inheritParams extract_metadata
extract_description <- function(data_list) {
  if (!data_list_correct(data_list)) {
    return(character())
  }

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

  if (!data_list_correct(data_list)) {
    return(character())
  }

  if (!'_about' %in% names(data_list)) {
    "No URL available"
  }

  info_url <- data_list$`_about`
  info_url
}

#' Extract access URL to the actual data from data_list
#'
#' @inheritParams extract_metadata
extract_access_url <- function(data_list) {
  if (!data_list_correct(data_list)) {
    return(character())
  }

  if (!'accessURL' %in% names(unlist(data_list$distribution))) {
    "No URL available"
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

  if (!data_list_correct(data_list)) {
    return(character())
  }

  if (!'format.value' %in% names(unlist(data_list$distribution))) {
    "No format available"
  }

  distr <- unlist(data_list['distribution'])
  raw_formats <- unname(distr[names(distr) == 'distribution.format.value'])
  # mimemap is a vector in R/utils.R containing all formats. For more info
  # go to utils.R
  correct_formats <- names(mimemap)[match(raw_formats, mimemap, nomatch = numeric())]

  correct_formats
}

#' Extract access languages available from data_list
#'
#' @inheritParams extract_metadata
extract_language <- function(data_list) {
  if (!data_list_correct(data_list)) {
    return(character())
  }

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
extract_date <- function(data_list) {
  if (!data_list_correct(data_list)) {
    return(character())
  }

  if (!'issued' %in% names(data_list)) {
    return("No date available")
  }

  # For now, but this should be converted
  # to date time
  data_list$issued
}

#' Extract the publisher code of the dataset from data_list
#'
#' @inheritParams extract_metadata
extract_publisher_code <- function(data_list) {
  if (!data_list_correct(data_list)) {
    return(character())
  }

  if (!'publisher' %in% names(data_list)) {
    "No publisher available"
  }

  publisher_code <- sub(".*\\/", "", data_list$publisher)

  publisher_code
}

#' Extract the publisher name of the dataset from data_list
#'
#' @inheritParams extract_metadata
extract_publisher_name <- function(data_list) {
  if (!data_list_correct(data_list)) {
    return(character())
  }

  if (!'publisher' %in% names(data_list)) {
    "No publisher available"
  }

  publisher_code <- sub(".*\\/", "", data_list$publisher)
  publisher_name <- translate_publisher(publisher_code)

  publisher_name
}


#' Extract the end path of the dataset that directs to datos.gob.es from a data_list
#'
#' @inheritParams extract_metadata
extract_endpath <- function(data_list) {
  if (!data_list_correct(data_list)) {
    return(character())
  }

  if (!'_about' %in% names(data_list)) {
    "No link to the data in datos.gob.es"
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
#' @inheritParams extract_metadata
data_list_correct <- function(data_list) {
  wrong_length <- length(data_list) == 0
  no_names <- is.null(attr(data_list, 'names'))

  # Because the URL slot is in the distribution
  # and all data_lists must follow the same structure
  no_distribution_slot <- !'distribution' %in% names(data_list)
  no_description_slot <- !'description' %in% names(data_list)


  if (wrong_length | no_names | no_distribution_slot | no_description_slot) {
    return(FALSE)
  }

  TRUE
}

# Some of the extract_components return vectors of length > 1
# that refers to many languages. Like the description
# comes in three langauges. We want those vectors to be turned
# into columns with the language prefix in the column names
vector_to_df_columnwise <- function(vec, column_names) {
  semi_df <- dplyr::as_tibble(matrix(vec, ncol = length(vec)))
  names(semi_df) <- column_names
  semi_df
}
