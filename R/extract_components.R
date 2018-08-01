
## Checks data_list is correct
## As we find further requiremenrs that a data_list
## must have, we add it here
data_list_correct <- function(data_list) {
  wrong_length <- length(data_list) == 0
  no_names <- is.null(attr(data_list, 'names'))

  # Because the URL slot is in the distribution
  # and all data_lists must follow the same structure
  no_distribution_slot <- !'distribution' %in% names(data_list)
  no_description_slot <- !'description' %in% names(data_list)


  if (wrong_length | no_names | no_distribution_slot) {
    return(FALSE)
  }

  TRUE
}


### Here's a set of functions to extract elements from
### a slot with data information


## Extract elements from that data_list
## For now I think we should concentrate on
# Keywords
# Title
# Description
# Access URL
# Language
# Date
# Publisher


## Two examples to test the functions below

id <- 'l01080193-numero-total-de-edificios-con-viviendas-segun-numero-de-plantas'
resp <- content(get_resp(path_dataset_id(id)))
data_list <- resp$result$items[[1]]

# resp <- content(get_resp(path_datasets()))
# data_list <- resp$result$items[[1]]

##

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

extract_description <- function(data_list) {
  if (!data_list_correct(data_list)) {
    return(character())
  }

  if (!'_value' %in% names(unlist(data_list$description))) {
    return("No description available")
  }

  descriptions <- vapply(data_list$description, function(x) unlist(x)['_value'],
                         FUN.VALUE = character(1))
  descriptions
}

extract_url <- function(data_list) {
  if (!data_list_correct(data_list)) {
    return(character())
  }

  if (!'accessURL' %in% names(unlist(data_list$distribution))) {
    "No URL available"
  }

  access_url <- vapply(data_list$distribution, function(x) x$accessURL,
                       FUN.VALUE = character(1))
  access_url
}

extract_url_format <- function(data_list) {
  urls <- extract_url(data_list)
  sub('.*\\.', '', urls)
}

extract_language <- function(data_list) {
  if (!data_list_correct(data_list)) {
    return(character())
  }

  if (!'_lang' %in% names(unlist(data_list$description))) {
    return("No language available")
  }

  languages <- vapply(data_list$description, function(x) unlist(x)['_lang'],
                      FUN.VALUE = character(1))
  languages
}

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

# This is wrong on purpose. Still waiting
# to create the function to extract
# the publishers automatically
# to call this and match the publisher ID
# to the actual name.
extract_publisher <- function(data_list) {
  if (!data_list_correct(data_list)) {
    return(character())
  }

  if (!'publisher' %in% names(data_list)) {
    "No publisher available"
  }

  publisher <- data_list$publisher
  publisher
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


# If half_columns, returns only the necessary columns
build_df_datalist <- function(data_list, half_columns = TRUE) {

  keywords <- extract_keywords(data_list)
  title <- extract_title(data_list)
  description <- extract_description(data_list)
  languages <- extract_language(data_list)
  url_path <- extract_url(data_list)
  url_formats <- extract_url_format(data_list)
  date_data <- extract_date(data_list)
  publisher <- extract_publisher(data_list)


  first_column_names <-
    lapply(c("title_", "description_", "language_"),
           function(x) paste0(x, languages))

  second_column_names <- list(paste0("url_", url_formats))

  # Title, description, languages and url_path
  # are vector of the same length as the number of
  # languages. We want thosse vectors to be
  # data frames with as many columns as there are languages
  # the below expression loops through each of the
  # data_list slot, turns it into a df and creates
  # column names based on the specific language
  all_dfs <-
    dplyr::bind_cols(
      Map(vector_to_df_columnwise,
      list(title, description, languages, url_path),
      c(first_column_names, second_column_names)
      )
    )

  # Do not change this to tibble because
  # all_dfs is a df and it will throw an error.
  # data.frame in this case allows to pass a data frrame
  # and bins the columns together
  data_df <-
    data.frame(
      keywords = keywords,
      all_dfs,
      date = date_data,
      publisher = publisher
    )

  if (half_columns) {
    data_df <- data_df[grepl("description|url|publisher", names(data_df))]
  }

  final_df <- dplyr::as_tibble(data_df)
  final_df
}
