
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

# id <- 'l01080193-numero-total-de-edificios-con-viviendas-segun-numero-de-plantas'
# resp <- content(get_resp(path_dataset_id(id)))
# data_list <- resp$result$items[[1]]

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
