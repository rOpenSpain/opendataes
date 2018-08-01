#' Make GET requests with repeated trials
#'
#' @param url A url, preferably from \code{make_url}
#' @param attempts_left Number of attempts of trying to request from the website
#'
#' @examples
get_resp <- function(url, attempts_left = 5) {

  stopifnot(attempts_left > 0)

  resp <- httr::GET(url)

  # Ensure that returned response is application/json
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  # On a successful GET, return the response
  if (httr::status_code(resp) == 200) {
    resp
  } else if (attempts_left == 1) { # When attempts run out, stop with an error
    stop_for_status(resp) # Return appropiate error message
  } else { # Otherwise, sleep a second and try again
    Sys.sleep(2)
    get_resp(url, attempts_left - 1)
  }


}

#' Build a custom url using the httr url class
#'
#' @param query_path the end path of the dataset of interest
#' @param param arguments for a query
#' @param ... any other arguments to building the path correctly. See \code{modify_url}
#'
#' @return
#' @export
#'
#' @examples
make_url <- function(path, param, ...) {
  hostname <- "datos.gob.es/apidata"

  # We could simply just paste together the URL
  # but `httr` has better handling for
  # character vectors of class url
  # that deal with the structure of URL's
  # more properly than I would know.
  semi_url <-
    structure(
      list(
        scheme = "https",
        hostname = hostname,
        path = query_path,
        query = param,
        ...),
      class = "url"
    )

  build_url(semi_url)
}


# LEt's develop a 'factory' of path functions that will
# return all different paths but making everything modular.
# Only one function is in charge of one path.
path_catalog <- function(path, param, ...) {
  make_url(paste0("catalog/", end_path), param, ...)
}

path_datasets <- function(param, ...) {
  path_catalog("dataset", param, ...)
}

path_publishers <- function(param, ...) {
  path_catalog("publisher", param, ...)
}


# Once you develop a new path above
# just add it below following the same guideline
grab_path <- function(path, param = NULL, ...) {
  switch(path,
         'datasets' = path_datasets(param, ...),
         'publishers' = path_publishers(param, ...))
}


## Get the total number of pages
# for a given path
page_counter <- function(path) {
  total_page <- 0
  url_path <- grab_path(path, param = list('_pageSize' = 50,
                                           '_page' = total_page))

  resp <- content(get_resp(url_path))

  while (length(resp$result$items) != 0) {
    total_page = total_page + 1

    url_path <- grab_path(path, param = list('_pageSize' = 50,
                                             '_page' = total_page))

    resp <- content(get_resp(url_path))
  }

  total_page
}












### Here's a set of functions to extract elements from
### a slot with data information

## Checks data_list is correct
## As we find further requiremenrs that a data_list
## must have, we add it here
data_list_correct <- function(data_list) {
  wrong_length <- length(data_list) == 0
  no_names <- is.null(attr(data_list, 'names'))

  # Because the URL slot is in the distribution
  # and all data_lists must follow the same structure
  no_distribution_slot <- 'distribution' %in% names(data_list)

  if (wrong_length | no_names | no_distribution_slot) {
    return(FALSE)
  }

  TRUE
}


## Extract elements from that data_list
## For now I think we should concentrate on
# Keywords
# Title
# Description
# Access URL
# Language
# Format
# Date
# Publisher


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

  title <- data_list$title
  title
}

extract_url <- function(data_list) {
  if (!data_list_correct(data_list)) {
    return(character())
  }

  if (!'access_url' %in% names(data_list)) {
    "No URL available"
  }

  access_url <- data_list$distribution$accessURL
  access_url
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



publisher <- "http://datos.gob.es/recurso/sector-publico/org/Organismo/EA0010987"


pt <- content(get_resp(grab_path('publishers')))

# Example:
# url <- make_url(query_path = "theme/sector-publico", param = list('_pageSize' = 50, '_page' = 1))
# resp <- get_resp(url)



#' Function to get datasets related to specified topic
#'
#' @param topic Related topic
#'
#' @return
#' @export
#'
#' @examples

# This function is a draft and it could be generalized. Instead of topic we could use it for retrieve the
# datasets by id, title, format, keyword, etc
# We also have to deal with pagination (see "Pagination" on https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html).
# For now only retrieves 50 first results descendly sorted by date and title
get_topics <- function(topic) {

  url <- make_url(query_path = paste0("theme/", topic), param = list('_sort' = '-issued,title', '_pageSize' = 50, '_page' = 1))
  response <- get_resp(url)

  # Parse the response obtained with get_resp
  cont <- content(response, as = "parsed")

  # All items has the same root
  items <- cont$result$items

  # Create empty dataframe with returned info
  df <- data.frame(
    title = character(),
    desc = character(),
    about = character(),
    last_modified = character(),
    stringsAsFactors = FALSE
  )

  # Loop to populate previous dataframe.
  # If information is not provided, it will be filled with NAs
  # (THINK HOW TO IMPROVE THIS CODE. IT WORKS BUT I DON'T LIKE THE SYNTAX)
  for (i in 1:length(items)){
    df[i,1] <- ifelse(length(items[[i]]$title[[1]]) > 0, items[[i]]$title[[1]], NA)
    df[i,2] <- ifelse(length(items[[i]]$description[[1]]$`_value`[[1]]) > 0, items[[i]]$description[[1]]$`_value`[[1]], NA)
    df[i,3] <- ifelse(length(items[[i]]$`_about`[[1]]) > 0, items[[i]]$`_about`[[1]], NA)
    df[i,4] <- ifelse(length(items[[i]]$modified[[1]]) > 0, items[[i]]$modified[[1]], NA)
  }

  return(df)


}
