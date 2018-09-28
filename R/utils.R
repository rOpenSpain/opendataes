#' Make GET requests over several pages of an API
#'
#' @param url URL to request from, preferably from the \code{path_*} functions
#' @param num_pages Number of pages to request
#' @param page The page at which the request should being. This should rarely be used
#' @param ... Arguments passed to \code{\link[httr]{GET}}
#'
#' @return the parsed JSON object as a list but inside the items
#' slots it contains all data lists obtained from the pages specified
#' in \code{num_pages}
get_resp_paginated <- function(url, num_pages = 1, page = 0, ...) {

  # For the parsed items data_list
  data_list <- NULL
  # For the accumulated number of data_lists
  data_lists <- NULL

  while (num_pages > 0) {

    # IMPORTANT!!
    # All number of page queries should be specified as arguments in this
    # function rather than in the URL
    url <- httr::modify_url(url, query = list("_pageSize" = 50, "_page" = page))

    parsed_response <- get_resp(url)

    data_list <- parsed_response$result$items
    data_lists <- c(data_list, data_lists)

    if (is.null(parsed_response$result$`next`)) {
      # finished pagination, can quit
      break
    }

    # set up for next iteration
    page <- page + 1
    num_pages <- num_pages - 1
  }

  # Return the last parsed_response but with
  # all accumulated datasets in the items slot
  parsed_response$result$items <- data_lists

  parsed_response
}


#' Make GET requests with repeated trials
#'
#' @param url A url, preferably from the \code{path_*} functions
#' @param attempts_left Number of attempts of trying to request from the website
#' @param ... Arguments passed to \code{\link[httr]{GET}}
get_resp <- function(url, attempts_left = 5, ...) {

  # Handle so that the API knows who's downloading the data
  ua <- httr::user_agent("https://github.com/cimentadaj/opendataes")

  stopifnot(attempts_left > 0)

  resp <- httr::GET(url, ua, ...)
  # To avoid making too many quick requests
  Sys.sleep(1)

  # Ensure that returned response is application/json
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  # On a successful GET, return the response's content
  if (httr::status_code(resp) == 200) {
    httr::content(resp)
  } else if (attempts_left == 1) { # When attempts run out, stop with an error
    httr::stop_for_status(resp) # Return appropiate error message
  } else { # Otherwise, sleep a second and try again
    Sys.sleep(1)
    get_resp(url, attempts_left - 1)
  }

}


#' Translate publisher code to publisher name
#'
#' @param code A publisher code
translate_publisher <- function(code) {
  all_publishers <- datos_publisher()
  index <- which(all_publishers$publisher_code == code)
  if (length(index) == 0) return("Publisher not available")
  all_publishers$publisher[index]
}

# Mime map of equivalents of different mime formats
# Taken from https://github.com/yihui/mime/blob/master/R/mimemap.R
# Keep adding as you need it
mimemap <- c(
    csv = "text/csv"
  )

permitted_formats <- c("csv")



# This is a function factory that produces the same function
# but changes only one function.
determine_valid_urls <- function(.fun) {
  function(data_list, allowed_formats = permitted_formats) {

    # URL formats
    url_formats <- extract_url_format(data_list)
    # This is the only thing that changes
    names(url_formats) <- .fun(data_list)

    available_formats <- allowed_formats %in% url_formats

    if (any(available_formats)) {
      # We turn to factor in order to sort according to the allowed formats.
      # This way when we subset we keep the order of preference of files.
      sorted_formats <- sort(factor(url_formats, levels = allowed_formats))

      names_urls <- names(sorted_formats)
      formats <- as.character(sorted_formats)

      index_formats <- formats %in% allowed_formats
      url_formats <- formats[index_formats]

      # We return the file format together with
      # the chosen unit for this particular function so that
      # we automatically subset the url without calling
      # extract_access_url again.
      names(url_formats) <- names_urls[index_formats]
    } else {
      url_formats <- character(0)
    }

    url_formats
  }
}

# This is equivalent to the previous is_readable but now
# is more modular
determine_dataset_name <- determine_valid_urls(extract_dataset_name)
determine_dataset_url <- determine_valid_urls(extract_access_url)

# Vector with available publishers
publishers_available = dplyr::tibble(
  name = c("Ayuntamiento de Barcelona"),
  id = c("l01080193")
)

suppress_all <- function(x) suppressMessages(suppressWarnings(x))
