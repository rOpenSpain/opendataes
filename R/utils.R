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
get_resp <- function(url, attempts_left = 5, ...) {

  stopifnot(attempts_left > 0)

  resp <- httr::GET(url, ...)

  # Ensure that returned response is application/json
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  # On a successful GET, return the response's content
  if (httr::status_code(resp) == 200) {
    httr::content(resp)
  } else if (attempts_left == 1) { # When attempts run out, stop with an error
    stop_for_status(resp) # Return appropiate error message
  } else { # Otherwise, sleep a second and try again
    Sys.sleep(1)
    get_resp_GET(url, attempts_left - 1)
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
