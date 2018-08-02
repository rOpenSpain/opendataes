#' Make GET requests over several pages of a URL
#'
#' @param url URL to request from, preferably from the \code{path_*} functions
#' @param num_pages Number of pages to request
#' @param page The page at which the request should being. This should rarely be used
#' @param ... Arguments passed to \code{\link[httr]{GET}}
#'
#' @return the parsed JSON object as a list but inside the items
#' slots it contains all data lists obtained from the pages specified
#' in \code{num_pages}
parse_paginated_resp <- function(url, num_pages = 1, page = 0, ...) {

  # For the parsed items data_list
  data_list <- NULL
  # For the accumulated number of data_lists
  data_lists <- NULL

  while (num_pages > 0) {

    # IMPORTANT!!
    # All number of page queries should be specified as arguments in this
    # function rather than in the URL
    url <- httr::modify_url(url, query = list("_pageSize" = 50, "_page" = page))

    req <- get_resp(url)

    parsed_response <- httr::content(req)

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
  # On a successful GET, return the response
  if (httr::status_code(resp) == 200) {
    resp
  } else if (attempts_left == 1) { # When attempts run out, stop with an error
    stop_for_status(resp) # Return appropiate error message
  } else { # Otherwise, sleep a second and try again
    Sys.sleep(1)
    get_resp_GET(url, attempts_left - 1)
  }

}

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
