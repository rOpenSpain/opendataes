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
make_url <- function(query_path, param = NULL, ...) {
  hostname <- "datos.gob.es/apidata/catalog/dataset"

  # We could simply just paste together the URL
  # but `httr` has better handling for
  # character vectors of class url
  # that deal with the structure of URL's
  # more properly than I would know.
  semi_url <-
    structure(
      list(
        scheme = "http",
        hostname = hostname,
        path = query_path,
        query = param,
        ...),
      class = "url"
    )

  build_url(semi_url)
}

# Example:
url <- make_url(query_path = "theme/sector-publico", param = list('_pageSize' = 50, '_page' = 1))
resp <- get_resp(url)

