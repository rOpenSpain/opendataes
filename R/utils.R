#' Make GET requests with repeated trials
#'
#' @param url A url, preferably from \code{make_url}
#' @param attempts_left Number of attempts of trying to request from the website
#'
#' @examples
get_resp <- function(url, attempts_left = 5) {
  
  stopifnot(attempts_left > 0)
  
  resp <- httr::GET(url, accept_json())
  
  # On a successful GET, return the response
  if (httr::status_code(resp) == 200) {
    resp
  } else if (attempts_left == 1) { # When attempts run out, stop with an error
    stop("Cannot connect to the API")
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
        scheme = "https",
        hostname = hostname,
        path = query_path,
        query = param,
        ...),
      class = "url"
    )
  
  build_url(semi_url)
}
