#' Build a custom url using the httr url class
#'
#' @param path the end path of the dataset of interest
#' @param param arguments for a query
#' @param ... any other arguments to building the path correctly. See \code{modify_url}
#'
make_url <- function(path, param, ...) {
  hostname <- "datos.gob.es/apidata"

  # datos.gob.es has some problems authenticating the SSL
  # certificate. With this line we ignore SSL. Note that
  # this is more important than it seems as it allows
  # the travis checks to pass.
  httr::set_config(httr::config(ssl_verifypeer = 0L))

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
        path = path,
        query = param,
        ...),
      class = "url"
    )

  httr::build_url(semi_url)
}


#' Build a url with a complete catalog/ prefix URL
#'
#' @inheritParams make_url
path_catalog <- function(path, param = NULL, ...) {
  make_url(paste0("catalog/", path), param = param, ...)
}

#' Build a url with a complete catalog/dataset prefix URL
#'
#' @inheritParams make_url
path_catalog_dataset <- function(path, param = NULL, ...) {
  make_url(paste0("catalog/dataset/", path), param = param)
}

#' Build a url with a complete datasets/ prefix URL
#'
#' @param param Extra parameters to add to the url. For this function, this is
#' useless because there's not further paths to the dataset end point. Keeping
#' the argument for consistency
#' @param ... Extra arguments passed to \code{\link[httr]{build_url}}
path_datasets <- function(param = NULL, ...) {
  path_catalog("dataset", param, ...)
}

#' Build a url with a complete publishers/ prefix URL
#'
#' @param param Extra parameters to add to the url. For this function, this is
#' useless because there's not further paths to the publishers end point. Keeping
#' the argument for consistency
#' @param ... Extra arguments passed to \code{\link[httr]{build_url}}
path_publishers <- function(param = NULL, ...) {
  path_catalog("publisher", param, ...)
}

#' Build a url with a complete distribution/ prefix URL
#'
#' @param param Extra parameters to add to the url. For this function, this is
#' useless because there's not further paths to the distribution end point. Keeping
#' the argument for consistency
#' @param ... Extra arguments passed to \code{\link[httr]{build_url}}
path_distribution <- function(param = NULL, ...) {
  path_catalog("distribution", param, ...)
}


#' Build a url with a complete begin-end date/ prefix URL
#'
#' @param start_date Start date in YYYYMMDD format
#' @param end_date End date in YYYYMMDD format
#' @param param Extra parameters to add to the url. For this function, this is
#' useless because there's not further paths to the distribution end point. Keeping
#' the argument for consistency
#' @param ... Extra arguments passed to \code{\link[httr]{build_url}}
path_begin_end_date <- function(start_date, end_date, param = NULL, ...) {
  #### NOTE ######
  # Function NOT READY for use.

  # First approach: working always with strings. I've not worked so much with dates in R (lubridate maybe?)
  # This function must(!) be reviewed
  year_start <- substr(start_date, 1, 4)
  year_end <- substr(end_date, 1, 4)
  month_start <- substr(start_date, 5, 6)
  month_end <- substr(end_date, 5, 6)
  day_start <- substr(start_date, 7, 8)
  day_end <- substr(end_date, 7, 8)

  start_date <- paste0(year_start, "-", month_start, "-", day_start, "T00:00Z")
  end_date <- paste0(year_end, "-", month_end, "-", day_end, "T00:00Z")

  #httr::modify_url(paste0(path_datasets(), "/modified/begin/", start_date, "/end/", end_date))
  httr::modify_url(paste0(path_datasets(), "/modified/begin/", start_date, "/end/", end_date))
}


#' Build a url with an ID of a dataset
#'
#' @param id dataset id from datos.gob.es such as 'l01080193-numero-total-de-edificios-con-viviendas-segun-numero-de-plantas'
#' @param param Extra parameters to add to the url.
#' @param ... Extra arguments passed to \code{\link[httr]{build_url}}
path_dataset_id <- function(id, param = NULL, ...) {
  httr::modify_url(paste0(path_datasets(), "/", id), query = param, ...)
}


#' Build a url to search for a given keyword in the datos.gob.es API
#'
#' @param keyword A string with the keyword to build the path with.
#'
path_explore_keyword <- function(keyword) {
  # Remove accents and other spanish words
  keyword  <- iconv(keyword, to='ASCII//TRANSLIT')
  path_catalog_dataset(paste0("keyword/", keyword))
}
