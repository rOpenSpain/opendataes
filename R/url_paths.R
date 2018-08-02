#' Build a custom url using the httr url class
#'
#' @param path the end path of the dataset of interest
#' @param param arguments for a query
#' @param ... any other arguments to building the path correctly. See \code{modify_url}
#'
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


#' Build a url with an ID of a dataset
#'
#' @param id dataset id from datos.gob.es such as 'l01080193-numero-total-de-edificios-con-viviendas-segun-numero-de-plantas'
#' @param param Extra parameters to add to the url.
#' @param ... Extra arguments passed to \code{\link[httr]{build_url}}
path_dataset_id <- function(id, param = NULL, ...) {
  httr::modify_url(paste0(path_datasets(), "/", id), query = param, ...)
}

