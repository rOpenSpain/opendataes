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
        path = path,
        query = param,
        ...),
      class = "url"
    )

  build_url(semi_url)
}

# LEt's develop a 'factory' of path functions that will
# return all different paths but making everything modular.
# Only one function is in charge of one path.
path_catalog <- function(path, param = NULL, ...) {
  make_url(paste0("catalog/", path), param = param, ...)
}

## these are all 'static' urls, meaning that they have
# no parameters to add.

path_datasets <- function(param = NULL, ...) {
  path_catalog("dataset", param, ...)
}

path_publishers <- function(param = NULL, ...) {
  path_catalog("publisher", param, ...)
}

path_distribution <- function(param = NULL, ...) {
  path_catalog("distribution", param, ...)
}



# Therse are 'dynamic' urls meaning that they have additional
# parameters that need to be specified to get the data

# ID here is the id of the dataset
# like 'l01080193-numero-total-de-edificios-con-viviendas-segun-numero-de-plantas'
path_dataset_id <- function(id, param = NULL, ...) {
  httr::modify_url(paste0(path_datasets(), "/", id), query = param, ...)
}

