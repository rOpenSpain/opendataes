#' Request all available publishers from datos.gob.es
#'
#' @return a \code{\link[tibble]{tibble}} with two columns: publisher_code and publishers
#' @export
#'
#' @examples
#'
#' datos_publiser()
#'
datos_publisher <- function() {
  # Specify random huge number just to avoid running out of pages
  # in the future.
  resp <- get_resp_paginated(path_publishers(), 1000)
  # Delete the publisher URL because they ALL lead to the same
  # URL which is this one http://datos.gob.es/recurso/sector-publico/org/Organismo#data
  list_tibbles <- lapply(resp$result$items, function(x) dplyr::as_tibble(x[-1]))
  publisher_df <- dplyr::bind_rows(list_tibbles)
  names(publisher_df) <- c('publisher_code', 'publisher')
  publisher_df
}
