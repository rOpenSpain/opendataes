#' Request all available publishers from datos.gob.es
#'
#' @return a \code{\link[tibble]{tibble}} with two columns: publisher_code and publishers
#' @export
#'
#' @examples
#'
#' datos_publisher()
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

#' Available publishers that `opendataes` can read
#'
#' @return a \code{\link[tibble]{tibble}} with two columns: publishers and publisher_code
#'
#' @seealso \code{\link[opendataes]{datos_publishers}}
#' @export
#'
#' @examples
#'
#' publishers_available
publishers_available <- dplyr::tibble(
  publishers = c("Ayuntamiento de Barcelona"),
  publisher_code = c("l01080193")
)

#' Translate publisher code to publisher name
#'
#' @param code A publisher code
translate_publisher <- function(code) {
  all_publishers <- datos_publisher()
  index <- which(all_publishers$publisher_code == code)
  if (length(index) == 0) return("Publisher not available")
  all_publishers$publisher[index]
}

#' Check if publisher is available in opendataes
#'
#' @param data_list A data_list similar to resp$result$items[[1]] that contains information on a dataset
is_publisher_available <- function(data_list) {
  publisher_code <- tolower(extract_publisher_code(data_list))
  res <- ifelse(publisher_code %in% tolower(publishers_available$publisher_code),
                TRUE, FALSE)

  res
}
