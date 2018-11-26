#' Request all available publishers from \url{https://datos.gob.es/}
#'
#' @return a \code{\link[tibble]{tibble}} with two columns: publisher_code and publishers
#' @export
#'
#' @examples
#'
#' cargar_publishers()
#'
cargar_publishers <- function() {
  # Specify random huge number just to avoid running out of pages
  # in the future.
  resp <- get_resp_paginated(path_publishers(), 1000)
  # Delete the publisher URL because they ALL lead to the same
  # URL which is this one http://datos.gob.es/recurso/sector-publico/org/Organismo#data
  list_tibbles <- lapply(resp$result$items, function(x) tibble::as_tibble(x[-1]))
  publisher_df <- Reduce(rbind, list_tibbles)
  names(publisher_df) <- c('publisher_code', 'publisher')
  publisher_df[!duplicated(publisher_df$publisher_code), ]
}

#' Available publishers that `opendataes` can read
#'
#' @return a \code{\link[tibble]{tibble}} with two columns: publishers and publisher_code
#'
#' @seealso \code{\link{cargar_publishers}}
#' @export
#'
#' @examples
#'
#' publishers_available
publishers_available <- tibble::tibble(
  publishers = c("Ayuntamiento de Barcelona", "Ayuntamiento de Madrid", "Ayuntamiento de Valencia", 
                 "Ayuntamiento de Las Palmas de Gran Canaria", "Ayuntamiento de Bilbao"),
  publisher_code = c("L01080193", "L01280796", "L01462508", "L01350167", "L01480209")
)

#' Translate publisher code to publisher name
#'
#' @param code A publisher code
translate_publisher <- function(code) {
  all_publishers <- cargar_publishers()
  all_publishers
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
