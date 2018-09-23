#' Function for getting data from the website.
#'
#' @param data_list A data_list similar to resp$result$items[[1]] that contains information on a dataset
#'
#' @details \code{get_data} will accept the end path of a data base and it will search for the access url.
#' If the dataset is either a csv, xls, xlsx or xml, then it will
#' attempt to read it. If it succeeds, it will return the data frame. If not, it will return
#' the data frame with only one column containing all available access URL's.
#'
#' For example, this URL: http://datos.gob.es/es/catalogo/a02002834-numero-de-centros-segun-ancho-de-banda-de-la-conexion-a-internet-que-tiene-el-centro6
#' says that it has a XML file but once you click on the 'download' XML, it redirects to a JavaScript based
#' webste that has the table. This file unfortunately is unreadble to the package.
#'


# Examples
# id <- 'l01080193-numero-total-de-edificios-con-viviendas-segun-numero-de-plantas'
# id <- 'a02002834-numero-de-centros-segun-ancho-de-banda-de-la-conexion-a-internet-que-tiene-el-centro6'
# id <- 'l02000012-centros-culturales'
# resp <- get_resp(path_dataset_id(id))
# data_list <- resp$result$items[[1]]

get_data <- function(data_list, encoding, ...) {

  # Check if publisher is available
  if(!is_publisher_available(data_list)) {
    stop("Publisher not available. Please check publishers_available() to get the available ones.")
  }

  # Check if the data_list is readable
  is_file_readable <- is_readable(data_list)

  # Initialize while-loop
  output_data <- dplyr::tibble()
  class(output_data) <- "try-error"

  while (inherits(output_data, "try-error")) {

    if (length(is_file_readable) != 0) {
      # Get the first format.
      format_to_read <- is_file_readable[1]

      # names(is_file_readable) contains the URL of the CSV file
      data_url <- names(is_file_readable)

      read_generic <- determine_read_generic(data_url)

      custom_locale <- readr::locale("es", encoding = encoding)

      # Try reading the data
      output_data <-
        try(read_generic(file = data_url,
                         locale = custom_locale,
                         ...),
            silent = TRUE)

      is_file_readable <- is_file_readable[-1]
    } else {
    # If there's any error, this means that none of the formats
    # could be read. So we return the the tibble with the url
    # formats and the access urls
    output_data <- dplyr::tibble(extract_url_format(data_list),
                               extract_access_url(data_list))

    names(output_data) <- c("format", "URL")
    }
  }

  # Output
  output_data

}
