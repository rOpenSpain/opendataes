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

  if (length(is_file_readable) != 0) {

    output_data <- vector("list", length(is_file_readable))

    # Loop over each chosen file
    for (index in seq_along(is_file_readable)) {

      data_url <- names(is_file_readable)[index]
      # Determine the delimited of the file
      read_generic <- determine_read_generic(data_url)

      custom_locale <- readr::locale("es", encoding = encoding)

      # Try reading the data and saving it in **that** data frame's slot
      output_data[[index]] <-
        try(read_generic(file = data_url,
                         locale = custom_locale,
                         ...),
            silent = TRUE)

      # If the data that was read raised an error, return
      # the format and URL as we would have done if there are no
      # files to read at all from this data_list
      if (inherits(output_data[[index]], "try-error")) {
        output_data[[index]] <-
          dplyr::tibble(format = is_file_readable[index],
                        URL = names(is_file_readable[index]))
      }
    }

  } else {
    # If no file to read, return all urls and formats
    output_data <- return_metadata(data_list)
  }

  # If only one data frame, return a tibble
  # otherwise return list
  if (length(output_data) == 1) output_data[[1]] else output_data
}


return_metadata <- function(data_list) {
  # If there's any error, this means that none of the formats
  # could be read. So we return the the tibble with the url
  # formats and the access urls
  output_data <- dplyr::tibble(extract_url_format(data_list),
                               extract_access_url(data_list))

  names(output_data) <- c("format", "URL")

  output_data
}

