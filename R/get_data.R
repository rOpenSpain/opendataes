#' Function for getting data from the website.
#'
#' @param data_list A data_list similar to resp$result$items[[1]] that contains information on a dataset
#'
#' @details \code{get_data} will accept the end path of a data base and it will search for the access url.
#' If the access url has an end path of either csv, xls, xlsx, json, html and xml, then it will
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

get_data <- function(data_list) {

  # Check if the data_list is readable
  is_file_readable <- is_readable(data_list)

  # If readable, get data
  # If not, get tibble with URL
  if (length(is_file_readable) != 0) {

    # Get the first format.
    format_to_read <- is_file_readable[1]
    args_rio <-
      list(file = names(format_to_read),
           format = format_to_read,
           setclass = "tibble")

    # The encoding argument is only available for the formats below, so
    # it should be added only when that format is the one being read.
    # I use is_file_readable and the data forms without the . because
    # it's easier to match than data_format which has two slashes
    if (format_to_read %in% c("csv", "html", "xml")) {
      args_rio <- c(args_rio, "encoding" = "UTF-8")
      # This encoding does not work perfectly. Sometimes
      # it does't work and UTF-8 captures spanish accents but
      # other times it doesn't.
    }

    # Try reading the data
    output_data <- try(do.call(rio::import, args_rio), silent = TRUE)

    # If there's any error, return an empty tibble
    if (methods::is(output_data, "try-error")) {
      output_data <- dplyr::tibble(extract_url_format(data_list),
                                   extract_access_url(data_list))
      names(output_data) <- c("format", "URL")
    }

  } else {
    output_data <- dplyr::tibble(extract_url_format(data_list),
                                 extract_access_url(data_list))
    names(output_data) <- c("format", "URL")
  }

  # Output
  output_data

}
