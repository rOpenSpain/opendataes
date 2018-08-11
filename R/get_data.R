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

    # Add regex end of string to avoid matching the path by change somewhere
    # in the string
    data_format <- paste0("\\.", is_file_readable)

    access_urls <-  extract_access_url(data_list)

    exact_access_url <- grep(data_format,
                             access_urls,
                             value = TRUE)

    output_data <- dplyr::as_tibble(rio::import(exact_access_url,
                                                encoding = "Latin-1"))
  } else {
    output_data <- dplyr::as_tibble(extract_access_url(data_list))
    names(output_data) <- "URL"
  }

  # Output
  output_data

}
