#install.packages("rio")
library("rio")
library("dplyr")


#' Function for getting data from the website.
#'
#' @param data_list A data_list similar to resp$result$items[[1]] that contains information on a dataset


# Examples
id <- 'l01080193-numero-total-de-edificios-con-viviendas-segun-numero-de-plantas'
id <- 'a02002834-numero-de-centros-segun-ancho-de-banda-de-la-conexion-a-internet-que-tiene-el-centro5'
id <- 'l02000012-centros-culturales'
resp <- httr::content(get_resp(path_dataset_id(id)))
data_list <- resp$result$items[[1]]



get_data <- function(data_list) {

  # Check if the data_list is readable
  is_file_readable <- is_readable(data_list)

  # If readable, get data
  # If not, get tibble with URL
  if(length(is_file_readable) != 0) {
    output_data <- extract_access_url(data_list)[grepl(is_file_readable, extract_access_url(data_list))] %>%
      rio::import(encoding = "Latin-1") %>%
      as_tibble()
  } else {
    output_data <- extract_access_url(data_list) %>%
      as_tibble %>%
      `colnames<-`(c("URL"))
  }

  # Output
  output_data

}
