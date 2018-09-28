#' Check if publisher is available in opendataes
#'
#' @param data_list A data_list similar to resp$result$items[[1]] that contains information on a dataset
is_publisher_available <- function(data_list) {
  publisher_code <- tolower(extract_publisher_code(data_list))
  ifelse(publisher_code %in% tolower(publishers_available$id), TRUE, FALSE)
}
