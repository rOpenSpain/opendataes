#' Get prefered method to download data (if possible)
#'
#' @param data_list A data_list similar to resp$result$items[[1]] that contains information on a dataset

is_readable <- function(data_list) {

  # URL formats
  file_format <- extract_url_format(data_list)

  # Allowed formats, in preference order
  allowed_format <- c("csv", "xls", "xlsx", "json", "html", "xml")

  if (any(!is.na(match(file_format, allowed_format)))) {
    file_format <- file_format[order(match(file_format, allowed_format))][[1]]
  } else {
    file_format
    }

  file_format

}

