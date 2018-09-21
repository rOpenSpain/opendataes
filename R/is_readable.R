#' Get prefered method to download data (if possible)
#'
#' @param data_list A data_list similar to resp$result$items[[1]] that contains information on a dataset
is_readable <- function(data_list) {

  # URL formats
  file_format <- extract_url_format(data_list)
  names(file_format) <- extract_access_url(data_list)

  # Allowed formats, in preference order
  allowed_format <- c("csv")

  available_formats <- allowed_format %in% file_format

  if (any(available_formats)) {
    # We turn to factor in order to sort according to the allowed formats.
    # This way when we subset we keep the order of preference of files.
    sorted_formats <- sort(factor(file_format, levels = allowed_format))

    urls <- names(sorted_formats)
    formats <- as.character(sorted_formats)

    index_formats <- formats %in% allowed_format
    file_format <- formats[index_formats]

    # We return the file format together with
    # the url so that we automatically subset the url without calling
    # extract_access_url again.
    names(file_format) <- urls[index_formats]
  } else {
    file_format <- character(0)
  }

  file_format
}

