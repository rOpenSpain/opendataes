#' #' Function for getting data in csv format
#' #'
#' #' @param format_to_read Value obtained in is_readable function, which returns the first available format to read.
#'
#' readcsv <- function(format_to_read, ...) {
#'   # Set arguments in order to read csv
#'   # Fread already takes care of csv separator
#'   args_readcsv <- list(input = names(format_to_read))
#'   inargs <-  list(...)
#'   args_readcsv[names(inargs)] <- inargs
#'
#'   # Read csv
#'   df <- try(do.call(data.table::fread, args_readcsv))
#'
#'   # Convert to tibble in order to keep the same format among all results
#'   dplyr::as_tibble(df)
#' }
#'
#'
#' #' Function for getting data in xls/xlsx format
#' #'
#' #' @param format_to_read Value obtained in is_readable function, which returns the first available format to read.
#'
#' readxls <- function(format_to_read, ...) {
#'   # This function will be able to read both xls and xlsx files
#'   # Set arguments
#'   args_readxls <- list(path = names(format_to_read))
#'   inargs <-  list(...)
#'   args_readxls[names(inargs)] <- inargs
#'
#'   # Read file
#'   # We should think how to handle with multiple excel sheets because
#'   # so far we just read the first one
#'   df <- try(do.call(readxl::read_excel, args_readxls))
#'
#'   # Return df
#'   df
#' }
#'
#' #' Function for getting data in json format
#' #'
#' #' @param format_to_read Value obtained in is_readable function, which returns the first available format to read.
#'
#' readjson <- function(format_to_read, ...) {
#'   # This function will be able to read json files
#'   # Set arguments
#'   args_readjson <- list(txt = names(format_to_read))
#'   inargs <-  list(...)
#'   args_readjson[names(inargs)] <- inargs
#'
#'   # Read file
#'   df <- try(do.call(jsonlite::fromJSON, args_readjson))
#'
#'   # Convert to tibble in order to keep the same format among all results
#'   dplyr::as_tibble(df)
#' }
#'
#' #' Function for getting data in html format
#' #'
#' #' @param format_to_read Value obtained in is_readable function, which returns the first available format to read.
#'
#' readhtml <- function(format_to_read, ...) {
#'   # This function will be able to read xml files
#'   # Set arguments
#'   args_readxml <- list(txt = names(format_to_read))
#'   inargs <-  list(...)
#'   args_readxml[names(inargs)] <- inargs
#' }
