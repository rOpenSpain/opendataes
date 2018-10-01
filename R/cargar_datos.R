#' Extract data and metadata from an end path of \url{https://datos.gob.es/}
#'
#' @param path_id The end path of a dataset such as 'l01280148-seguridad-ciudadana-actuaciones-de-seccion-del-menor-en-educacion-vial-20141'
#' from \url{https://datos.gob.es/es/catalogo/l01280148-seguridad-ciudadana-actuaciones-de-seccion-del-menor-en-educacion-vial-20141}.
#' Must be a character string of length 1.
#' @param encoding The encoding passed to read the csv. Most cases should be resolved with either
#' 'UTF-8' or 'latin1'. However, the user can try any encoding available at
#' @param ... arguments passed to \code{\link[readr]{read_csv}}
#'
#' @details The API of \url{https://datos.gob.es/} is not completely homogenous because it is an aggregator
#' of many different API's from different cities and provinces of Spain. Currently, \code{cargar_datos}
#' can read csv.
#'
#' However, in order for \code{cargar_datos} to read these files the access URL of the data needs to
#' end with any of these paths. Note that the access URL is not the same as the URL from
#' \url{https://datos.gob.es/}. It is the URL from the publisher of the dataset.
#'
#' For example, this URL: \url{http://datos.gob.es/es/catalogo/a02002834-numero-de-centros-segun-ancho-de-banda-de-la-conexion-a-internet-que-tiene-el-centro6}
#' says that it hosts an XML file but once you click on the 'download' XML, it redirects to a JavaScript based
#' webste that has the table in XML hosted on the website. This URL does not end with .xml and so it is unreadble to
#' the package to \code{cargar_datos}.
#'
#' Fortunately, \code{cargar_datos} will attempt to read the data and if it can't read it, it will return
#' the same \code{\link[tibble]{tibble}} but with a column named URL with all available URL's. The user can then
#' access the data manually.
#'
#' \code{cargar_datos} attempts to extract all the metadata of the dataset but sometimes this metadata might be wrong.
#' For example, there are cases when there are several languages available and the order of the different titles
#' are not in the same order of the languages. If you find any of these errors, try raising the issue directly to
#' \url{https://datos.gob.es/} as the package attempts to extract all metadata in the same order as it is.
#'
#' Whenever the metadata is in different languages, the resulting \code{\link[tibble]{tibble}} will have
#' the same numer of rows as there are languages containing the different texts in different languages and
#' repeating the same information whenever it's similar across languages (such as the dates, which are language agnostic).
#'
#' In case the API returns empty requests, both data and metadata will be empty \code{\link[tibble]{tibble}}'s
#' with the same column names.
#'
#' @return a list with two slots: metadata and data. metadata is a \code{\link[tibble]{tibble}} that contains the
#' metadata of the file. data is also a \code{\link[tibble]{tibble}} and contains the actual data requested.
#' See the details section for some caveats.
#' @export
#'
#' @examples
#'
#' id <- 'l01080193-numero-total-de-edificios-con-viviendas-segun-numero-de-plantas'
#' some_data <- cargar_datos(id)
#'
#' # Print the file to get some useful information
#' some_data
#'
#' # Access the metadata
#' some_data$metadata
#'
#' # Access the data
#' some_data$data
#'
cargar_datos <- function(path_id, encoding = 'UTF-8', ...) {
  raw_json <- get_resp(path_dataset_id(path_id))

  if (!data_list_correct(raw_json)) return(list())

  data_list <- raw_json$result$items[[1]]

  returned_list <-
    structure(
      list(
        metadata = extract_metadata(data_list),
        data = suppress_all(get_data(data_list, encoding, ...))
      ),
      class = "datos_gob_es"
    )


  returned_list
}

# Print method for the datos_gob_es
print.datos_gob_es <- function(x) {

  # If spanish is available, return metadata in spanish. If not
  # then choose the first language. Spanish is preferable because
  # I think all metadata must have at least spanish, so it's safer.
  es_index <- x$metadata$language == "es"

  if (any(es_index)) {
    metadata <- x$metadata[x$metadata$language == "es", ]
  } else {
    metadata <- x$metadata[1, ]
  }

  check_read <- function(data) ncol(data) > 2 & !all(names(data) %in% c('format', "URL"))


  # We ned to check whether data is a data frame (1 data read)
  # or a list with many dfs. If it's a df we turn it into a list
  # with the df inside because below (vapply) we check whether the data
  # was read by if x[[2]] is a dataframe then it is going to loop
  # through the columns and we need to loop over dataframes
  if (is.data.frame(x$data)) x[2] <- list(x[2])

  has_url_col <- vapply(x[[2]], check_read, logical(1))
  number_of_reads  <- paste0(sum(has_url_col), " out of ", length(has_url_col))

  # This width allows the text to cut at the specified width of the console
  # automatically
  width_print <- getOption("width") - 18
  # Add ellipsis if it's equalk or higher than the width
  ellipsis <- if (nchar(metadata$description) < width_print) "" else "..."

  cat("<datos.gob.es API>",
      paste0("   Description: ", strtrim(metadata$description, width_print), ellipsis),
      paste0("   Publisher: ", metadata$publisher),
      paste0("   Languages: ", paste0(x$metadata$language, collapse = ", ")),
      paste0("   Date of release: ", metadata$date_issued),
      paste0("   # of files read: ", number_of_reads),
      sep = "\n")
}
