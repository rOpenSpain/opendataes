#' Extract data and metadata from a given data set of \url{https://datos.gob.es/}
#'
#' @param path_id The end path of a dataset such as 'l01280148-seguridad-ciudadana-actuaciones-de-seccion-del-menor-en-educacion-vial-20141'
#' from \url{https://datos.gob.es/es/catalogo/l01280148-seguridad-ciudadana-actuaciones-de-seccion-del-menor-en-educacion-vial-20141}.
#' Must be a character string of length 1.
#'
#' @param encoding The encoding passed to read (all) the csv ('s). Most cases should be resolved with either
#' 'UTF-8', latin1' or 'ASCII'. There are edge cases such as when printing any of the dataframes in the
#' data slot results in the error 'input string 1 is invalid UTF-8'. When that happens, use
#' \code{\link[readr]{guess_encoding}} to determine the encoding and try reading the dataset with the
#' new encoding.
#'
#' @param ... Arguments passed to \code{\link[readr]{read_csv}} and the other related \code{read_*} functions from \\code{\link[readr]{readr}}.
#' Internally, \code{cargar_datos} determines the delimiter of the file being read but the arguments
#' for each of these functions are practically the same, so it doesn't matter how \code{cargar_datos}
#' determines the delimiter, any of the arguments will work on all \code{read_*} functions.
#'
#' @details
#' \code{cargar_datos} can return two possible outcomes: either an empty list or a list with a slot called metadata
#' and another slot called data. Whenever the \code{path_id} argument is an invalid dataset path, it will return an empty list.
#' When \code{path_id} is a valid dataset path, \code{cargar_datos} will return an a list with the two slots described above.
#'
#' For the metadata slot, \code{cargar_datos} returns a \code{\link[tibble]{tibble}} with most available metadata of the dataset.
#' The columns are:
#'
#' \itemize{
#' \item keywords: the available keywords from the dataset in the homepage of the dataset.
#' \item language: the available languages of the dataset's metadata. Note that that this does not mean that the dataset
#' is in different languages but only the metadata.
#' \item description: a short description of the data being read.
#' \item url: the url of the dataset in \url{https://datos.gob.es/}. Note that this URL is not the access URL to the dataset
#' but to the dataset's homepage in \url{https://datos.gob.es/}.
#' \item date_issued: the date at which the dataset was uploaded.
#' \item date_modified: the date at which the last dataset was uploaded. If the dataset has only been uploaded once, this
#' will return \code{'No modification date available'}.
#' \item publisher: the entity that publishes the dataset. See \code{\link{cargar_publishers}} for all available publishers.
#' \item publisher_data_url: the homepage of the dataset in the website of the publisher. This is helpful to look
#' at the definitions of the columns in the dataset.
#' }
#'
#' The metadata of the API can sometimes be returned in an incorrect order. For example, there are cases when there are several
#' languages available and the order of the different descriptions are not in the same order of the languages. If you
#' find any of these errors, try raising the issue directly to \url{https://datos.gob.es/} as the package extracts all
#' metadata in the same order as it is.
#'
#' Whenever the metadata is in different languages, the resulting \code{\link[tibble]{tibble}} will have
#' the same numer of rows as there are languages containing the different texts in different languages and
#' repeating the same information whenever it's similar across languages (such as the dates, which are language agnostic).
#'
#' In case the API returns empty requests, both data and metadata will be empty \code{\link[tibble]{tibble}}'s
#' with the same column names.
#'
#' For the data slot, \code{cargar_datos} returns a list containing at least one \code{\link[tibble]{tibble}}.
#' If the dataset being request has file formats that \code{cargar_datos} can read (see \code{\link{permitted_formats}})
#' it will read those files. If that dataset has several files, then it will return a list of the same length
#' as there are datasets where each slot in that list is a \code{\link[tibble]{tibble}} with the data. If for
#' some reason any of the datasets being read cannot be read, \code{cargar_datos} has a fall back mechanism
#' that returns the format that attempted to read together with the URL so that the user can try to read the
#' dataset directly. In any case, the result will always be a list with \code{\link[tibble]{tibble}}'s
#' where each one could be the requested dataset (success) or a dataset with the format and url that attempted
#' to read but failed (failure).
#'
#' Inside the data slot, each list slot containing \code{\link[tibble]{tibble}}'s will be named according
#' to the dataset that was read. When there is more than one dataset, the user can then enter the website
#' in the \code{url} column in the metadata slot to see all names of the datasets. This is handy, for example,
#' when the same dataset is repeated across time and we want to figure out which data is which from the slot.
#'
#' The API of \url{https://datos.gob.es/} is not completely homogenous because it is an aggregator
#' of many different API's from different cities and provinces of Spain. \code{cargar_datos} can only read
#' a limited number of file formats but will keep increasing as the package evolves. You can check the available file formats
#' in \code{\link{permitted_formats}}. If the file format of the requested \code{path_id} is not readable, \code{cargar_datos}
#' will return a list with only one \code{\link[tibble]{tibble}} with all available formats with their respective data URL
#' inside the data slot so that users can read the manually.
#'
#' In a similar line, in order for \code{cargar_datos} to provide the safest behaviour, it is very conservative in which
#' publisher it can read from \url{https://datos.gob.es/}. Because some publishers do not have standardized datasets
#' reading many different publishers can become very messy. \code{cargar_datos} currently reads files from selected
#' publishers because they offer standardized datasets which makes it safer to read. As the package evolves and the
#' data quality improves between publishers, the package will include more publishers. See the publishers that the
#' package can read in \code{publishers_available}.
#'
#' @return if \code{path_id} is a valid dataset path, a list with two slots: metadata and data. Each slot
#' contains \code{\link[tibble]{tibble}}'s that contain either metadata or the data itself. If \code{path_id}
#' is not a valid dataset path, it returns an empty list. See the details section for some caveats.
#' @export
#'
#' @examples
#'
#' # For a dataset with only one file to read
#' example_id <- 'l01080193-fecundidad-madres-de-15-a-19-anos-quinquenal-2003-2014'
#' some_data <- cargar_datos(example_id)
#'
#' # Print the file to get some useful information
#' some_data
#'
#' # Access the metadata
#' some_data$metadata
#'
#' # Access the data. Note that the name of the dataset is in the list slot. Whenever
#' # there are different files being read, you might want to enter to website of the dataset
#' # you can either enter the website for some_data$metadata$url or some_data$metadata$publisher_data_url
#' some_data$data
#'
#' # For a dataset with many files
#'
#' \dontrun{
#' example_id <- 'l01080193-domicilios-segun-nacionalidad'
#' res <- cargar_datos(example_id)
#'
#' # Note that you can see how many files were read in '# of files read'
#' res
#'
#' # See how all datasets were read but we're not sure what each one means.
#' # Check the metadata and read the description. If that doesn't do it,
#' # go to the URL of the dataset from the metadata.
#' res$data
#'
#' # Also note that some of the datasets were not read uniformly correct. For example,
#' # some of these datasets were read with more columns or more rows. This is left
#' # to the user to fix. We could've added new arguments to the `...` but that would
#' # apply to ALL datasets and it then becomes too complicated.
#'
#'
#' # Encoding problems
#'
#'
#' id <- 'l01080193-descripcion-de-la-causalidad-de-los-accidentes-gestionados-por-la-guardia-urbana-en-la-ciudad-de-barcelona'
#' pl <- cargar_datos(id)
#'
#' # The dataset is read successfully but once we print them, there's an error
#' pl$data
#' $`2011_ACCIDENTS_CAUSES_GU_BCN_.csv`
#' Error in nchar(x[is_na], type = "width") :
#'   invalid multibyte string, element 1
#'
#' # This error is due to an encoding problem.
#' # We can use readr::guess_encoding to determine the encoding and reread
#'
#' # This suggests an ASCII encoding
#' library(readr)
#' guess_encoding(pl$data[[1]])
#'
#' pl <- cargar_datos(id, 'ASCII')
#'
#' # Success
#' pl$data
#' }
#'
cargar_datos <- function(path_id, encoding = 'UTF-8', ...) {

  if (!is.character(path_id) || length(path_id) > 1) stop("`path_id` must be a character of length 1")

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

  check_read <- function(data) ncol(data) > 3 & !all(names(data) %in% c('name', 'format', "URL"))


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
