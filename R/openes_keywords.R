#' Explore datasets by keywords and publishers in \url{https://datos.gob.es/}
#'
#' @param keyword A character string specifying a keyword to identify a data set. For example, 'vivienda'.
#' @param publisher A character string with the \strong{publisher code}. Should be only one publisher code.
#' See \code{\link{publishers_available}} for the permitted publisher codes.
#'
#' @details \code{openes_keywords} works only for searching for one keyword for a given publisher. For example,
#' 'viviendas' for the Ayuntamiento of Barcelona. If there are no matches for a keyword-publisher combination,
#' \code{openes_keywords} will raise an error stating that there are no matches.
#'
#' \code{openes_keywords} returns a data frame with the following columns:
#'
#' \itemize{
#' \item description: a short description of each of the matched datasets in Spanish
#'  (Spanish is set as default if available, if not, then the first non-spanish language is chosen).
#' \item publisher: the entity that publishes the dataset. See \code{\link{openes_load_publishers}} for all available publishers.
#' \item is_readable: whether that dataset is currently readable by \code{\link{openes_load}}.
#' See \code{\link{permitted_formats}} for currently available formats.
#' \item path_id: the end path that identifies that dataset in the \url{https://datos.gob.es/} API.
#' \item url: the complete url of the dataset in \url{https://datos.gob.es/}. Note that this URL is not the access URL to the dataset
#' but to the dataset's homepage in \url{https://datos.gob.es/}.
#' }
#'
#' In most cases the user will need to narrow down their search because the result of \code{openes_keywords}
#' will have too many datasets. Beware that for passing the result of this function to \code{\link{openes_load}} the final
#' data frame needs to be narrowed down to only one dataset (that is, 1 row) and the structure needs to be the same
#' as from the original output of \code{openes_keywords} (same column names, in the same order). See examples below.
#'
#'
#' @seealso \code{\link{openes_load}}
#'
#' @return A \code{\link[tibble]{tibble}} containing the matched datasets.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' library(dplyr)
#'
#' kw <- openes_keywords("vivienda", "l01080193") # Ayuntamiento de Barcelona
#'
#' kw
#'
#' # Notice how we narrow down to only 1 dataset
#' dts <-
#'  kw %>%
#'  filter(grepl("Precios", description)) %>% # Narrow it down to only 1 dataset
#'  openes_load('ASCII')
#'
#' # Notice that we had to specify the encoding because printing the dataset returns an error.
#' # If that happens to you, try figuring out the encoding with readr::guess_encoding(dts$data[[1]])
#' # and specify the most likely encoding in `openes_load`
#'
#' dts$metadata
#'
#' dts$data
#' }
#'
openes_keywords <- function(keyword, publisher) {
  stopifnot(length(publisher) == 1,
            length(keyword) == 1,
            is.character(publisher),
            is.character(keyword))

  lower_pub <- tolower(publisher)

  if (!(lower_pub %in% tolower(publishers_available$publisher_code)))
    stop("Publisher `", publisher,"` not available. Please check publishers_available() to get the available ones.") #nolintr

  path_keyword <- path_explore_keyword(keyword)
  resp <- get_resp_paginated(path_keyword, num_pages = 1000) # num_pages to determinate
  data_list <- resp$result$items

  # Get publishers based on the searched keyword
  all_publishers_code <- lapply(data_list, function(x) tolower(extract_publisher_code(data_list = x)))

  # Match the publishers code from the keyword to the requested publisher. This works
  # because we already checked that this publisher is available.
  matched_publisher <- all_publishers_code %in% lower_pub

  # If any of them match with the available pubilshers code, get the index of them
  if (!any(matched_publisher))
    stop("There are not datasets matching the keyword `", keyword, "` and publisher `", publisher, "`")

  index_publisher <- which(matched_publisher)

  # Build the final dataframe
  # If description is in Spanish, take Spanish, else, take the first one
  selected_dl <- data_list[index_publisher]

  data_explored <- lapply(selected_dl, function(x) {
    es_lang <- extract_language(x) == "es"

    # Always has preerence for spanish because I think all
    # datasets must have Spanish.
    if (any(es_lang)) {
      desc_datasets <- extract_description(x)[es_lang]
    } else {
      desc_datasets <- extract_description(x)[1]
    }

    # Get name, id and URL
    id_datasets <- extract_endpath(x)
    url_datasets <- extract_url(x)

    # Is this file (already filtered by publisher) readable?
    formats_read <- determine_dataset_url(x)

    # Check whether the match is readable.
    is_format_readable <- ifelse(length(formats_read) != 0, TRUE, FALSE)


    data_explored <- tibble::tibble(description = desc_datasets,
                                   is_readable = is_format_readable,
                                   path_id = id_datasets,
                                   url = url_datasets)
    data_explored
  })

  final_dt <- Reduce(rbind, data_explored)

  # Becasue we only accept one publisher, we need to run this only once at the end
  final_dt$publisher <- translate_publisher(code = toupper(publisher))

  final_dt <- final_dt[!duplicated(final_dt$description), ]

  final_dt[c('description', 'publisher', 'is_readable', 'path_id', 'url')]
}
