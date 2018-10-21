# [NOT FULLY TESTED SCRIPT]

# Exploring datos.gob.es
# The origin of these set of functions is path_datasets(), since it creates the root path (/catalog/dataset/)
# shared by all of them.
path_catalog_dataset <- function(path, param = NULL, ...) {
  make_url(paste0("catalog/dataset/", path), param = param)
}

# Building path for getting datasets by keyword
path_explore_keyword <- function(keyword) {
  keyword  <- iconv(keyword, to='ASCII//TRANSLIT')
  path_catalog_dataset(paste0("keyword/", keyword))
}



# Function to explore the keywords by publisher (documentation pending)
explorar_keywords <- function(keyword, publisher) {

  stopifnot(length(publisher) == 1,
            length(keyword) == 1,
            is.character(publisher),
            is.character(keyword))

  lower_pub <- tolower(publisher)

  if (!(lower_pub %in% tolower(publishers_available$publisher_code)))
    stop("Publisher `", publisher,"`` not available. Please check publishers_available() to get the available ones.")

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


    data_explored <- dplyr::tibble(description = desc_datasets,
                                   is_readable = is_format_readable,
                                   path_id = id_datasets,
                                   complete_url = url_datasets)
    data_explored
  })

  final_dt <- Reduce(rbind, data_explored)

  # Becasue we only accept one publisher, we need to run this only once at the end
  final_dt$publisher <- translate_publisher(code = toupper(publisher))

  final_dt[c('description', 'publisher', 'is_readable', 'path_id', 'complete_url')]
}
