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

  if(!any(tolower(publisher) %in% tolower(publishers_available$publisher_code)))
    stop("Publisher not available. Please check publishers_available() to get the available ones.")

  path_keyword <- path_explore_keyword(keyword)
  resp <- get_resp_paginated(path_keyword, num_pages = 100) # num_pages to determinate
  data_list <- resp$result$items

  # Get all publishers based on the searched keyword
  all_publishers_code <- lapply(data_list, function(x) tolower(extract_publisher_code(data_list = x)))


  # If any of them match with the available pubilshers code, get the index of them
  if(!any(all_publishers_code %in% publishers_available$publisher_code))
    stop("There is no dataset for publishers available. Please check publishers_available() to get the available ones.")
  index_publisher <- which(all_publishers_code %in% publishers_available$publisher_code)


  # Are these files (already filtered by publisher) readable?
  all_are_readable <- lapply(data_list[index_publisher], function(x) determine_dataset_url(data_list = x))
  is_format_readable <- ifelse(length(all_are_readable) != 0, TRUE, FALSE)


  # Build the final dataframe
  # If description is in Spanish, take Spanish, else, take the first one
  if(any(extract_language(data_list[[index_publisher]]) == "es")) {
    desc_datasets <- extract_description(data_list[[index_publisher]])
    desc_datasets <- desc_datasets[extract_language(data_list[[index_publisher]]) == "es"]
  } else {
    desc_datasets <- extract_description(data_list[[index_publisher]])[1]
  }

  # Get name, id and URL
  name_publisher <- translate_publisher(code = toupper(publisher))
  id_datasets <- extract_endpath(data_list[[index_publisher]])
  url_datasets <- names(determine_dataset_url(data_list[[index_publisher]]))


  data_explored <- dplyr::tibble(Descripcion_fichero = desc_datasets,
                                  Publicador = name_publisher,
                                  Es_legible = is_format_readable,
                                  ID_lectura = id_datasets,
                                  URL = url_datasets)


  data_explored

}
