# This function receives a file, tries to determine the
# delimiter and dispatches based on the separator of the file
# to a read_* function from readr
determine_read_generic <- function(file) {
  delimiter <- csv_delim(file)

  read_generic <-
    switch(delimiter,
         "," = readr::read_csv,
         ";" = readr::read_csv2,
         "\t" = readr::read_tsv,
         " " = function(file, ...) readr::read_delim(file = file, delim = " ", ...),
         ":" = function(file, ...) readr::read_delim(file = file, delim = ":", ...),
         function() stop())
  # If cannot find delimiter, return an error that will be called
  # when the function is used. Because this read generic will be called
  # under try in extract_data, the error will suggest that the data cannot be read
  # and just return the meta data
  read_generic
}


# Ideally we'd like to eliminate this function altogether and port
# it into a package that can be called once this function
# is cleaned and refactored
csv_delim <- function(file, guess_max = 1000, threshold_rows = 0.9,
                      delim = c(',', '\t', ';', ' ', ':')) {

  data <-
    tryCatch(
      readr::read_lines(file, n_max = guess_max),
      error = function(e) NA_character_
    )

  data <- strsplit(data, "\n")

  filtered_data <- data[!vapply(data, function(x) is.null(x) | length(x) == 0, logical(1))]

  # Get the number of rows read after deleting the empty rows
  # in filtered_data because otherwise the threshold is calculated
  # wrongly
  rows_read <- min(length(filtered_data), guess_max)

  res <- lapply(filtered_data, function(x) table(strsplit(x, "")))
  table_names <- lapply(res, names)

  all_chars <- unlist(table_names)

  all_chars <- all_chars[all_chars %in% delim]

  prop_repetition <- table(all_chars) / rows_read
  if (one_true(prop_repetition == 1) %in% c('one true', '> one true')) {
    repeated_names <- names(which(prop_repetition == 1))
  } else if (any(prop_repetition > threshold_rows)) {
    repeated_names <- names(which(prop_repetition > threshold_rows))
  } else {
    # Because no character was matched at or over the threshold of rows
    return (NA_character_)
  }

  unique_repetitions <- lapply(repeated_names, function(delimiter) {
    unique_vals <- unique(unlist(lapply(res, `[`, delimiter)))
    unique_vals[!is.na(unique_vals)]
  })

  unique_repetitions <- stats::setNames(unique_repetitions, repeated_names)

  if (length(unique_repetitions) == 0) return(NA_character_)

  same_count_delimiter <-
    sapply(unique_repetitions, function(x) length(unique(x)) == 1)

  matched_delimiters <- one_true(same_count_delimiter)

  if (matched_delimiters == "one true") {

    unique_delimiter <- names(same_count_delimiter[which(same_count_delimiter)])

    return(unique_delimiter)

  } else if (matched_delimiters == '> one true') {
    # If there were two delimiters that have a single number
    # repeated in all rows and are at 90% of the rows or more
    # filter whether these two are in the prefered delimiters and
    # pick in the order of preference in the preferred delimiters

    conflicting_delims <- names(same_count_delimiter)[which(same_count_delimiter)]
    chosen_delimiter <- pick_preference(conflicting_delims, delim)

    return(chosen_delimiter)
  } else {
    undecided_delims <- names(same_count_delimiter)
    chosen_delimiter <- pick_preference(undecided_delims, delim)

    if (!is.na(chosen_delimiter)) return(chosen_delimiter)
  }

  NA_character_ # no format was found
}


one_true <- function(x) {
  table_trues <- table(x)

  # If there's only ONE true, return 'one true',
  # if more than one true, return '> one true'
  # else 'no true'
  if (any(as.logical(names(table_trues)))) {
    if (table_trues['TRUE'] == 1) {
      return("one true")
    } else {
      return("> one true")
    }
  }

  "no true"
}


# Matches the `match` in `pool_matches` and brings
# according to the order in `pool_matches`
pick_preference <- function(match, pool_matches) {
  available_delims <- match %in% pool_matches

  if (any(available_delims)) {
    # We turn to factor in order to sort according to the allowed formats.
    # This way when we subset we keep the order of preference of files.
    sorted_formats <- sort(factor(pool_matches, levels = pool_matches))
    chosen_delimiter <- sorted_formats[which(sorted_formats %in% match)[1]]
    chosen_delimiter <- as.character(chosen_delimiter)
    return(chosen_delimiter)
  }

  NA_character_
}
