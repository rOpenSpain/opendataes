# [NOT FULLY TESTED SCRIPT]

# Exploring datos.gob.es
# The origin of these set of functions is path_datasets(), since it creates the root path (/catalog/dataset/)
# shared by all of them.
path_catalog_dataset <- function(path, param = NULL, ...) {
  make_url(paste0("catalog/dataset/", path), param = param)
}

# Building path for getting datasets by theme
path_explore_themes <- function(theme) {
  theme <- tolower(theme)
  theme <- remove_accents(string = theme)
  # Separate words by "-"
  if(lengths(gregexpr("\\W+", theme)) + 1 > 1) {
   theme <- set_hyphen(string = theme)
  }
  path_catalog_dataset(paste0("theme/", theme))
}

# Building path for getting datasets by keyword
path_explore_keyword <- function(keyword) {
  theme <- tolower(keyword)
  theme <- remove_accents(string = keyword)
  path_catalog_dataset(paste0("keyword/", keyword))
}

# Building path for getting datasets by title
path_explore_title <- function(title) {
  theme <- tolower(title)
  theme <- remove_accents(string = title)
  path_catalog_dataset(paste0("title/", title))
}


