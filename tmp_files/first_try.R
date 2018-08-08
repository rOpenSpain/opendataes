library(httr)
library(readr)
library(RJSONIO)
library(dplyr)
library(purrr)
library(magrittr)
library(tidyr)

# Run this until we ahve the package structure
# When we have it we can just run devtools::load_all()
# or CTRL + SHIFT + L in Windows
source("./R/utils.R")

# There is abig difference between using http and https
# https is more secure and returns a json and http returns
# an xml or html.

# I added a check for json in the GET request to make sure it can
# be converted into json

# datos_url <- make_url("a07002862-resultado-electoral-2003-informacion-complementaria2")
datos_url <- "https://datos.gob.es/apidata/catalog/dataset"

res <- content(get_resp(datos_url))

raw_vec <- unlist(res$result$items[[1]]$distribution)

all_urls <- raw_vec[grepl("accessURL", names(raw_vec))]

read.csv2(all_urls)



url <- make_url(query_path = "theme/seguridad", param = list('_sort' = '-issued,title', '_pageSize' = 50, '_page' = 1))
resp <- get_resp(url)
cont <- content(resp, as = "parsed")


df <- data.frame(
  title = character(),
  desc = character(),
  about = character(),
  last_modified = character(),
  stringsAsFactors = FALSE
)

for (i in 1:length(cont$result$items)){
  df[i,1] <- ifelse(length(cont$result$items[[i]]$title[[1]]) > 0, cont$result$items[[i]]$title[[1]], NA)
  df[i,2] <- ifelse(length(cont$result$items[[i]]$description[[1]]$`_value`[[1]]) > 0, cont$result$items[[i]]$description[[1]]$`_value`[[1]], NA)
  df[i,3] <- ifelse(length(cont$result$items[[i]]$`_about`[[1]]) > 0, cont$result$items[[i]]$`_about`[[1]], NA)
  df[i,4] <- ifelse(length(cont$result$items[[i]]$modified[[1]]) > 0, cont$result$items[[i]]$modified[[1]], NA)
}



# This function isn't valid
# I can't deal with NULLS
get_topics <- function(topic) {

  url <- make_url(query_path = paste0("theme/", topic), param = list('_sort' = '-issued,title', '_pageSize' = 50, '_page' = 1))
  response <- get_resp(url)

  # Parse the response obtained with get_resp
  cont <- content(response, as = "parsed")

  # All items has the same root
  items <- cont$result$items

  # Create empty dataframe with returned info
  df <- data.frame(
    title = character(),
    desc = character(),
    about = character(),
    last_modified = character(),
    stringsAsFactors = FALSE
  )

  df <- purrr::map_df(items, `[`, c("title", "description" , "_about", "modified"))

  return(df)


}

df <- get_topics("seguridad")


# This is another approach but I can't deal with nested list
url <- make_url(query_path = paste0("theme/", "salud"), param = list('_sort' = '-issued,title', '_pageSize' = 50, '_page' = 1))
response <- get_resp(url)

# Parse the response obtained with get_resp
cont <- content(response, as = "parsed")

# All items has the same root
items <- cont$result$items


df <- data.frame(
  title = purrr::map_chr(items, .null = NA, "title"),
  desc = purrr::map_chr(items, .null = NA, "description"),
  about = purrr::map_chr(items, .null = NA, "_about"),
  last_modified = purrr::map_chr(items, .null = NA, "modified")
)

# This solution isn't valid
# If any of the fields is composed by a list, returned column value is null

# Unnest list
items <- cont$result$items %>% unlist()

df <- data.frame(
  title = if(length(which(grepl("^title$", names(items)))) > 0) items[grepl("^title$", names(items))] else NA,
  desc = if(length(which(grepl("^description$", names(items)))) > 0) items[grepl("^description$", names(items))] else NA,
  about = if(length(which(grepl("^_about$", names(items)))) > 0) items[grepl("^_about$", names(items))] else NA,
  last_modified = if(length(which(grepl("^modified$", names(items)))) > 0) items[grepl("^modified$", names(items))] else NA,
  stringsAsFactors = FALSE
)
