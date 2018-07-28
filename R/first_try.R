library(httr)
library(readr)
library(RJSONIO)
library(dplyr)


# Run this until we ahve the package structure
# When we have it we can just run devtools::load_all()
# or CTRL + SHIFT + L in Windows
source("./R/utils.R")

datos_url <- make_url("a07002862-resultado-electoral-2003-informacion-complementaria2")

res <- content(get_resp(datos_url), as = 'parsed')

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


