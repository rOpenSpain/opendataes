library(httr)
library(readr)

# Run this until we ahve the package structure
# When we have it we can just run devtools::load_all()
# or CTRL + SHIFT + L in Windows
source("./R/utils.R")

datos_url <- make_url("a07002862-resultado-electoral-2003-informacion-complementaria2")

res <- content(get_resp(datos_url), as = 'parsed')

raw_vec <- unlist(res$result$items[[1]]$distribution)

all_urls <- raw_vec[grepl("accessURL", names(raw_vec))]

read.csv2(all_urls)

hola
