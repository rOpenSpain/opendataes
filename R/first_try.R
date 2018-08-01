library(httr)
library(readr)

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
