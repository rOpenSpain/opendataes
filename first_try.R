library(httr)

# The only thing we have to change is the end of path that represents the dataset that we want.
url <- "http://datos.gob.es/apidata/catalog/dataset/a07002862-resultado-electoral-2003-informacion-complementaria2"

res <- content(GET(url))

raw_vec <- unlist(res$result$items[[1]]$distribution)

all_urls <- raw_vec[grepl("accessURL", names(raw_vec))]

head(read.csv2(all_urls))
