# This is a file where we should add tests to make sure the function works.

# this if very informal and not ready for unit testing just yet. This file is to unconver
# errors from the function and fix them as we go.


id <- 'a16003011-indicadores-del-mercado-laboral-del-ano-2005-al-2014'
pt <- extract_datos(id)


id <- 'a16003011-ganancia-media-y-brecha-salarial-por-hora-entre-mujeres-y-hombres-seguntipo-de-contrato-cae-y-espana-2002-y-20061'
pt <- extract_datos(id)

## By changing the index we can try all diferent datasets
resp <- get_resp(path_datasets())
id <- extract_endpath(resp$result$items[[5]])
pt <- extract_datos(id)


id <- 'l01080193-numero-total-de-edificios-con-viviendas-segun-numero-de-plantas'
pt <- extract_datos(id)
