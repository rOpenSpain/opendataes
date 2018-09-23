# This is a file where we should add tests to make sure the function works.

# this if very informal and not ready for unit testing just yet. This file is to unconver
# errors from the function and fix them as we go.


id <- 'a16003011-indicadores-del-mercado-laboral-del-ano-2005-al-2014'
pt <- cargar_datos(id)

path_id <- 'l02000011-casas-consistoriales'
pt <- cargar_datos(path_id)

id <- 'ea0001304-atraques-de-cruceros'
pt <- cargar_datos(id)

id <- 'a16003011-ganancia-media-y-brecha-salarial-por-hora-entre-mujeres-y-hombres-seguntipo-de-contrato-cae-y-espana-2002-y-20061'
pt <- cargar_datos(id)

id <- 'ea0010587-flujo-de-inmigracion-procedente-del-extranjero-por-ano-sexo-y-edad-anual-estadistica-de-migraciones-identificador-api-24282'
pt <- cargar_datos(id)

id <- 'ea0010587-viviviendas-con-ejecucion-hipotecaria-iniciada-segun-titular-de-la-vivienda-por-provincia-anual-estadistica-sobre-ejecuciones-hipotecarias-identificador-api-10745'
pt <- cargar_datos(id)

id <- 'l01080193-listado-de-direcciones-de-edificios-de-la-ciudad-de-barcelona'
pt <- cargar_datos(id)

id <- 'l01080193-viviendas-principales-segun-anos-de-construccion'
pt <- cargar_datos(id)

id <- 'l01280796-oficinas-de-registro'
pt <- cargar_datos(id)

id <- 'l01080193-base-de-datos-del-presupuesto-ingresos'
pt <- cargar_datos(id)

id <- 'l01080193-domicilios-segun-nacionalidad'
pt <- cargar_datos(id)

id <- 'l01280796-accidentes-de-trafico-2009-a-2014-seguridad-vial'
pt <- cargar_datos(id)

id <- 'l01280796-bici-ciclocarriles'
pt <- cargar_datos(id)

id <- 'l01080193-elecciones-al-parlamento-europeo-sobre-electores'
pt <- cargar_datos(id)

id <- 'l01080193-relacion-de-tramos-de-la-via-publica'
pt <- cargar_datos(id)

id <- 'l01080193-definicion-de-itinerarios-y-especificacion-de-los-tramos-que-lo-componen'
pt <- cargar_datos(id)

id <- 'l01290672-padron-de-habitantes-por-distrito-municipal-2008'
pt <- cargar_datos(id)

## By changing the index we can try all diferent datasets
resp <- get_resp(path_datasets())
id <- extract_endpath(resp$result$items[[5]])
pt <- cargar_datos(id)


id <- 'l01080193-numero-total-de-edificios-con-viviendas-segun-numero-de-plantas'
pt <- cargar_datos(id)

path_id <- 'l01080193-estaciones-de-bicing-mecanicas-y-electricas'
pt <- cargar_datos(id)


# Error because we try to convert jsons to tibble
path_id <- 'l01080193-estaciones-de-bicing-mecanicas-y-electricas'
pt <- cargar_datos(path_id)


id <- "ea0001304-empresas-prestatarias-de-servicios-portuarios1"
resp <- get_resp(path_dataset_id(id))
data_list <- resp$result$items[[1]]
is_readable(data_list)
# Correct ordering of the urls

id <- "l01280148-contratos-mayores-4-trimestre-20141"
resp <- get_resp(path_dataset_id(id))
data_list <- resp$result$items[[1]]
is_readable(data_list)
pt <- cargar_datos(id)
# Encoding crashes
pt$data


id <- "a16003011-toponimia-de-la-comunidad-autonoma-vasca-nombres-oficiales-y-nombres-normativizados"
resp <- get_resp(path_dataset_id(id))
data_list <- resp$result$items[[1]]
is_readable(data_list)
pt <- cargar_datos(id)

# Even when a format is not availabel it doesn't return that format. Only format availables


id <- 'l01280148-deuda-municipal-2015-20161'
resp <- get_resp(path_dataset_id(id))
data_list <- resp$result$items[[1]]
is_readable(data_list)
pt <- cargar_datos(id)
# When no format is available, it returns an empty vector, as expected
