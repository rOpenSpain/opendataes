# This is a file where we should add tests to make sure the function works.

# this if very informal and not ready for unit testing just yet. This file is to unconver
# errors from the function and fix them as we go.



# Examples
# id <- 'l01080193-numero-total-de-edificios-con-viviendas-segun-numero-de-plantas'
# id <- 'a02002834-numero-de-centros-segun-ancho-de-banda-de-la-conexion-a-internet-que-tiene-el-centro6'
# id <- 'l02000012-centros-culturales'
# resp <- get_resp(path_dataset_id(id))
# data_list <- resp$result$items[[1]]


id <- 'a16003011-indicadores-del-mercado-laboral-del-ano-2005-al-2014'
pl <- cargar_datos(id)

path_id <- 'l02000011-casas-consistoriales'
pl <- cargar_datos(path_id)

id <- 'ea0001304-atraques-de-cruceros'
pl <- cargar_datos(id)

id <- 'a16003011-ganancia-media-y-brecha-salarial-por-hora-entre-mujeres-y-hombres-seguntipo-de-contrato-cae-y-espana-2002-y-20061'
pl <- cargar_datos(id)

id <- 'ea0010587-flujo-de-inmigracion-procedente-del-extranjero-por-ano-sexo-y-edad-anual-estadistica-de-migraciones-identificador-api-24282'
pl <- cargar_datos(id)

id <- 'ea0010587-viviviendas-con-ejecucion-hipotecaria-iniciada-segun-titular-de-la-vivienda-por-provincia-anual-estadistica-sobre-ejecuciones-hipotecarias-identificador-api-10745'
pl <- cargar_datos(id)

id <- 'l01080193-listado-de-direcciones-de-edificios-de-la-ciudad-de-barcelona'
pl <- cargar_datos(id)

id <- 'l01080193-viviendas-principales-segun-anos-de-construccion'
pl <- cargar_datos(id)

id <- 'l01080193-puntos-wifi'
pl <- cargar_datos(id)

id <- 'l01080193-descripcion-de-la-causalidad-de-los-accidentes-gestionados-por-la-guardia-urbana-en-la-ciudad-de-barcelona'
pl <- cargar_datos(id, 'ASCII')

id <- 'l01280796-oficinas-de-registro'
pl <- cargar_datos(id)

id <- 'l01080193-base-de-datos-del-presupuesto-ingresos'
pl <- cargar_datos(id)

id <- 'l01280796-accidentes-de-trafico-2009-a-2014-seguridad-vial'
pl <- cargar_datos(id)

id <- 'l01280796-bici-ciclocarriles'
pl <- cargar_datos(id)

id <- 'l01080193-elecciones-al-parlamento-europeo-sobre-electores'
pl <- cargar_datos(id, encoding = 'latin1')

id <- 'l01080193-relacion-de-tramos-de-la-via-publica'
pl <- cargar_datos(id)

id <- 'l01080193-definicion-de-itinerarios-y-especificacion-de-los-tramos-que-lo-componen'
pl <- cargar_datos(id)

id <- 'l01290672-padron-de-habitantes-por-distrito-municipal-2008'
pl <- cargar_datos(id)

## By changing the index we can try all diferent datasets
resp <- get_resp(path_datasets())
id <- extract_endpath(resp$result$items[[5]])
pl <- cargar_datos(id)


id <- 'l01080193-numero-total-de-edificios-con-viviendas-segun-numero-de-plantas'
pl <- cargar_datos(id)

path_id <- 'l01080193-estaciones-de-bicing-mecanicas-y-electricas'
pl <- cargar_datos(id)


# Error because we try to convert jsons to tibble
path_id <- 'l01080193-estaciones-de-bicing-mecanicas-y-electricas'
pl <- cargar_datos(path_id)


id <- "ea0001304-empresas-prestatarias-de-servicios-portuarios1"
resp <- get_resp(path_dataset_id(id))
data_list <- resp$result$items[[1]]
is_readable(data_list)
# Correct ordering of the urls

id <- "l01280148-contratos-mayores-4-trimestre-20141"
resp <- get_resp(path_dataset_id(id))
data_list <- resp$result$items[[1]]
is_readable(data_list)
pl <- cargar_datos(id)
# Encoding crashes
pl$data


id <- "a16003011-toponimia-de-la-comunidad-autonoma-vasca-nombres-oficiales-y-nombres-normativizados"
resp <- get_resp(path_dataset_id(id))
data_list <- resp$result$items[[1]]
is_readable(data_list)
pl <- cargar_datos(id)

# Even when a format is not availabel it doesn't return that format. Only format availables


id <- 'l01280148-deuda-municipal-2015-20161'
resp <- get_resp(path_dataset_id(id))
data_list <- resp$result$items[[1]]
is_readable(data_list)
pl <- cargar_datos(id)
# When no format is available, it returns an empty vector, as expected


## tests for csv_delim

# this is ;
determine_read_generic("https://datosabiertos.ayto-arganda.es/dataset/4e2b6867-0c88-4b2e-a290-bad625a9b3ac/resource/740d3e9c-735a-4145-9149-7af8eb9e8405/download/registro-de-entrada.-4-trimestre-2017.-oficinas-de-registro.csv")

# this is ,
determine_read_generic("https://datosabiertos.ayto-arganda.es/dataset/3b9e2138-a3c9-45da-8086-7eba0abce047/resource/e9c0b4a3-8955-46b4-a63b-ad619609588d/download/escuela-infantilalumnos.csv")

# this is ,
determine_read_generic('http://datos.apc.es/wcm/connect/42a6fc93-d6d7-4f4e-971b-046adf9578ed/ATRAQUES_CRUCEROS.CSV?MOD=AJPERES&CONVERT_TO=url&CACHEID=ROOTWORKSPACE-42a6fc93-d6d7-4f4e-971b-046adf9578ed-miMIved')

# this is ,
determine_read_generic('https://apirtod.dipucadiz.es/api/datos/casas_consistoriales.csv&rnd=1725731944')

# this is ,
determine_read_generic('https://datosabiertos.ayto-arganda.es/dataset/bb8079e5-1425-467c-ab87-860e3d1aba6d/resource/d3dfeb5b-fc76-4bf0-9577-0975a0342479/download/sectorpublicodocumentacionnotarialhistoricototalmunicipio20102014.csv')

# this is ,
determine_read_generic("1,2,3\n4,5,6")

determine_read_generic("a,b\n\n\n1,2")

determine_read_generic("a,b\n1,2\n\n\n2,3\n")

determine_read_generic("a,b\n1,2\n\n\n")

# Fails because the , is not repeated in over 90% of rows
determine_read_generic("v1,v2\n#foo\n1,2\n#bar\n3,4")

determine_read_generic("x1,x2,x3\nA2,B2,C2\nA3#,B2,C2\nA4,A5,A6")

determine_read_generic("http://www.ine.es/jaxiT3/files/t/es/csv/24282.csv?nocab=1")

determine_read_generic("https://datosabiertos.ayto-arganda.es/dataset/3fa98747-316d-4782-aab6-a7a42b8b00b8/resource/63c7c0ab-691c-4c75-929d-ae521bd67926/download/precipitacion.csv")


determine_read_generic("https://data.cityofnewyork.us/api/views/kku6-nxdu/rows.csv?accessType=DOWNLOAD")

determine_read_generic("https://chronicdata.cdc.gov/views/g4ie-h725/rows.csv?accessType=DOWNLOAD")

