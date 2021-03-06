% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract.R
\name{extract_data}
\alias{extract_data}
\title{Extract data from a data_list.}
\usage{
extract_data(data_list, encoding, guess_encoding, ...)
}
\arguments{
\item{data_list}{A data_list similar to resp$result$items[[1]] that contains information on a dataset}

\item{encoding}{The encoding passed to read (all) the files. Most cases should be resolved with either
'UTF-8', 'latin1' or 'ASCII'.}

\item{guess_encoding}{A logical stating whether to guess the encoding. This is set to TRUE by default.
Whenever guess_encoding is set to TRUE, the 'encoding' argument is ignored. If \code{\link[readr]{guess_encoding}}
fails to guess the encoding, openes_load falls back to the encoding argument.}

\item{...}{Arguments passed to \code{\link[readr]{read_csv}} and all other \code{read_*} functions.}
}
\description{
Extract data from a data_list.
}
\details{
\code{get_data} will accept the end path of a data base and it will search for the access url.
If the dataset is either a csv, xls, xlsx or xml, then it will
attempt to read it. If it succeeds, it will return the data frame. If not, it will return
the data frame with only one column containing all available access URL's.

For example, this URL: http://datos.gob.es/es/catalogo/a02002834-numero-de-centros-segun-ancho-de-banda-de-la-conexion-a-internet-que-tiene-el-centro6
says that it has a XML file but once you click on the 'download' XML, it redirects to a JavaScript based
website that has the table. This file unfortunately is unreadable to the package.
}
