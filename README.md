
<!-- README.md is generated from README.Rmd. Please edit that file -->

# opendataes

[![Travis build
status](https://travis-ci.org/rOpenSpain/opendataes.svg?branch=master)](https://travis-ci.org/rOpenSpain/opendataes)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/rOpenSpain/opendataes?branch=master&svg=true)](https://ci.appveyor.com/project/cimentadaj/opendataes)
[![Coverage
status](https://codecov.io/gh/rOpenSpain/opendataes/branch/master/graph/badge.svg)](https://codecov.io/github/rOpenSpain/opendataes?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

## Description

The goal of `opendataes` is to interact and download data from the
<https://datos.gob.es> API.

This API is an effort from the Spanish government to unify all data
sources from different provinces and regions into a single API. The API
includes data from entities such as universities, small and big city
halls, autonomous communities and Spain as a whole. With over 19,000
datasets in topics all the way from the number of parking spaces in a
given city to the levels of air pollution at the regional level (at the
moment of writing, October 2018), the API keeps growing with a rich set
of public information that researchers and data analysts can use for
their own research.

Because aggregating data from all these different sources poses a
challenge for reading different formats and harmonizing different
datasets, `opendataes` is very conservative in what it can read. Once
you install the package, you can print the contents of
`permitted_formats` and `available_publishers` to explore which formats
and publishers are available.

## Collaboration

This package is meant to be developed by the R community and it is
completely open to new pull requests, ideas and collaborations. The idea
of the package is to include as many formats and publishers as possible
by bringing the support and knowledge of other developers If you’re
interested in collaborating, please check the vignettes as the package
is described in detail there and file a pull request.

## Installation

`opendataes` is currently being tested and is not available on CRAN. You
can install the development version from [Github](https://github.com)
with:

``` r
remotes::install_github("ropenspain/opendataes")
```

## Example

The package has one main function that allows to read data from the API:
`cargar_datos`. However, that function can be used in two different
ways.

### Web-based search

You can search for a dataset
[here](http://datos.gob.es/es/catalogo?_publisher_display_name_limit=0)
and click on it’s names to go to its homepage. You’ll have something
like this:

<img src="man/figures/datos_web.png" align="center" />

<br/> <br/>

`cargar_datos` needs only the end path of a given dataset to read the
data. So, we copy this:

<br/> <br/>

<img src="man/figures/datos_url.png" align="center"/>

<br/> <br/>

and pass it on to `cargar_datos`, and that is it.

``` r
library(opendataes)

id <- 'l01080193-elecciones-al-parlamento-europeo-sobre-electores'
elections <- cargar_datos(id)
```

### R-based search

Alternatively, `opendataes` allows to seach datasets by keywords. For
example, to read the same data as before we could search for a keyword
and specifying the publisher of that dataset. We know that the data
belongs to the ‘Ayuntamiento of Barcelona’ but `explorar_keywords`
requires that publisher’s code.

First we extract the code using the `publishers_available` data frame
from
`opendataes`:

``` r
pb_code <- publishers_available$publisher_code[publishers_available$publishers == 'Ayuntamiento de Barcelona']
```

(you can check out the available publishers with `publishers_available`,
which should increase as the package evolves)

And then we search for a given key keyword

``` r
kw <- explorar_keywords('elecciones', pb_code)
kw
#> # A tibble: 12 x 5
#>    description         publisher   is_readable path_id       url          
#>    <chr>               <chr>       <lgl>       <chr>         <chr>        
#>  1 Elecciones al Parl~ Ayuntamien~ TRUE        l01080193-el~ http://datos~
#>  2 Elecciones al Parl~ Ayuntamien~ TRUE        l01080193-el~ http://datos~
#>  3 Elecciones al Parl~ Ayuntamien~ TRUE        l01080193-el~ http://datos~
#>  4 Elecciones al Parl~ Ayuntamien~ TRUE        l01080193-el~ http://datos~
#>  5 Elecciones General~ Ayuntamien~ TRUE        l01080193-el~ http://datos~
#>  6 Elecciones General~ Ayuntamien~ TRUE        l01080193-el~ http://datos~
#>  7 Elecciones al Parl~ Ayuntamien~ TRUE        l01080193-el~ http://datos~
#>  8 Elecciones Locales~ Ayuntamien~ TRUE        l01080193-el~ http://datos~
#>  9 Elecciones al Parl~ Ayuntamien~ TRUE        l01080193-el~ http://datos~
#> 10 Elecciones General~ Ayuntamien~ TRUE        l01080193-el~ http://datos~
#> 11 Elecciones Locales~ Ayuntamien~ TRUE        l01080193-el~ http://datos~
#> 12 Elecciones Locales~ Ayuntamien~ TRUE        l01080193-el~ http://datos~
```

Once we have that, `cargar_datos` only requires that we pass this exact
data frame but only with one row. We can do some data munging and subset
our data of
interest.

``` r
final_dt <- kw[grepl("Elecciones al Parlamento Europeo. % sobre electores", kw$description), ]

final_dt
#> # A tibble: 1 x 5
#>   description       publisher    is_readable path_id        url           
#>   <chr>             <chr>        <lgl>       <chr>          <chr>         
#> 1 Elecciones al Pa~ Ayuntamient~ TRUE        l01080193-ele~ http://datos.~
```

And we pass it to `cargar_datos`

``` r
elections <- cargar_datos(final_dt)
```

### Usage

Once we have the results, we get a print out of the relevant metadata of
the dataset.

``` r
elections
#> <datos.gob.es API>
#>    Description: Elecciones al Parlamento Europeo. % sobre electores
#>    Publisher: Ayuntamiento de Barcelona
#>    Languages: es, ca, en
#>    Date of release: 2013-03-13 23:00:00
#>    # of files read: 2 out of 2
```

But more importantly, we can subset the metadata of the dataset as well
as the data that was read.

``` r
elections$metadata
#> # A tibble: 3 x 8
#>   keywords language description url   date_issued         date_modified
#>   <chr>    <chr>    <chr>       <chr> <dttm>              <chr>        
#> 1 Año 200~ es       Elecciones~ http~ 2013-03-13 23:00:00 No modificat~
#> 2 Año 200~ ca       Eleccions ~ http~ 2013-03-13 23:00:00 No modificat~
#> 3 Año 200~ en       European P~ http~ 2013-03-13 23:00:00 No modificat~
#> # ... with 2 more variables: publisher <chr>, publisher_data_url <chr>
```

``` r
elections$data
#> $`2014_europees-a02.csv`
#> # A tibble: 74 x 14
#>      Dte Barris Participacio Abstencio ERCNECat CIU   ICVEUIA   PSC    PP
#>    <int> <chr>         <dbl>     <dbl>    <dbl> <chr>   <dbl> <dbl> <dbl>
#>  1    NA BARCE~          510       490      110 10,6       64    62    61
#>  2     1 1. el~          375       625       75 5,1        55    63    41
#>  3     1 "2. e~          417       583       94 7,1        68    50    36
#>  4     1 3. la~          367       633      104 5,4        42    57    33
#>  5     1 4. Sa~          431       569       85 6,6        77    55    41
#>  6     2 5. el~          557       443      132 12,9       72    52    59
#>  7     2 "6. l~          538       462      141 12,7       68    51    52
#>  8     2 7. la~          575       425      119 17,6       74    40    65
#>  9     2 8. l'~          551       449      117 16,4       65    42    61
#> 10     2 9. la~          555       445      130 14,6       66    48    60
#> # ... with 64 more rows, and 5 more variables: Cs <dbl>, PODEMOS <chr>,
#> #   Altres <dbl>, Blancs <chr>, Nuls <chr>
#> 
#> $`2009_europees-a02.csv`
#> # A tibble: 74 x 10
#>    Dte    Barris       Blancs Nuls    PSC CIU      PP ERC   ICVEUIA Altres
#>    <chr>  <chr>        <chr>  <chr> <dbl> <chr> <dbl> <chr> <chr>   <chr> 
#>  1 BARCE~ <NA>         1,2    0,3     131 8,7      83 3,4   3,2     2,4   
#>  2 1      1. el Raval  0,7    0,2     127 4,4      59 2,6   3,1     2,4   
#>  3 1      "2. el Barr~ 0,9    0,3     104 5,8      47 3,6   4,1     2,9   
#>  4 1      3. la Barce~ 0,5    0,2     129 4,8      48 2,6   2,3     1,7   
#>  5 1      4. Sant Per~ 0,9    0,5     118 6,4      56 3,2   4,4     2,8   
#>  6 2      5. el Fort ~ 1,3    0,3     122 11,0     84 4,3   3,8     2,7   
#>  7 2      "6. la Sagr~ 1,3    0,3     121 10,1     72 4,1   3,5     2,2   
#>  8 2      7. la Dreta~ 1,6    0,3      91 14,7     94 4,0   3,9     3,1   
#>  9 2      8. l'Antiga~ 1,4    0,3      97 13,3     89 3,6   3,5     2,7   
#> 10 2      9. la Nova ~ 1,3    0,2     110 11,5     86 4,2   3,5     2,4   
#> # ... with 64 more rows
```

For a deeper explanation of what the columns of the metadata mean and
some important caveats of what the package can and cannot do, please
read the package’s vignettes.
