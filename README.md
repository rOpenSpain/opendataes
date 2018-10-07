
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis build status](https://travis-ci.org/cimentadaj/opendataes.svg?branch=master)](https://travis-ci.org/cimentadaj/opendataes) [![Coverage status](https://codecov.io/gh/cimentadaj/opendataes/branch/master/graph/badge.svg)](https://codecov.io/github/cimentadaj/opendataes?branch=master)

opendataes
==========

The goal of `opendataes` is to interact and download data with the API of <https://datos.gob.es>

Installation
------------

You can install the development version of opendataes from [Github](https://github.com) with:

``` r
remotes::install_github("cimentadaj/opendataes")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
library(opendataes)

example_id <- 'l01080193-fecundidad-madres-de-15-a-19-anos-quinquenal-2003-2014'
res <- cargar_datos(example_id)
```

You can access the metadata by subsetting the `metadata` slot.

``` r
res$metadata
#> # A tibble: 3 x 8
#>   keywords language description url   date_issued         date_modified
#>   <chr>    <chr>    <chr>       <chr> <dttm>              <chr>        
#> 1 Barrios~ es       Fecundidad~ http~ 2017-07-13 22:00:00 No modificat~
#> 2 Barrios~ ca       Fecunditat~ http~ 2017-07-13 22:00:00 No modificat~
#> 3 Barrios~ en       Fertility ~ http~ 2017-07-13 22:00:00 No modificat~
#> # ... with 2 more variables: publisher <chr>, publisher_data_url <chr>
```

And the full data in the `data` slot.

``` r
res$data
#> $t02.csv
#> # A tibble: 75 x 11
#>    Dte.  Barris `2003-2007` `2004-2008` `2005-2009` `2006-2010` `2007-2011`
#>    <chr> <chr>        <int>       <int>       <int>       <int>       <int>
#>  1 BARC~ <NA>          1359        1381        1404        1363        1310
#>  2 1     1. el~          76          77          80          78          79
#>  3 1     "2. e~          19          16          13          10           9
#>  4 1     3. la~          15          14          17          18          15
#>  5 1     4. Sa~          24          27          23          24          25
#>  6 2     5. el~          15          11          15          12          10
#>  7 2     "6. l~          28          20          17          16          16
#>  8 2     7. la~          10          10           7           6           5
#>  9 2     8. l'~          13          17          16          14          14
#> 10 2     9. la~          19          21          21          18          18
#> # ... with 65 more rows, and 4 more variables: `2008-2012` <int>,
#> #   `2009-2013` <int>, `2010-2014` <int>, `2011-2015` <int>
```
