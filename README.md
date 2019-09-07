
<!-- README.md is generated from README.Rmd. Please edit that file -->
flying
======

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/BMasinde/flight.svg?branch=master)](https://travis-ci.org/BMasinde/flight) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/BMasinde/flight?branch=master&svg=true)](https://ci.appveyor.com/project/BMasinde/flight) [![Coveralls test coverage](https://coveralls.io/repos/github/BMasinde/flight/badge.svg)](https://coveralls.io/r/BMasinde/flight?branch=master) <!-- badges: end -->

The package provides methods for predicting flight range of birds based on their physiological characteristics. This is an R implementation of Flight program provided by Pennycuick. Note, that this package is constantly under development, funcitons will break.

Installation
------------

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("BMasinde/flight")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
library(flying)
## basic example code

## birds comes with the package
data("birds")

birds_range <- flysim(data = birds, method = "breguet")
#> ## ctrl not defined. Using default constants.
#>             
#> Default air_dens = 1.00 kg m^3

birds_range$Range
#>                     name     Range
#> 1            Anser anser         0
#> 2   Hydrobates pelagicus         0
#> 3    Pachyptila desolata         0
#> 4        Regulus regulus         0
#> 5       Calidris canutus         0
#> 6      Aegypius monachus         0
#> 7       Limosa lapponica 11765.445
#> 8            Anas crecca         0
#> 9        Hirundo rustica  3997.965
#> 10         Cygnus cygnus  3379.585
#> 11          Sylvia borin  2873.567
#> 12     Luscinia luscinia  2360.887
#> 13       Corvus monedula  2515.598
#> 14         Anas penelope         0
#> 15   Fregata magnificens         0
#> 16      Larus ridibundus         0
#> 17      Diomedea exulans         0
#> 18   Phalacrocorax carbo         0
#> 19       Gyps rueppellii         0
#> 20   Torgos tracheliotus         0
#> 21         Ardeotis kori         0
#> 22      Sturnus vulgaris         0
#> 23     Fringilla coelebs  2941.452
#> 24      Carduelis spinus  3001.646
#> 25     Turdus philomelos  3103.075
#> 26 Calidris tenuirostris  5891.907
#> 27     Buteo swainsoni M         0
#> 28     Buteo swainsoni F         0
```

The function *flysim* also outputs: constants used, fuel, Minimum power speed *Vmp*, Maximum range speed *Vmr* (still needs looking into), and lastly the data).

The data
--------

Note order of columns and definition in *birds*. Data passed to the function *flysim* agree with this order.

``` r
?birds
#> No documentation for 'birds' in specified packages and libraries:
#> you could try '??birds'
```
