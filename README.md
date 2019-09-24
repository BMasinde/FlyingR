
<!-- README.md is generated from README.Rmd. Please edit that file -->
flying
======

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/BMasinde/flight.svg?branch=master)](https://travis-ci.org/BMasinde/flight) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/BMasinde/flight?branch=master&svg=true)](https://ci.appveyor.com/project/BMasinde/flight) [![Coveralls test coverage](https://coveralls.io/repos/github/BMasinde/flight/badge.svg)](https://coveralls.io/r/BMasinde/flight?branch=master) <!-- badges: end -->

The package provides methods for predicting flight range of birds based on their physiological characteristics. This is an R implementation of Flight program provided by Pennycuick.

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

birds_range <- flysim(data = birds)
#> >> ctrl not defined. Using default constants. <<

birds_range$range
#>  [1]  3275.7  3336.3  4299.5  1559.8  4349.4  4052.9 11496.7  3724.5
#>  [9]  3904.7  3380.5  2797.9  2364.3  2426.9  5567.1 10797.4  5997.5
#> [17]  5576.2  2993.6  6982.9  6497.1  4319.5  4177.7  2951.3  2933.6
#> [25]  3217.9  6208.7  5561.5  7174.2
```

``` r
## when estimating range of a single bird
Garden_Warbler <- list("name" = "Garden Warbler",
                       "body_mass" = 0.022,
                       "fat_mass" = 0.00660,
                       "wing_span" = 0.240,
                       "wing_area" = 0.0110,
                       "order" = as.factor(1)
)

G_warbler <- flysim(data = Garden_Warbler, ctrl = list(airDensity = 0.9093))

G_warbler$range
#> [1] 2873.6
```

The function *flysim* also outputs: constants used, fuel, Minimum power speed *Vmp*, Maximum range speed *Vmr* (still needs looking into), and lastly the data).

The data
--------

*birds* definitions pulled from Flight program in-built datasets and fat mass g randomly generated where initially zero. Users's data should have columns named appropriately. The package looks for columns named *id, name or species.name*, *bodymass or allupmass*, *wingspan, ws*, *wingarea*, *ordo, order* (which is a factored column with levles 1 or 2 passerines and non-passerines respectively. Lastly *fatmass, fat.mass, fat\_mass*.

``` r
birds
#>          Scientific.name Empty.mass Wing.span Fat.mass Order Wing.area
#> 1            Anser anser    3.77000     1.600  0.84641     2   0.33100
#> 2   Hydrobates pelagicus    0.02580     0.355  0.00591     2   0.01610
#> 3    Pachyptila desolata    0.15500     0.637  0.03886     2   0.04710
#> 4        Regulus regulus    0.00542     0.156  0.00112     1   0.00525
#> 5       Calidris canutus    0.12700     0.538  0.03500     2   0.03320
#> 6      Aegypius monachus    9.90000     3.040  2.02565     2   1.40000
#> 7       Limosa lapponica    0.36700     0.748  0.20112     2   0.05680
#> 8            Anas crecca    0.23500     0.582  0.06562     2   0.04580
#> 9        Hirundo rustica    0.01900     0.318  0.00570     1   0.01320
#> 10         Cygnus cygnus   12.50000     2.560  2.50000     2   0.75600
#> 11          Sylvia borin    0.02200     0.240  0.00660     1   0.01100
#> 12     Luscinia luscinia    0.02700     0.263  0.00675     1   0.01300
#> 13       Corvus monedula    0.18100     0.600  0.03620     1   0.06180
#> 14         Anas penelope    0.77000     0.822  0.28607     2   0.08290
#> 15   Fregata magnificens    1.67000     2.140  0.55799     2   0.37200
#> 16      Larus ridibundus    0.28500     0.967  0.07881     2   0.09920
#> 17      Diomedea exulans    9.57000     3.060  2.12836     2   0.64400
#> 18   Phalacrocorax carbo    2.56000     1.350  0.50768     2   0.22400
#> 19       Gyps rueppellii    7.30000     2.500  2.52588     2   0.89200
#> 20   Torgos tracheliotus    6.60000     2.640  2.01454     2   1.03000
#> 21         Ardeotis kori   11.90000     2.470  3.45889     2   1.06000
#> 22      Sturnus vulgaris    0.08190     0.384  0.02973     1   0.02530
#> 23     Fringilla coelebs    0.02300     0.264  0.00690     1   0.01310
#> 24      Carduelis spinus    0.01120     0.212  0.00336     1   0.00785
#> 25     Turdus philomelos    0.07160     0.361  0.02148     1   0.02250
#> 26 Calidris tenuirostris    0.23300     0.587  0.08970     2   0.03960
#> 27     Buteo swainsoni M    0.77500     1.250  0.22248     2   0.21000
#> 28     Buteo swainsoni F    1.06000     1.330  0.37117     2   0.24000
```
