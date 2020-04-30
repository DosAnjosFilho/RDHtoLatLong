
<!-- README.md is generated from README.Rmd. Please edit that file -->
RDHtoLatLong
============

The goal of RDHtoLatLong is a simple way for converting "Rijksdriehoek" (RDH) coordinates to lat and long (WGS84) in R.

Rijksdriehoek
-------------

Coordinates in the system of the National Triangulation Survey or in short, National Triangular Coordinates (also: RD coordinates) are the coordinates in the geodetic coordinate system that is used at the national level for the European Netherlands as a basis for geographical indications and files, such as in a geographical information system (GIS). and on maps of the Land Registry and other authorities (such as: Basic map Large-scale Topography (BGT), cadastral map and topographical maps).

Installation
------------

You can install RDHtoLatLong from github with:

``` r
# install.packages("devtools")
devtools::install_github("dosanjosfilho/RDHtoLatLong")
```

Example
-------

Converting the coordinates of the city of Amsterdam in RDH to Lat Long

``` r
library(RDHtoLatLong)

x <- 121687
y <- 487484

RDHtoLatLong(x, y)
#> [1] 52.374216  4.898012
```

Reference
---------

This package was developed based on a [post](https://www.roelvanlisdonk.nl/2012/11/21/simple-way-for-converting-rijksdriehoek-coordinates-to-lat-and-long-wgs84-in-c/) published by Roel Van Lisdonk.
