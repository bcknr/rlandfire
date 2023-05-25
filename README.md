
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img align="left" width="125" height="125" src="./img/rlandfire.png">

## rlandfire: Tools for Accessing and Working with LANDFIRE in R

<!-- badges: start -->
<!-- badges: end -->

![](https://img.shields.io/github/r-package/v/bcknr/rlandfire)

## Installation

You can install the development version of rlandfire from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bcknr/rlandfire")
```

## `landfireAPI()`

The `landfireAPI()` function calls the LANDFIRE Product Services (LFPS)
API and downloads products with a user specified resolution and
projection. The image below shows the relationship between the
`landfireAPI()` arguments and the LFPS parameters.

![Comparison of LFPS and \`landfireAPI()\`](img/lfps.png)

``` r
library(rlandfire)

# LFPS Parameters
products <-  c("ASP2020", "ELEV2020", "140CC")
aoi <- c("-123.7835", "41.7534", "-123.6352", "41.8042")
projection <- 6414
resolution <- 90
edit_rule <- list(c("condition","ELEV2020","lt",500), 
                  c("change", "140CC", "st", 181))

# R specific arguments
save_file <- tempfile(fileext = ".zip")

# call API
ncal <- landfireAPI(products, aoi, projection, resolution, 
                    edit_rule = edit_rule, path = save_file)
```

## `getAOI()`

`getAOI` returns extent vector, ordered `xmin`, `ymin`, `xmax`, `ymax`
with a lat/lon (wgs84) projection from objects with class `SpatRaster`,
`SpatVector`, `sf`, `stars` or `RasterLayer`. Additionally, the the
extent can be extended by passing a numeric vector with length 1, 2, or
4 (see: `terra::extend()`).

``` r
library(terra)

# Create raster layer
r <- terra::rast(nrows = 50, ncols = 50, xmin = -30, xmax = 10, ymin = -30,
   ymax = 10, crs = terra::crs("epsg:4326"), vals = rnorm(2500))

# Extract and extend AOI
ext <- getAOI(r, c(10, 15))
```

## License

[![](https://img.shields.io/github/license/bcknr/rlandfire?color=green&logo=https%253A%252F%252Fwww.gnu.org%252Flicenses%252Fgpl-3.0.html)](https://www.gnu.org/licenses/gpl-3.0.html)

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
Public License for more details.

You should have received a copy of the GNU General Public License along
with this program. If not, see <https://www.gnu.org/licenses/>.
