
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rlandfire

<!-- badges: start -->
<!-- badges: end -->

This project is an in-development R package for working with LANDFIRE
data.

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

### Example

``` r
library(rlandfire)

# LFPS Parameters
products <-  c("ASP2020", "ELEV2020", "SLPP2020")
aoi <- c("-123.7835", "41.7534", "-123.6352", "41.8042")
projection <- 6414
resolution <- 90

# R specific arguments
save_file <- tempfile(fileext = ".zip")

# call API
ncal <- landfireAPI(products, aoi, projection, resolution, path = save_file)
```

## License

![](https://img.shields.io/github/license/bcknr/rlandfire)

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
