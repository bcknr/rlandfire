
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/rlandfire.png" align="left" width="125" height="125"/>

<br><br>

## rlandfire: Interface to ‘LANDFIRE Product Service’ API

<!-- badges: start -->

![](https://img.shields.io/github/r-package/v/bcknr/rlandfire)
[![R-CMD-check](https://github.com/bcknr/rlandfire/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bcknr/rlandfire/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`rlandfire` provides access to a diverse suite of spatial data layers
via the LANDFIRE Product Services
([LFPS](https://lfps.usgs.gov/arcgis/rest/services/LandfireProductService/GPServer))
API. [LANDFIRE](https://landfire.gov) is a joint program of the USFS,
DOI, and other major partners, which provides data layers for wildfire
management, fuel modeling, ecology, natural resource management,
climate, conservation, etc. The complete list of available layers and
additional resources can be found on the LANDFIRE webpage.

## Installation

Install `rlandfire` from CRAN:

``` r
install.packages("rlandfire")
```

The development version of `rlandfire` can be installed from GitHub
with:

``` r
# install.packages("devtools")
devtools::install_github("bcknr/rlandfire")
```

Set `build_vignettes = TRUE` to access this vignette in R:

``` r
devtools::install_github("bcknr/rlandfire", build_vignettes = TRUE)
```

This package being maintained in my free time and users may encounter
bugs or unexpected behavior. Please report any issues, feature requests,
or suggestions in the package’s [GitHub
repo](https://github.com/bcknr/rlandfire/issues).

## Using `rlandfire`

To demonstrate `rlandfire`, we will explore how ponderosa pine forest
canopy cover changed after the 2020 Calwood fire near Boulder, Colorado.

``` r
library(rlandfire)
library(sf)
library(terra)
library(foreign)
```

First, we will load the Calwood Fire perimeter data which was downloaded
from Boulder County’s [geospatial data
hub](https://opendata-bouldercounty.hub.arcgis.com/).

``` r
boundary_file <- file.path(tempdir(), "wildfire")
utils::unzip(system.file("extdata/wildfire.zip", package = "rlandfire"),
             exdir = tempdir())

boundary <- st_read(file.path(boundary_file, "wildfire.shp")) |>
  sf::st_transform(crs = st_crs(32613))
#> Reading layer `wildfire' from data source `/tmp/RtmpHghXRj/wildfire/wildfire.shp' using driver `ESRI Shapefile'
#> Simple feature collection with 1 feature and 7 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -105.3901 ymin: 40.12149 xmax: -105.2471 ymax: 40.18701
#> Geodetic CRS:  WGS 84

plot(boundary$geometry, main = "Calwood Fire Boundary (2020)", 
     border = "red", lwd = 1.5)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" alt="" width="100%" style="display: block; margin: auto;" />

### AOI

We can use the function `rlandfire::getAOI()` to create an area of
interest (AOI) vector with the correct format for `landfireAPIv2()`.
`getAOI()` handles several steps for us, it ensures that the AOI is
returned in the correct order (`xmin`, `ymin`, `xmax`, `ymax`) and
converts the AOI to latitude and longitude coordinates (as required by
the API) if needed.

Using the `extend` argument, we will increase the AOI by 1 km in all
directions to provide additional context surrounding the burned area.
This argument takes an optional numeric vector of 1, 2, or 4 elements.

``` r
aoi <- getAOI(boundary, extend = 1000)
aoi
#> [1] -105.40207   40.11224 -105.23526   40.19613
```

Alternatively, you can supply a LANDFIRE map zone number in place of the
AOI vector. The function `getZone()` returns the zone number containing
an `sf` object or which corresponds to the supplied zone name. See
`help("getZone")` for more information and an example.

### Products

For this example, we are interested in canopy cover data for two years,
2016 (`LF2016_CC`) and 2022 (`LF2022_CC`), and existing vegetation type
(`LF2016_EVT`). All available data products, and their abbreviated
names, can be found in the [products
table](https://lfps.usgs.gov/products) which can be opened by calling
`viewProducts()`.

``` r
products <- c("LF2016_CC", "LF2022_CC", "LF2016_EVT")
```

### Email

As part of the LANDFIRE program’s update to LFPSv2 an email is now
required with each request. See the [LFPS
Guide](https://lfps.usgs.gov/LFProductsServiceUserGuide.pdf) for more
information.

``` r
email <- "rlandfire@markabuckner.com"
```

### Projection and resolution

We can ask the API to project the data to the same CRS as our fire
perimeter data by providing the `WKID` for our CRS of interest and a
resolution of our choosing, in meters.

``` r
projection <- 32613 # WGS 84 / UTM zone 13N
resolution <- 90
```

### Edit rule

We will use the `edit_rule` argument to filter out canopy cover data
that does not correspond to Ponderosa Pine Woodland. The `edit_rule`
statement should tell the API that when existing vegetation cover is
anything other than Ponderosa Pine Woodland (`7054`), the value of the
canopy cover layers should be set to (`st`) a specified value.

To do so, we specify that when `LF2016_EVT` is not equal (`ne`) to
`7054`, the “condition,” the canopy cover layers should be set equal
(`st`) to `1`, the “change.” The edit rule syntax is explained in more
depth in the [LFPS
guide](https://lfps.usgs.gov/LFProductsServiceUserGuide.pdf).

*(How the API applies edit rules can be unintuitive. For example, if we
used ‘clear value’ \[`cv`\] or set the value outside of 0-100 the edits
we want would not work. To work around this behavior, we set the values
to `1` since it is not found in the original data set.*)

``` r
edit_rule <- list(
  c("condition", "LF2016_EVT", "ne", 7054),
  c("change", "LF2016_CC", "st", 1),
  c("change", "LF2022_CC", "st", 1)
)
```

Note: Edits are performed in the order that they are listed and are
limited to fuel theme products (i.e., Fire Behavior Fuel Model 13, Fire
Behavior Fuel Model 40, Forest Canopy Base Height, Forest Canopy Bulk
Density, Forest Canopy Cover, and Forest Canopy Height). The more
advanced functionality of the LFPS edit rules are available and
implemented in this package, but not covered here.

If we wanted to restrict these edits to a certain area we could pass the
path to a zip archive (`.zip`) containing a shapefile to `edit_mask`:

``` r
edit_mask <- "path/to/wildfire.zip"
```

If an `edit_mask` is provided `landfireAPIv2` will automatically post
the raster to the LFPS and integrate it into your call.

(*Note: The file must follow ESRI shapefile naming standards (e.g., no
special characters) and be less than 1MB in size.*)

### Path

Finally, we will provide a path to a temporary zip file. Setting the
path as a temp file is not strictly necessary because if `path` is left
blank `landfireAPIv2()` will save the data to a temporary folder by
default. The path can be set to any writable directory.

``` r
path <- tempfile(fileext = ".zip")
```

### Call `landfireAPIv2()`

Now we are able to submit a request to the LANDFIRE Product Services API
with the `landfireAPIv2()` function.

``` r
resp <- landfireAPIv2(products = products,
                      aoi = aoi,
                      email = email,
                      projection = projection,
                      resolution = resolution,
                      edit_rule = edit_rule,
                      path = path,
                      verbose = TRUE)
```

`landfireAPIv2()` will download your requested data into the folder
provided in the path argument. If you did not provide one, you can find
the path to your data in the `$path` element of the `landfire_api`
object.

``` r
resp$path
```

### Load and process LF data

The files returned by the LFPS API are compressed `.zip` files
containing a single multiband `GeoTIFF` and the metadata files. We have
two options for programmatically accessing the files:

1.  Manually unzip the directory before reading the `.tif` file using
    `terra`, or

``` r
lf_dir <- file.path(tempdir(), "lf")
utils::unzip(path, exdir = lf_dir)

lf <- terra::rast(list.files(lf_dir, pattern = ".tif$", 
                             full.names = TRUE, 
                             recursive = TRUE))
```

2.  Read the `GeoTIFF` file in directly using GDAL’s Virtual File system
    in a single line.

``` r
lf  <- landfireVSI(resp)
lf
#> class       : SpatRaster 
#> size        : 108, 163, 3  (nrow, ncol, nlyr)
#> resolution  : 90, 90  (x, y)
#> extent      : 465554.1, 480224.1, 4440121, 4449841  (xmin, xmax, ymin, ymax)
#> coord. ref. : WGS 84 / UTM zone 13N (EPSG:32613) 
#> source      : j97f36f30e1f740f584847bde495d940d.tif 
#> names       : LF2016_CC_CONUS, LF2016_EVT_CONUS, LF2022_CC_CONUS 
#> min values  :               0,             7011,               0 
#> max values  :              55,             9829,              65
```

Note: `landfireVSI()` can also be used to read data directly from the
LFPS server into R when using `method = "none"` in `landfireAPIv2()`.

Now we can reclassify the canopy cover layers to remove any values which
are not classified as Ponderosa Pine, calculate the change, and plot our
results.

``` r
lf$LF2016_CC_CONUS[lf$LF2016_CC_CONUS == 1] <- NA
lf$LF2022_CC_CONUS[lf$LF2022_CC_CONUS == 1] <- NA

change <- lf$LF2022_CC_CONUS - lf$LF2016_CC_CONUS

plot(change, col = rev(terrain.colors(250)),
     main = "Canopy Cover Loss - Calwood Fire (2020)",
     xlab = "Easting",
     ylab = "Northing")
plot(boundary$geometry, add = TRUE, col = NA,
     border = "black", lwd = 2)
```

<img src="man/figures/README-unnamed-chunk-17-1.png" alt="" width="100%" style="display: block; margin: auto;" />

### Working with Categorical Products

The LFPS REST API now embeds attributes in the GeoTIFF files for some
variables and returns a database file (`.dbf`) containing the full
attribute table.

To demonstrate, we will download the Existing Vegetation Cover product
from LF 2.4.0 (`LF2023_EVC`). Unlike in the example above we will submit
a minimal request with the default projection and resolution. We will
also allow `rlandfire` to save the files to a temporary directory
automatically. As mentioned above, we can find the path to the temporary
directory in the `$path` element of the `landfire_api` object returned
by `landfireAPIv2()`.

``` r
resp <- landfireAPIv2(products = "LF2023_EVC",
                    aoi = aoi,
                    email = email,
                    verbose = FALSE)
```

When we read in and plot the EVC layer the legend will now list the
`classnames` for each vegetation type.

``` r
evc <- landfireVSI(resp)
plot(evc)
```

<img src="man/figures/README-unnamed-chunk-20-1.png" alt="" width="100%" />

To access the values each `classname` is assigned to we can uses the
`levels()` function. This returns a simple two column data frame
containing both the index and active category, in our case the
vegetation cover classes.

``` r
head(levels(evc)[[1]])
#>   Value                        CLASSNAMES
#> 1    11                        Open Water
#> 2    13 Developed-Upland Deciduous Forest
#> 3    14 Developed-Upland Evergreen Forest
#> 4    15     Developed-Upland Mixed Forest
#> 5    16       Developed-Upland Herbaceous
#> 6    17        Developed-Upland Shrubland
```

Alternatively, we can access the full attribute table using two methods.
We can use the function `cats()` which works similarly to `levels()` but
returns the full attribute table as a data frame. Alternatively, we can
read the database file using `foreign::read.dbf()` which requires us to
unzip the directory first. Both methods return similar results, although
in this case, we see that the `.dbf` file includes an additional `Count`
column not included in the data frame returned from `cats()`.

``` r
# cats
attr_tbl <- cats(evc)

# Find path to database file
lf_cat <- file.path(tempdir(), "lf_cat")
utils::unzip(resp$path, exdir = lf_cat)

dbf <- list.files(lf_cat, pattern = ".dbf$",
                  full.names = TRUE,
                  recursive = TRUE)

# Read file
dbf_tbl  <- foreign::read.dbf(dbf)

head(attr_tbl[[1]])
#>   Value                        CLASSNAMES Count   R   G   B RED GREEN BLUE
#> 1    11                        Open Water   228   0   0 255   0     0  255
#> 2    13 Developed-Upland Deciduous Forest   120  64  61 168  64    61  168
#> 3    14 Developed-Upland Evergreen Forest   332  68  79 137  68    79  137
#> 4    15     Developed-Upland Mixed Forest   193 102 119 205 102   119  205
#> 5    16       Developed-Upland Herbaceous   369 122 142 245 122   142  245
#> 6    17        Developed-Upland Shrubland   183 158 170 215 158   170  215
head(dbf_tbl)
#>   Value Count                        CLASSNAMES   R   G   B      RED    GREEN
#> 1    11   228                        Open Water   0   0 255 0.000000 0.000000
#> 2    13   120 Developed-Upland Deciduous Forest  64  61 168 0.250980 0.239216
#> 3    14   332 Developed-Upland Evergreen Forest  68  79 137 0.266667 0.309804
#> 4    15   193     Developed-Upland Mixed Forest 102 119 205 0.400000 0.466667
#> 5    16   369       Developed-Upland Herbaceous 122 142 245 0.478431 0.556863
#> 6    17   183        Developed-Upland Shrubland 158 170 215 0.619608 0.666667
#>       BLUE
#> 1 1.000000
#> 2 0.658824
#> 3 0.537255
#> 4 0.803922
#> 5 0.960784
#> 6 0.843137
```

### Citation

Visit the [LANDFIRE webpage](https://landfire.gov/data/citation) for
information on citing LANDFIRE data layers. The package citation
information can be viewed with `citation("rlandfire")`.
