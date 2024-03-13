#' Create extent vector for `landfireAPI()`
#'
#' @description
#' `getAOI` creates an extent vector in WGS84 from spatial data
#'
#' @param data A SpatRaster, SpatVector, sf, stars, or RasterLayer (raster) object
#' @param extend Optional. A numeric vector of 1, 2, or 4 elements to
#'   increase the extent by.
#' @param sf_order If `extend` != NULL, logical indicating that the order of the
#'   `extend` vector follows [sf::st_bbox()] (`xmin`, `ymin`, `xmax`, `ymax`) when TRUE or
#'   [terra::extend()] (`xmin`, `xmax`, `ymin`, `ymax`) when FALSE. This
#'   is `FALSE` by default to ensure backwards compatibility with previous versions.
#'
#' @return Returns an extent vector ordered `xmin`, `ymin`, `xmax`, `ymax`
#'    with a lat/lon projection.
#'
#' @md
#' @export
#'
#' @examples
#' r <- terra::rast(nrows = 50, ncols = 50,
#'   xmin = -123.7835, xmax = -123.6352,
#'   ymin = 41.7534, ymax = 41.8042,
#'   crs = terra::crs("epsg:4326"),
#'   vals = rnorm(2500))
#' ext <- getAOI(r, c(10, 15))
#'
getAOI <- function(data, extend = NULL, sf_order = FALSE) {
  # Check object class and extract extent as SpatExtent
  if(inherits(data, c("SpatRaster", "SpatVector", "sf", "RasterLayer", "stars"))) {
    ext <- sf::st_bbox(data)
  } else {
    stop("`data` must be SpatRaster, SpatVector, sf, stars, or RasterLayer (raster) object")
  }

  stopifnot("argument `sf_order` must be logical" = inherits(sf_order, c("logical")))

  if(!is.null(extend)) {
    stopifnot("argument `extend` must be numeric vector" = inherits(extend, c("numeric")))
    stopifnot("argument `extend` must be a numeric vector with a length of 1, 2, or 4 elements" = length(extend) %in% c(1,2,4))
  }

  # if extend !null extend the SpatExtent Object
  if(!is.null(extend)){

    # Change order, for back compatibility
    if(sf_order == FALSE & length(extend) == 4) {
      extend <- extend[c(1,3,2,4)]
    }

    if(length(extend) < 4) {
      extend <- rep_len(extend, 4)
    }
    # Extend
    ext <- ext + extend * c(-1,-1,1,1)
  }

  # Check object projection and project to lat/lon if need
  if(sf::st_crs(data) != sf::st_crs(4326)) {
    ext <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(ext), 4326))
  }
  as.vector(c(ext$xmin, ext$ymin, ext$xmax, ext$ymax))
}



#' Find LANDFIRE map zone for use with `landfireAPI()`
#'
#' @description
#' `getZone` returns the LANDFIRE Map Zone(s) a spatial object intersects or the
#' zone number from the zone name. Currently, only map zones within CONUS are
#' supported.
#'
#' @param data An sf object or character string with the map zone name.
#'
#' @return Returns a numeric vector containing the map zone(s)
#'
#' @md
#' @export
#'
#' @examples
#' \dontrun{
#'v <- sf::st_bbox(sf::st_as_sf(data.frame(x = c(-123.7835,-123.6352),
#'                                         y = c(41.7534,41.8042)),
#'                              coords = c("x", "y"),
#'                              crs = 4326)) |>
#'  sf::st_as_sfc()
#' zone <- getZone(v)
#'}
#'
getZone <- function(data) {

  if(inherits(data, c("sf", "sfc"))) {
    # Extract mz
    if(sf::st_crs(data) != sf::st_crs(mapzones)) {
      data <- sf::st_transform(data, sf::st_crs(mapzones))
    }

    mz <- sf::st_intersects(data, mapzones)

  } else if(inherits(data, "character")) {
    mz <- which(sf::st_drop_geometry(mapzones[,"ZONE_NAME"]) == data)

  } else {
    stop("argument `data` must be sf object, or character string")
  }

  mz <- unique(unlist(mz))

  stopifnot("argument `data` must be sf object or zone name within CONUS" = length(mz) != 0)
  if(length(mz) > 1) {
    warning("Spatial object intersects more than one map zone. `landfireAPI` only accepts one zone at a time!\nConsider using `getAOI()` instead")
  }

  mapzones$ZONE_NUM[mz]

}
