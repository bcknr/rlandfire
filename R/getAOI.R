#' Create extent vector for `landfireAPI()`
#'
#' @description
#' `getAOI` creates an extent vector in WGS84 from spatial data
#'
#' @param data A SpatRaster, SpatVector, sf, stars, or RasterLayer (raster) object
#' @param extend Optional. A numeric vector of 1, 2, or 4 elements by which to
#'   increase the extent by.
#' @param sf_order If `extend` != NULL, logical indicating if the order of the
#'   vector follows the [sf::st_bbox()] order (`xmin`, `ymin`, `xmax`, `ymax`) or
#'   the [terra::extend()] ordering scheme (`xmin`, `xmax`, `ymin`, `ymax`). This
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

