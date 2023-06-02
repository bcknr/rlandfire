#' Create extent vector for `landfireAPI()`
#'
#' @description
#' `getAOI` creates and extent vector in WGS84 from spatial data
#'
#' @param data A SpatRaster, SpatVector, sf, stars, or RasterLayer (raster) object
#' @param extend Optional. A numeric vector of 1, 2, or 4 elements passed to
#'   [terra::extend()] by which to increase the extent by.
#'
#' @return Returns an extent vector ordered `xmin`, `ymin`, `xmax`, `ymax`
#'    with a lat/lon projection.
#'
#' @md
#' @export
#'
#' @examples
#' r <- terra::rast(nrows = 50, ncols = 50, xmin = -30, xmax = 10, ymin = -30,
#'   ymax = 10, crs = terra::crs("epsg:4326"), vals = rnorm(2500))
#' ext <- getAOI(r, c(10, 15))
#'
getAOI <- function(data, extend = NULL) {
  # Check object class and extract extent as SpatExtent
  if(inherits(data, c("SpatRaster", "SpatVector", "sf", "RasterLayer", "stars"))) {
    ext <- terra::ext(sf::st_bbox(data)) # not the nicest but makes changing extent easier
  } else {
    stop("`data` must be SpatRaster, SpatVector, sf, stars, or RasterLayer (raster) object")
  }

  # if extend !null extend the SpatExtent Object
  if(!is.null(extend)){
    ext <- terra::extend(ext, extend)
  }

  # Check object projection and project to lat/lon if need
  if(sf::st_crs(data) != sf::st_crs(4326)) {
    ext <- terra::project(ext, sf::st_crs(data)$wkt, sf::st_crs(4326)$wkt)
  }
  as.vector(c(ext$xmin, ext$ymin, ext$xmax, ext$ymax))
}

