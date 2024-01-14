test_that("`getAOI` works with supported object classes", {
  r <- terra::rast(nrows = 50, ncols = 50,
                   xmin = -123.7835, xmax = -123.6352,
                   ymin = 41.7534, ymax = 41.8042,
                   crs = terra::crs("epsg:4326"),
                   vals = rnorm(2500))
  v <- terra::as.polygons(r)
  ext <- c(-123.7835, 41.7534, -123.6352, 41.8042)

  expect_equal(getAOI(r), ext)
  expect_equal(getAOI(v), ext)
  expect_equal(getAOI(sf::st_as_sf(v)), ext)
  expect_equal(getAOI(raster::raster(r)), ext)
  expect_equal(getAOI(stars::st_as_stars(r)), ext)

})


test_that("`getAOI()` recognizes arguement errors", {
  r <- terra::rast(nrows = 50, ncols = 50,
                   xmin = -123.7835, xmax = -123.6352,
                   ymin = 41.7534, ymax = 41.8042,
                   crs = terra::crs("epsg:4326"),
                   vals = rnorm(2500))

  # Check for required arguments
  expect_error(getAOI(extend = c(10,15)),
               'argument "data" is missing, with no default')

  expect_error(getAOI(data = r, extend = c(10,15), sf_order = 1),
               "argument `sf_order` must be logical")

  # Check class
  expect_error(getAOI(r, extend = "string"),
               "argument `extend` must be numeric vector")
  expect_error(getAOI(r, extend = c(1,2,3)),
               "argument `extend` must be a numeric vector with a length of 1, 2, or 4 elements")
  expect_error(getAOI(r, sf_order = 1),
               "argument `sf_order` must be logical")


})

test_that("`getAOI` returns correct extended AOI", {
  r <- terra::rast(nrows = 50, ncols = 50,
                   xmin = -123.7835, xmax = -123.6352,
                   ymin = 41.7534, ymax = 41.8042,
                   crs = terra::crs("epsg:4326"),
                   vals = rnorm(2500))

  ext <- c(-123.7835, 41.7534, -123.6352, 41.8042)

  expect_equal(getAOI(r, extend = c(10)), ext + c(-10, -10, 10, 10))
  expect_equal(getAOI(r, extend = c(10, 15)), ext + c(-10, -15, 10, 15))
  expect_equal(getAOI(r, extend = c(10, 15, 2, 4)), ext + c(-10, -2, 15, 4))
  expect_equal(getAOI(r, extend = c(10, 15, 2, 4), sf_order = TRUE), ext + c(-10, -15, 2, 4))
})


test_that("`getAOI` returns correct CRS", {
  r <- terra::rast(nrows = 50, ncols = 50,
                   xmin = -2261174.94, xmax = -2247816.36,
                   ymin = 2412704.65, ymax = 2421673.98,
                   crs = terra::crs("epsg:5070"),
                   vals = rnorm(2500))

  ext <- c(-123.80251, 41.72353, -123.61607, 41.83432)

  # Expect minor differences from transformation
  expect_equal(getAOI(r), ext, tolerance = 0.001)
})


# Tests for `getZone`

test_that("`getZone` works with supported object classes", {
  v <- sf::st_bbox(sf::st_as_sf(data.frame(x = c(-123.7835,-123.6352),
                                           y = c(41.7534,41.8042)),
                                coords = c("x", "y"),
                                crs = 4326)) |>
    sf::st_as_sfc()

  p <- sf::st_as_sf(data.frame(x = c(-123.7835,-123.6352),
                                           y = c(41.7534,41.8042)),
                                coords = c("x", "y"),
                                crs = 4326)

  zone <- 3

  # polygon
  expect_equal(getZone(v), zone)
  # points
  expect_equal(getZone(p), zone)
  # character string
  expect_equal(getZone("Northern California Coastal Range"), zone)
})


test_that("`getZone` recognizes arguement errors", {
  v <- sf::st_bbox(sf::st_as_sf(data.frame(x = c(-6.2,-6),
                                           y = c(41.7534,41.8042)),
                                coords = c("x", "y"),
                                crs = 4326)) |>
    sf::st_as_sfc()

  # v_mult <- sf::st_bbox(sf::st_as_sf(data.frame(x = c(-114.77,-114.57),
  #                                          y = c(33.26,33.37)),
  #                               coords = c("x", "y"),
  #                               crs = 4326)) |>
  #   sf::st_as_sfc()

  expect_error(getZone(v),
               "argument `data` must be sf object or zone name within CONUS")

  expect_error(getZone("Not a Zone"),
               "argument `data` must be sf object or zone name within CONUS")

  expect_error(getZone(terra::vect(v)),
               "argument `data` must be sf object, or character string")
})
