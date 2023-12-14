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


test_that("`landfireAPI()` recognizes arguement errors", {
  r <- terra::rast(nrows = 50, ncols = 50,
                   xmin = -123.7835, xmax = -123.6352,
                   ymin = 41.7534, ymax = 41.8042,
                   crs = terra::crs("epsg:4326"),
                   vals = rnorm(2500))

  # Check for required arguments
  expect_error(landfireAPI(aoi = aoi),
               "argument `products` is missing with no default")

  expect_error(landfireAPI(products),
               "argument `aoi` is missing with no default")

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
